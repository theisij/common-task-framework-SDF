# ==============================================================================
# KNS Portfolio Optimization (Kozak, Nagel, Santosh)
# ==============================================================================
library(data.table)  # For setDT, melt, dcast, rbindlist
library(tidyverse)   # For select, filter, group_by, reframe, inner_join, if_else
library(future)      # For plan, multisession
library(furrr)       # For future_map, furrr_options
library(arrow)

prepare_data <- function(chars, features) {
  # Convert to data.table (infrastructure loads as tibble via arrow::read_parquet)
  chars <- setDT(chars)

  # Create 'me' column from 'market_equity' if it doesn't exist
  # (new data files only have market_equity, old files had both)
  if (!"me" %in% names(chars)) {
    chars[, me := market_equity]
  }

  # Debug: Check what we have
  cat("DEBUG: features length =", length(features), "\n")
  cat("DEBUG: features[1:5] =", paste(head(features, 5), collapse=", "), "\n")
  cat("DEBUG: chars columns =", paste(head(names(chars), 10), collapse=", "), "...\n")
  cat("DEBUG: chars has", ncol(chars), "columns\n")
  cat("DEBUG: Do all features exist in chars?", all(features %in% names(chars)), "\n")

  chars[, (features) := lapply(.SD, as.double), .SDcols = features]  # Convert feature columns to double to avoid loosing precision
  for(f in features) {
    if (match(f, features) %% 10 == 0) print(paste0("Feature ", match(f, features), " out of ", length(features)))
    chars[, zero := (get(f) == 0)]
    chars[!is.na(get(f)), (f) := ecdf(get(f))(get(f)), by = eom]
    chars[zero == T, (f) := 0][, zero := NULL]  # Set exact zeros to 0 (ecdf always returns >0)
  }
  # Feature Imputation
  chars[, (features) := lapply(.SD, function(x) if_else(is.na(x), 0.5, x)), .SDcols=features, by=eom]

  chars[, (features) := lapply(.SD, function(r) (r - mean(r)) / sum(abs(r - mean(r)))), .SDcols=features, by = eom]
  chars[, (features) := lapply(.SD, function(w) ifelse(is.na(w), 0, w)), .SDcols=features]
  chars[, me := me / sum(me), by = eom]
  chars
}

rank_weighted_factor_ws <- function(chars, features) {
  chars |>
    select(id, eom, eom_ret, all_of(features)) |>
    melt(
      id.vars = c("id", "eom", "eom_ret"),
      variable.name = "feature",
      value.name = "w"
    )
}

factor_daily_returns <- function(factor_ws, daily, workers = 3) {
  # Add eom (end-of-month) column to daily if it doesn't exist
  # daily_ret only has (id, date, ret_exc), but we need eom for joins
  if (!"eom" %in% names(daily)) {
    daily <- daily |>
      mutate(eom = ceiling_date(date, "month") - days(1))
  }

  # plan(multisession, workers = workers)
  # options(future.globals.maxSize = 50000*1024^2, future.rng.onMisuse = "ignore")

  sort(unique(factor_ws$eom_ret)) |>
    map(.progress = T, function(e) {
    # future_map(.progress = TRUE, function(e) {
       sub_d <- daily |> filter(eom == e)
       sub_ws <- factor_ws |> filter(eom_ret == e, id %in% sub_d$id)
       sub_d <- sub_d |> filter(id %in% sub_ws$id)

      factor_daily_ws <- inner_join(
        sub_ws, sub_d,
        by = c("eom_ret" = "eom", "id"),
        relationship = "many-to-many"
      )

      factor_daily_ws |>
        group_by(date, feature, eom_ret) |>
        reframe(
          ret = sum(w * ret_exc)
        ) |>
        setDT() |>
        dcast(date + eom_ret ~ feature, value.var = "ret")
    }) |>
    # }, .options = furrr_options(packages = c("data.table", "tidyverse"))) |>
    rbindlist(fill = TRUE) |>  # fill=TRUE handles dates with missing features
    setDT() |>
    arrange(date)
}

create_rw_factors <- function(chars, daily, features) {
  rw_factor_ws <- rank_weighted_factor_ws(chars, features)
  factor_daily_returns(rw_factor_ws, daily)
}

create_mkt_rets <- function(chars, daily) {
  # Add eom (end-of-month) column to daily if it doesn't exist
  # daily_ret only has (id, date, ret_exc), but we need eom for joins
  if (!"eom" %in% names(daily)) {
    daily <- daily |>
      mutate(eom = ceiling_date(date, "month") - days(1))
  }

  mkt_ret_daily <- daily |>
    mutate(
      eom_l = eom + 1 - months(1) - 1
    ) |>
    inner_join(chars[,c("id", "eom", "me")], by = c("eom_l" = "eom", "id" = "id")) |>
    group_by(date) |>
    summarize(
      ret_exc = sum(me * ret_exc, na.rm = TRUE) / sum(me),
      .groups = "drop"
    ) |>
    setDT()

  mkt_ret_daily
}

calc_opt_lambdas <- function(d, rw_factor_d_raw, mkt_ret_daily, features, n_folds, l2s, l1s = c(), use_pc=FALSE) {
  all_months <- sort(unique(rw_factor_d_raw$eom_ret))
  sub_months <- all_months[all_months < d & all_months >= d + 1 - months(120) - 1]

  cut_length <- floor(120 / n_folds)
  cut_idx <- c(0, (1:n_folds)  * cut_length)
  lambda_search <- 1:n_folds |>
    map(function(k) {
      val_months <- sub_months[cut_idx[k]:cut_idx[k+1]]
      train_months <- sub_months[! sub_months %in% val_months]

      val_k <- rw_factor_d_raw |> filter(eom_ret %in% val_months)
      train_k <- rw_factor_d_raw |> filter(eom_ret %in% train_months)
      mkt_train <- mkt_ret_daily |> filter(date %in% train_k$date)
      mkt_val <- mkt_ret_daily |> filter(date %in% val_k$date)

      beta <- cov(train_k |> select(all_of(features)), mkt_train$ret_exc) / var(mkt_train$ret_exc)

      X_train <- (train_k |> select(all_of(features))) - mkt_train$ret_exc %*% t(beta)
      X_val <- val_k[, ..features] - mkt_val$ret_exc %*% t(beta)

      fct_train <- cbind(X_train, mkt_train$ret_exc)
      fct_val <- cbind(X_val, mkt_val$ret_exc)

      mu_train <- fct_train |> colMeans()
      Sigma_train <- fct_train |> cov()

      mu_val <- fct_val |> colMeans()
      Sigma_val <- fct_val |> cov()

      if (use_pc) {
        decomp <- eigen(Sigma_train)
        P <- decomp$vectors
      }

      l2s |>
        map(function(l2) {
          if (length(l1s) != 0) {
            if (use_pc) {
              w_tilde <- diag(1/(decomp$values + l2)) %*% t(P) %*% mu_train
            }
            dt <- l1s |>
              map(function(l1) {
                if (use_pc) {
                  w_pc <- sign(w_tilde) * pmax(0, abs(w_tilde) - l1)
                  w <- P %*% w_pc
                } else {
                  print("Warning: L1 regularization without PC is computationally expensive.")
                  model <- glmnet(
                    Sigma_train + diag(l2, ncol(Sigma_train)), mu_train,
                    alpha = 1, lambda = l1,
                    intercept = FALSE,
                    standardize = FALSE
                  )
                  w <- model$beta
                }
                data.table(
                  fold = k,
                  l1 = l1,
                  l2 = l2,
                  r2 = 1 - sum((mu_val - Sigma_val %*% w)^2) / sum(mu_val^2)
                )
              }) |>
              rbindlist()
          } else {
            w <- solve(Sigma_train + diag(l2, ncol(Sigma_train))) %*% mu_train
            dt <- data.table(
              fold = k,
              l1 = NaN,
              l2 = l2,
              r2 = 1 - sum((mu_val - Sigma_val %*% w)^2) / sum(mu_val^2)
            )
          }
          dt
        }) |>
        rbindlist()
    }) |>
    rbindlist() |>
    setDT() |>
    summarize(
      r2 = mean(r2),
      .by = c(l1, l2)
    ) |>
    setDT()

  data.table(
    eom_ret = d,
    l1 = lambda_search[r2 == max(r2)][order(-l1, -l2)][1]$l1,
    l2 = lambda_search[r2 == max(r2)][order(-l1, -l2)][1]$l2,
    r2 = lambda_search[r2 == max(r2)][order(-l1, -l2)][1]$r2
  )
}

kns <- function(d, rw_factor_d_raw, mkt_ret_daily, features, opt_lambdas, use_pc) {
    l1 <- opt_lambdas[eom_ret == d, l1]
    l2 <- opt_lambdas[eom_ret == d, l2]
    hist <- rw_factor_d_raw |> filter(eom_ret < d, eom_ret >= d + 1 - months(120) - 1)
    mkt_hist <- mkt_ret_daily |> filter(date %in% hist$date)

    beta <- cov(hist[, ..features], mkt_hist$ret_exc) / var(mkt_hist$ret_exc)
    X_hist <- hist[, ..features] - mkt_hist$ret_exc %*% t(beta)

    fct_hist <- cbind(X_hist, mkt_hist$ret_exc)

    mu_hist <- fct_hist |> colMeans()
    Sigma_hist <- fct_hist |> cov()

    if (use_pc) {
      decomp <- eigen(Sigma_hist)
      P <- decomp$vectors
      w_tilde <- diag(1/(decomp$values + l2)) %*% t(P) %*% mu_hist
      w_pc <- sign(w_tilde) * pmax(0, abs(w_tilde) - l1)
      w <- P %*% w_pc
      names(w) <- c(features, "me")
    } else {
      inv <- solve(Sigma_hist + diag(l2, ncol(Sigma_hist)))
      w <- inv %*% mu_hist
    }

    dt <- data.table(
      eom_ret = d,
      t(w[-length(w)]),
      me = sum(t(w) * c(-beta, 1))
    )
    names(dt) <- c("eom_ret", features, "me")
    dt
}


main <- function(chars, features, daily_ret) {
  # Parse features if it's a dataframe
  if (is.data.frame(features)) {
    features_raw <- features$features
    # Parse if it's a string representation (format: 'c("feat1", "feat2", ...)')
    if (length(features_raw) == 1 && grepl("^c\\(", features_raw)) {
      # Extract content between c( and )
      content <- sub("^c\\((.*)\\)$", "\\1", features_raw)
      # Split by comma and trim whitespace
      items <- strsplit(content, ",\\s*")[[1]]
      # Remove surrounding quotes (both " and ')
      features <- gsub("^[\"']|[\"']$", "", items)
    } else {
      features <- features_raw
    }
  }

  cat("Parsed features in main():", length(features), "features\n")

  chars_normalized <- prepare_data(chars, features)
  write_parquet(chars_normalized, "data/interim/chars_normalized.parquet")
  # chars_normalized <- read_parquet("data/interim/chars_normalized.parquet")
  print("Creating RW Factors")
  rw_daily <- create_rw_factors(chars_normalized, daily_ret, features)
  write_parquet(rw_daily, "data/interim/rw_factors_daily.parquet")
  # rw_daily <- read_parquet("data/interim/rw_factors_daily.parquet")
  mkt_rets_daily <- create_mkt_rets(chars_normalized, daily_ret)

  pf_dates <- unique(subset(chars_normalized, ctff_test == TRUE)$eom_ret)

  l2s <- 10^seq(-10, 0, 0.1)
  l1s <- 10^seq(-10, 4, 0.1)
  n_folds <- 5

  print("Optimal Lambdas")
  plan(multisession, workers = 8)
  options(future.globals.maxSize = 5000*1024^2, future.rng.onMisuse = "ignore")
  opt_lambdas <- pf_dates |>
    # map(.progress = TRUE, function(d) {
    future_map(.progress = TRUE, function(d) {
      calc_opt_lambdas(d, rw_daily, mkt_rets_daily, features, n_folds, l2s, l1s, use_pc=TRUE)
    # }) |>
    }, .options = furrr_options(packages = c("data.table", "tidyverse"))) |>
    rbindlist() |>
    setDT()
  write_parquet(opt_lambdas, "data/interim/opt_lambdas_pc.parquet")

  print("Optimal PF")
  w_kns <- pf_dates |>
    # map(.progress = TRUE, function(d) {
    future_map(.progress = TRUE, function(d) {
      kns(d, rw_daily, mkt_rets_daily, features, opt_lambdas, use_pc=TRUE)
    # }) |>
    }, .options = furrr_options(packages = c("data.table", "tidyverse"))) |>
    rbindlist() |>
    setDT()
  write_parquet(w_kns, "data/interim/w_kns_pc.parquet")
  
  # w_kns_pc <- read_parquet("data/interim/w_kns_pc.parquet")

  test_chars <- chars_normalized |>
    filter(ctff_test == TRUE) |>
    select(id, eom, eom_ret, me, all_of(features))

  pf <- unique(test_chars$eom_ret) |>
    map(.progress = TRUE, function(d) {
      test_chars_d <- test_chars |> filter(eom_ret == d)
      w_kns_d <- w_kns |> filter(eom_ret == d)

      for (f in c(features, "me")) {
        test_chars_d[[f]] <- test_chars_d[[f]] * w_kns_d[[f]]
      }
      test_chars_d
    }) |>
    rbindlist() |>
    setDT()

  pf$w <- pf |> select(all_of(features), "me") |> rowSums()

  # Output
  pf[, .(id, eom, w)]
}


if (FALSE) {
  features <- read_parquet("data/raw/ctff_features.parquet") |> pull(features)
  chars <- read_parquet("data/raw/ctff_chars.parquet")
  daily_ret <- read_parquet("data/raw/ctff_daily_ret.parquet")
  
  pf <- chars |> main(daily_ret=daily_ret, features=features)
  pf |> fwrite("data/processed/kns_pc.csv")
}