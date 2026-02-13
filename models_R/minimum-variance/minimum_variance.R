# minimum_variance.R — Minimum-variance portfolio using Barra USE4S factor risk model
# Consolidated from models_R/minimum-variance/old_code/ (4-file pipeline)

# Section 1: Libraries ---------------------------------------------------------
library(arrow)
library(data.table)
library(tidyverse)
library(glmnet)

# Section 2: Data Preparation -------------------------------------------------
prepare_pred_data <- function(data, features, feat_prank, impute) {
  if (feat_prank) {
    data[, (features) := lapply(.SD, as.double), .SDcols = features]
    cat(sprintf("Percentile-ranking %d features...\n", length(features)))
    data[, (features) := lapply(.SD, function(x) {
      non_na <- !is.na(x)
      is_zero <- non_na & (x == 0)
      x[non_na] <- frank(x[non_na], ties.method = "max") / sum(non_na)
      x[is_zero] <- 0
      x - 0.5
    }), .SDcols = features, by = .(excntry, eom)]
  }
  if (impute) {
    if (feat_prank) {
      setnafill(data, fill = 0, cols = features)
    } else {
      data[, (features) := lapply(.SD, function(x) {
        fifelse(is.na(x), median(x, na.rm = TRUE), x)
      }), .SDcols = features, by = .(excntry, eom)]
    }
  }
  return(data)
}

ff12_class <- function(sic) {
  case_when(
    sic %in% c(100:999, 2000:2399, 2700:2749, 2770:2799, 3100:3199, 3940:3989) ~ "NoDur",
    sic %in% c(2500:2519, 3630:3659, 3710:3711, 3714, 3716, 3750:3751, 3792, 3900:3939, 3990:3999) ~ "Durbl",
    sic %in% c(2520:2589, 2600:2699, 2750:2769, 3000:3099, 3200:3569, 3580:3629,
               3700:3709, 3712:3713, 3715, 3717:3749, 3752:3791, 3793:3799, 3830:3839, 3860:3899) ~ "Manuf",
    sic %in% c(1200:1399, 2900:2999) ~ "Enrgy",
    sic %in% c(2800:2829, 2840:2899) ~ "Chems",
    sic %in% c(3570:3579, 3660:3692, 3694:3699, 3810:3829, 7370:7379) ~ "BusEq",
    sic %in% c(4800:4899) ~ "Telcm",
    sic %in% c(4900:4949) ~ "Utils",
    sic %in% c(5000:5999, 7200:7299, 7600:7699) ~ "Shops",
    sic %in% c(2830:2839, 3693, 3840:3859, 8000:8099) ~ "Hlth",
    sic %in% c(6000:6999) ~ "Money",
    TRUE ~ "Other"
  )
}

create_factor_chars <- function(chars, factors, ind, seed) {
  data <- chars[, c("id", "eom", "eom_ret", "ff12", factors), with = FALSE]
  # If all factor values are constant, inject random noise to avoid zero-variance
  set.seed(seed)
  data[, (factors) := lapply(.SD, function(x) {
    if (round(sd(x), 6) == 0) x <- x + rnorm(length(x))
    return(x)
  }), .SDcols = factors, by = eom]
  # Standardize factors (mean=0, sd=1) by month
  data[, (factors) := lapply(.SD, function(x) (x - mean(x)) / sd(x)), by = eom, .SDcols = factors]
  # Add industry dummies or market factor
  if (ind) {
    industries <- sort(unique(data$ff12))
    for (i in industries) {
      data[, (i) := as.integer(ff12 == i)]
    }
  } else {
    data[, mkt := 1]
  }
  return(data)
}

# Section 3: Factor Returns ---------------------------------------------------
x_vars_fun <- function(factors, ind) {
  if (ind) {
    ind_factors <- c("BusEq", "Chems", "Durbl", "Enrgy", "Hlth", "Manuf",
                     "Money", "NoDur", "Other", "Shops", "Telcm", "Utils")
  } else {
    ind_factors <- "mkt"
  }
  c(ind_factors, factors)
}

factor_formula <- function(y, factors, ind) {
  if (ind) {
    ind_factors <- c("BusEq", "Chems", "Durbl", "Enrgy", "Hlth", "Manuf",
                     "Money", "NoDur", "Other", "Shops", "Telcm", "Utils")
  } else {
    ind_factors <- "mkt"
  }
  if (is.null(factors)) {
    form <- paste0(y, "~-1+", paste0(ind_factors, collapse = "+"))
  } else {
    cls_names <- paste0("`", factors, "`")
    form <- paste0(y, "~-1+", paste0(c(ind_factors, cls_names), collapse = "+"))
  }
  return(form)
}

create_factor_regs_ridge <- function(chars, factor_chars, daily, factors, ind, lambda) {
  # Join chars to daily data: chars eom_ret aligns with daily eom (= month of daily returns)
  data <- daily[date >= min(factor_chars$eom) & id %in% unique(chars$id)][
    , .(id, date, ret_exc, eom_ret = eom)]
  factor_chars_d <- factor_chars[data, on = .(id, eom_ret), nomatch = 0L]
  # Factor return formula
  form <- factor_formula(y = "ret_exc", factors = factors, ind = ind)
  # Run daily cross-sectional ridge regressions
  days <- factor_chars_d$date |> unique() |> sort()
  fct_regs <- days |> map(function(d) {
    sub <- factor_chars_d[date == d]
    X <- model.matrix(as.formula(form), data = sub)
    y <- sub$ret_exc
    fit_ridge <- glmnet(X, y, alpha = 0, lambda = lambda, standardize = FALSE, intercept = FALSE)
    # Factor returns
    factor_returns <- fit_ridge |>
      broom::tidy() |>
      mutate(date = d) |>
      select(date, term, estimate) |>
      pivot_wider(names_from = term, values_from = estimate) |>
      setDT()
    # Factor residuals
    y_pred <- predict(fit_ridge, X, s = lambda)
    residuals <- y - y_pred
    factor_res <- data.table(id = sub$id, date = d, res = drop(residuals))
    list(factor_returns = factor_returns, factor_res = factor_res)
  }, .progress = "   Factor regression by date")
  # Combine (fill=TRUE because some industries may have no stocks on certain days)
  factor_returns <- fct_regs |> map("factor_returns") |> rbindlist(fill = TRUE)
  # Impute missing factor returns: industry cols get median of non-missing industry
  # returns that day, non-industry cols get median of non-missing feature returns
  ind_cols <- intersect(c("BusEq", "Chems", "Durbl", "Enrgy", "Hlth", "Manuf",
                          "Money", "NoDur", "Other", "Shops", "Telcm", "Utils"),
                        names(factor_returns))
  feat_cols <- setdiff(names(factor_returns), c("date", ind_cols))
  if (length(ind_cols) > 0) {
    factor_returns[, (ind_cols) := {
      vals <- unlist(.SD)
      med <- median(vals, na.rm = TRUE)
      lapply(.SD, function(x) fifelse(is.na(x), med, x))
    }, .SDcols = ind_cols, by = date]
  }
  if (length(feat_cols) > 0) {
    factor_returns[, (feat_cols) := {
      vals <- unlist(.SD)
      med <- median(vals, na.rm = TRUE)
      lapply(.SD, function(x) fifelse(is.na(x), med, x))
    }, .SDcols = feat_cols, by = date]
  }
  factor_res <- fct_regs |> map("factor_res") |> rbindlist()
  list(factor_returns = factor_returns, factor_res = factor_res)
}

# Section 4: Covariance Estimation ---------------------------------------------
ewma_vol <- function(x, lambda, start) {
  # Pure R EWMA volatility (replaces missing Rcpp ewma_c)
  # x: vector of returns (or residuals)
  # lambda: decay factor, e.g. 0.5^(1/84)
  # start: number of initial observations to seed the variance
  n <- length(x)
  result <- rep(NA_real_, n)
  if (n <= start) return(result)
  # Seed variance from first `start` observations
  var_t <- mean(x[1:start]^2)
  result[start] <- sqrt(var_t)
  for (t in (start + 1):n) {
    var_t <- lambda * var_t + (1 - lambda) * x[t]^2
    result[t] <- sqrt(var_t)
  }
  return(result)
}

create_factor_cov <- function(factor_returns, cov_set, test_dates) {
  fct_dates <- sort(factor_returns$date)
  w_cor <- (0.5^(1 / cov_set$hl_cor))^(cov_set$obs:1)
  w_var <- (0.5^(1 / cov_set$hl_var))^(cov_set$obs:1)

  factor_cov_est <- test_dates |> map(function(d) {
    first_obs <- min(tail(fct_dates[fct_dates <= d], cov_set$obs))
    cov_data <- factor_returns[date >= first_obs & date <= d]
    t <- nrow(cov_data)
    if (t < cov_set$obs - 30) warning("INSUFFICIENT NUMBER OF OBSERVATIONS!!")
    cov_est <- cov_data |> select(-date) |> cov.wt(wt = tail(w_cor, t), cor = TRUE, center = TRUE, method = "unbiased")
    cor_est <- cov_est$cor
    var_est <- cov_data |> select(-date) |> cov.wt(wt = tail(w_var, t), cor = FALSE, center = TRUE, method = "unbiased")
    sd_diag <- diag(sqrt(diag(var_est$cov)), ncol = ncol(cor_est))
    cov_est <- sd_diag %*% cor_est %*% sd_diag
    rownames(cov_est) <- colnames(cov_est) <- colnames(cor_est)
    return(cov_est)
  }, .progress = "   Factor covariance estimates by date")
  names(factor_cov_est) <- as.character(test_dates)
  return(factor_cov_est)
}

create_specific_risk <- function(factor_res, cov_set) {
  data <- copy(factor_res)
  data |> setorder(id, date)
  # EWMA variance of residuals per stock
  data[, res_vol := ewma_vol(res,
                             lambda = 0.5^(1 / cov_set$hl_stock_var),
                             start = cov_set$initial_var_obs), by = id]
  # Require at least 200 non-missing observations out of the last 252 trading days
  fct_dates <- sort(unique(factor_res$date))
  td_range <- data.table(date = fct_dates, td_252d = shift(fct_dates, 252, type = "lag"))
  data <- td_range[data, on = "date"]
  data[, date_200d := shift(date, 200, type = "lag"), by = id]
  data <- data[date_200d >= td_252d & !is.na(res_vol)][, .(id, date, res_vol)]
  # Extract specific risk at month end
  data[, eom_ret := ceiling_date(date, unit = "month") - 1]
  data[, max_date := max(date), by = .(id, eom_ret)]
  data <- data[date == max_date, .(id, eom = eom_ret, res_vol)]
  return(data)
}

create_specific_risk_models_ridge <- function(factor_chars, spec_risk, factors, ind, lambda) {
  data <- spec_risk[factor_chars, on = .(id, eom)]
  data <- data[!is.na(res_vol)]
  data[, res_vol_log := log(res_vol)]
  form <- factor_formula(y = "res_vol_log", factors = factors, ind = ind)
  eoms <- data$eom |> unique() |> sort()
  models <- eoms |> map(function(m) {
    sub <- data[eom == m]
    X <- model.matrix(as.formula(form), data = sub)
    y <- sub$res_vol_log
    fit_ridge <- glmnet(X, y, alpha = 0, lambda = lambda, standardize = FALSE, intercept = FALSE)
    tibble(eom = m, fit = list(fit_ridge))
  }, .progress = "   Spec risk models") |> bind_rows()
  return(models)
}

create_spec_risk_preds <- function(factor_chars, spec_risk_models, test_dates, x_vars) {
  preds_full <- test_dates |> map(function(d) {
    data <- factor_chars[eom == d]
    model <- spec_risk_models |> filter(eom == d) |> pull(fit)
    preds <- predict(model[[1]], as.matrix(data[, x_vars, with = FALSE]))
    data[, .(id, eom, res_vol_pred = drop(exp(preds)))]
  }, .progress = "   Specific risk predictions") |> rbindlist()
  return(preds_full)
}

# Section 5: Portfolio Construction --------------------------------------------
woodbury_solve <- function(D_diag, Sigma_f, X, b) {
  # Solve (D + X Sigma_f X')^{-1} b using the Woodbury identity:
  # (D + X Sf X')^{-1} = D^{-1} - D^{-1} X (Sf^{-1} + X' D^{-1} X)^{-1} X' D^{-1}
  # Only inverts K x K matrix (factors) instead of N x N (stocks)
  D_inv_b <- b / D_diag
  D_inv_X <- X / D_diag  # N x K, each row of X divided by corresponding D_diag element
  # Force symmetry on Sigma_f before inverting
  Sigma_f <- (Sigma_f + t(Sigma_f)) / 2
  Sf_inv <- solve(Sigma_f)
  M <- Sf_inv + t(X) %*% D_inv_X  # K x K
  M <- (M + t(M)) / 2  # Force symmetry
  M_inv <- solve(M)
  # Woodbury: D_inv_b - D_inv_X %*% M_inv %*% (X' D_inv_b)
  D_inv_b - D_inv_X %*% (M_inv %*% (t(X) %*% D_inv_b))
}

compute_min_var_weights <- function(factor_cov_d, factor_chars_sub, x_vars) {
  factor_chars_sub |> setorder(id)
  X <- as.matrix(factor_chars_sub[, x_vars, with = FALSE])
  D_diag <- factor_chars_sub$res_vol^2
  n <- nrow(factor_chars_sub)
  ones <- rep(1, n)
  # w = Sigma^{-1} 1 / (1' Sigma^{-1} 1)
  Sigma_inv_ones <- woodbury_solve(D_diag, factor_cov_d, X, ones)
  w <- drop(Sigma_inv_ones) / sum(Sigma_inv_ones)
  data.table(id = factor_chars_sub$id, eom = factor_chars_sub$eom, w = w)
}

# Section 6: Main Entry Point -------------------------------------------------
main <- function(chars, features, daily_ret) {
  # Settings
  seed <- 1
  ind <- TRUE
  ridge_lambda <- 1e-4
  cov_set <- list(
    obs = 2520,             # 252 * 10 trading days
    hl_cor = 504,           # Barra USE4S correlation half-life
    hl_var = 84,            # Barra USE4S variance half-life
    hl_stock_var = 84,      # Barra USE4S stock-specific variance half-life
    min_stock_obs = 252,
    initial_var_obs = 63    # 21 * 3 = ~3 months to seed EWMA
  )

  # Convert inputs to data.table
  chars <- as.data.table(chars)
  daily_ret <- as.data.table(daily_ret)
  features <- features$features

  # Prepare features: percentile rank + impute
  chars <- chars |>
    prepare_pred_data(features = features, feat_prank = TRUE, impute = TRUE)

  # Add FF12 industry classification
  chars[, ff12 := ff12_class(sic)]

  # Create factor characteristics (standardized features + industry dummies)
  factors <- features
  factor_chars <- create_factor_chars(
    chars = chars,
    factors = factors,
    ind = ind,
    seed = seed
  )

  # Prepare daily returns: add eom (month-end date for joining with chars)
  daily_ret[, eom := ceiling_date(date, unit = "month") - 1]

  # Run daily cross-sectional ridge regressions to get factor returns + residuals
  cat("Running factor regressions...\n")
  factor_regs <- create_factor_regs_ridge(
    chars = chars,
    factor_chars = factor_chars,
    daily = daily_ret,
    factors = factors,
    ind = ind,
    lambda = ridge_lambda
  )

  # Compute EWMA specific risk from residuals
  cat("Computing specific risk...\n")
  spec_risk <- create_specific_risk(
    factor_res = factor_regs$factor_res,
    cov_set = cov_set
  )

  # Train specific risk cross-sectional models per month
  cat("Training specific risk models...\n")
  spec_risk_models <- create_specific_risk_models_ridge(
    factor_chars = factor_chars,
    spec_risk = spec_risk,
    factors = factors,
    ind = ind,
    lambda = ridge_lambda
  )

  # Identify test dates and x-variables
  test_dates <- chars[ctff_test == 1, sort(unique(eom))]
  x_vars <- x_vars_fun(factors = factors, ind = ind)

  # Predict specific risk for test dates
  cat("Predicting specific risk for test dates...\n")
  spec_risk_preds <- create_spec_risk_preds(
    factor_chars = factor_chars,
    spec_risk_models = spec_risk_models,
    test_dates = test_dates,
    x_vars = x_vars
  )

  # Merge actual specific risk with predictions (use actual where available)
  spec_risk_full <- spec_risk[spec_risk_preds, on = .(id, eom)]
  spec_risk_full[is.na(res_vol), res_vol := res_vol_pred]
  spec_risk_full[, res_vol_pred := NULL]

  # Attach specific risk to factor chars
  factor_chars <- spec_risk_full[factor_chars, on = .(id, eom)]

  # Compute factor covariance matrices for test dates
  cat("Computing factor covariance matrices...\n")
  factor_cov <- create_factor_cov(
    factor_returns = factor_regs$factor_returns,
    cov_set = cov_set,
    test_dates = test_dates
  )

  # Compute minimum-variance weights for each test date
  cat("Computing minimum-variance portfolios...\n")
  weights <- test_dates |> map(function(d) {
    factor_cov_d <- factor_cov[[as.character(d)]]
    factor_chars_sub <- factor_chars[eom == d & !is.na(res_vol)]
    compute_min_var_weights(factor_cov_d, factor_chars_sub, x_vars)
  }, .progress = "   Min-var portfolios by date") |> rbindlist()

  return(weights[, .(id, eom, w)])
}

# Section 7: Local Testing -----------------------------------------------------
if (FALSE) {
  features <- arrow::read_parquet("data/raw/ctff_features.parquet")
  chars <- arrow::read_parquet("data/raw/ctff_chars.parquet")
  daily_ret <- arrow::read_parquet("data/raw/ctff_daily_ret.parquet")

  pf <- main(chars = chars, features = features, daily_ret = daily_ret)
  print(head(pf))
  print(paste("Unique months:", length(unique(pf$eom))))
  print(paste("Total rows:", nrow(pf)))

  # Save output
  pf |> fwrite("data/processed/minimum_variance.csv")
}
