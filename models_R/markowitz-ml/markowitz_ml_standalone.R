library(arrow)
library(data.table)
library(tidyverse)
library(xgboost)
library(dials)
library(glmnet)

# markowitz_ml.R — Mean-variance (tangency) portfolio
# Combines XGBoost expected returns with Barra USE4S factor covariance

# Section 1: Libraries ---------------------------------------------------------

# Section 2: Shared Utilities --------------------------------------------------
# [build] Begin inlined: utils/R/data_prep.R
# data_prep.R — Shared data preparation utilities
# Used by: factor_ml, minimum_variance, markowitz_ml

prepare_pred_data <- function(data, features, feat_prank, impute, min_obs = NULL) {
  if (feat_prank) {
    data[, (features) := lapply(.SD, as.double), .SDcols = features]
    cat(sprintf("Percentile-ranking %d features...\n", length(features)))
    data[, (features) := lapply(.SD, function(x) {
      non_na <- !is.na(x)
      # Optionally require a minimum number of non-missing observations to rank
      if (!is.null(min_obs) && sum(non_na) < min_obs) return(rep(NA_real_, length(x)))
      is_zero <- non_na & (x == 0)
      x[non_na] <- frank(x[non_na], ties.method = "max") / sum(non_na)
      # Override zeros universally: while 0 may not literally be the lowest
      # value for some variables (e.g., chcsho_12m), it gives XGBoost a
      # consistent way to identify exact zeros, which is likely a special value.
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
# [build] End inlined: utils/R/data_prep.R
# [build] Begin inlined: utils/R/xgb_utils.R
# xgb_utils.R — XGBoost cross-validation and training utilities
# Used by: factor_ml, markowitz_ml

fold_dates_fun <- function(test_date, train_years, folds) {
  train_last <- test_date + 1 - months(1)
  train_dates <- seq.Date(
    from = train_last - years(train_years) + months(1),
    to = train_last,
    by = "1 month"
  ) - 1
  fold_dates <- split(train_dates, cut(seq_along(train_dates), folds, labels = FALSE))
  return(fold_dates)
}

fold_data_fun <- function(data, feat, fold_dates, y, i) {
  train_dates <- do.call(c, fold_dates[-i])
  val_dates <- fold_dates[[i]]
  train <- data[eom_ret %in% train_dates]
  train <- xgb.DMatrix(data = as.matrix(train[, feat, with = FALSE]),
                        label = train[[y]])
  val <- data[eom_ret %in% val_dates]
  val <- xgb.DMatrix(data = as.matrix(val[, feat, with = FALSE]),
                      label = val[[y]])
  return(list(train = train, val = val))
}

fit_xgb <- function(train, val, params_base, params, iter, es, cores, seed) {
  set.seed(seed)
  params_all <- list(
    tree_method = params_base$tree_method,
    objective = params_base$objective,
    base_score = params_base$base_score,
    eval_metric = params_base$eval_metric,
    booster = params_base$booster,
    max_depth = as.integer(params$tree_depth),
    learning_rate = as.numeric(params$learn_rate),
    min_split_loss = as.numeric(params$loss_reduction),
    subsample = as.numeric(params$sample_size),
    colsample_bytree = as.numeric(params$mtry),
    min_child_weight = as.numeric(params$min_n),
    reg_lambda = as.numeric(params$penalty),
    nthread = cores
  )
  model <- xgb.train(
    data = train,
    params = params_all,
    evals = list(train = train, val = val),
    nrounds = iter,
    early_stopping_rounds = es,
    verbose = 0,
    maximize = FALSE
  )
  return(model)
}

xgb_hp_search <- function(train, val, feat, params_base, hp_grid,
                           iter, es, cores, seed, print = F) {
  hp_grid <- as.data.table(hp_grid)
  val_y <- val |> getinfo("label")
  train_mean <- train |> getinfo("label") |> mean()
  search <- 1:nrow(hp_grid) |> lapply(function(j) {
    if (print) print(paste0("HP: ", j))
    hps <- hp_grid[j, ]
    xgb_fit <- fit_xgb(train = train, val = val, params_base = params_base,
                        params = hps, iter = iter, es = es, cores = cores,
                        seed = seed)
    val_mse <- as.numeric(xgb.attr(xgb_fit, "best_score"))^2
    stats <- data.table(
      val_mse = val_mse,
      val_rmse = val_mse^0.5,
      r2 = 1 - val_mse / mean((val_y - mean(val_y))^2),
      r2_zero = 1 - val_mse / mean(val_y^2),
      r2_oos = 1 - val_mse / mean((val_y - train_mean)^2),
      best_iter = as.integer(xgb.attr(xgb_fit, "best_iteration"))
    )
    if (print) print(stats)
    cbind(hps, stats)
  }) |> rbindlist()
  return(search)
}

search_best_fun <- function(search) {
  agg <- search |>
    group_by(hp_set, mtry, tree_depth, sample_size, penalty, min_n, loss_reduction, learn_rate) |>
    summarise(
      n = n(),
      best_iter_avg = floor(mean(best_iter)),
      mse_avg = mean(val_mse),
      .groups = "drop"
    ) |>
    ungroup() |>
    filter(mse_avg == min(mse_avg))
  # If multiple hyperparameter sets share the same minimum mse_avg,
  # the first one in the current ordering is returned as the best set.
  agg[1, ]
}

xgb_main <- function(data_list, fold_dates, feat, params_base, hp_grid,
                      eta1, eta2, iter1, iter2, es, cores, seed) {
  # Stage 1: Find tree parameters with high learning rate
  hp_grid1 <- hp_grid |> mutate(learn_rate = eta1)
  search_stage1 <- 1:length(fold_dates) |> map(function(i) {
    fold_data_list <- data_list$train |>
      fold_data_fun(fold_dates = fold_dates, feat = feat,
                    y = "ret_exc_lead1m", i = i)
    search_stage1_i <- xgb_hp_search(
      train = fold_data_list$train, val = fold_data_list$val,
      feat = feat, params_base = params_base, hp_grid = hp_grid1,
      iter = iter1, es = es, cores = cores, seed = seed
    )
    search_stage1_i |> mutate(fold = i)
  }, .progress = "Stage 1") |> bind_rows()
  best_hp1 <- search_stage1 |> search_best_fun()

  # Stage 2: Find optimal iterations with low learning rate
  hp_grid2 <- best_hp1 |>
    select(mtry, tree_depth, sample_size, penalty, min_n, loss_reduction, hp_set) |>
    mutate(learn_rate = eta2)
  search_stage2 <- 1:length(fold_dates) |> map(function(i) {
    fold_data_list <- data_list$train |>
      fold_data_fun(fold_dates = fold_dates, feat = feat,
                    y = "ret_exc_lead1m", i = i)
    search_stage2_i <- xgb_hp_search(
      train = fold_data_list$train, val = fold_data_list$val,
      feat = feat, params_base = params_base, hp_grid = hp_grid2,
      iter = iter2, es = es, cores = cores, seed = seed
    )
    search_stage2_i |> mutate(fold = i)
  }, .progress = "Stage 2") |> bind_rows()
  best_hp2 <- search_stage2 |> search_best_fun()
  best_iter2 <- best_hp2$best_iter_avg

  # Re-fit to all training data
  train_all <- xgb.DMatrix(data = as.matrix(data_list$train[, feat, with = FALSE]),
                            label = data_list$train$ret_exc_lead1m)
  final_fit <- fit_xgb(train = train_all, val = train_all,
                        params_base = params_base, params = best_hp2,
                        iter = best_iter2, es = NULL, cores = cores, seed = seed)

  # Predictions
  pred <- final_fit |> predict(newdata = data_list$test[, feat, with = FALSE] |> as.matrix())
  pred_op <- data_list$test[, .(id, eom, pred = drop(pred))]
  return(pred_op)
}
# [build] End inlined: utils/R/xgb_utils.R
# [build] Begin inlined: utils/R/factor_model_utils.R
# factor_model_utils.R — Barra factor model utilities
# Used by: minimum_variance, markowitz_ml

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
      if (is.na(med)) med <- 0  # Fallback if all values in category are NA
      lapply(.SD, function(x) fifelse(is.na(x), med, x))
    }, .SDcols = ind_cols, by = date]
  }
  if (length(feat_cols) > 0) {
    factor_returns[, (feat_cols) := {
      vals <- unlist(.SD)
      med <- median(vals, na.rm = TRUE)
      if (is.na(med)) med <- 0  # Fallback if all values in category are NA
      lapply(.SD, function(x) fifelse(is.na(x), med, x))
    }, .SDcols = feat_cols, by = date]
  }
  factor_res <- fct_regs |> map("factor_res") |> rbindlist()
  list(factor_returns = factor_returns, factor_res = factor_res)
}

ewma_vol <- function(x, lambda, start) {
  # Pure R EWMA volatility matching Rcpp ewma_c() exactly
  # x: vector of returns (or residuals)
  # lambda: decay factor, e.g. 0.5^(1/84)
  # start: number of initial observations to seed the variance
  n <- length(x)
  result <- rep(NA_real_, n)
  if (n <= start) return(result)
  # Seed variance from first `start` observations (Bessel correction, skip NAs)
  x_init <- x[1:start]
  non_na <- !is.na(x_init)
  var_t <- sum(x_init[non_na]^2) / (sum(non_na) - 1)
  # Seed goes at index start+1 (0-indexed start in C++ = 1-indexed start+1 in R)
  result[start + 1] <- sqrt(var_t)
  # EWMA update uses the PREVIOUS observation x[t-1], not x[t]
  if ((start + 2) <= n) {
    for (t in (start + 2):n) {
      if (is.na(x[t - 1])) {
        # Carry forward previous variance on NA
        result[t] <- result[t - 1]
      } else {
        var_t <- lambda * var_t + (1 - lambda) * x[t - 1]^2
        result[t] <- sqrt(var_t)
      }
    }
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

woodbury_solve <- function(D_diag, Sigma_f, X, b) {
  # Solve (D + X Sigma_f X')^{-1} b using the Woodbury identity:
  # (D + X Sf X')^{-1} = D^{-1} - D^{-1} X (Sf^{-1} + X' D^{-1} X)^{-1} X' D^{-1}
  # Only inverts K x K matrix (factors) instead of N x N (stocks)
  D_inv_b <- b / D_diag
  D_inv_X <- X / D_diag  # N x K, each row of X divided by corresponding D_diag element
  # Check and force symmetry on Sigma_f before inverting
  asym_Sf <- max(abs(Sigma_f - t(Sigma_f)))
  if (!is.finite(asym_Sf) || asym_Sf > 1e-6) {
    warning(sprintf("Sigma_f asymmetry: %.2e (forcing symmetric)", asym_Sf))
  }
  Sigma_f <- (Sigma_f + t(Sigma_f)) / 2
  Sf_inv <- solve(Sigma_f)
  M <- Sf_inv + t(X) %*% D_inv_X  # K x K
  # Check and force symmetry on M
  asym_M <- max(abs(M - t(M)))
  if (!is.finite(asym_M) || asym_M > 1e-6) {
    warning(sprintf("M asymmetry: %.2e (forcing symmetric)", asym_M))
  }
  M <- (M + t(M)) / 2
  M_inv <- solve(M)
  # Woodbury: D_inv_b - D_inv_X %*% M_inv %*% (X' D_inv_b)
  D_inv_b - D_inv_X %*% (M_inv %*% (t(X) %*% D_inv_b))
}
# [build] End inlined: utils/R/factor_model_utils.R

# Section 3: Portfolio Construction --------------------------------------------
compute_markowitz_weights <- function(factor_cov_d, factor_chars_sub, x_vars, mu, vol_ann) {
  # Tangency portfolio: w_raw = Sigma^{-1} mu, scaled to target annualized vol
  factor_chars_sub |> setorder(id)
  mu |> setorder(id)

  # Align: keep only stocks present in both factor_chars_sub and mu
  common_ids <- intersect(factor_chars_sub$id, mu$id)
  factor_chars_sub <- factor_chars_sub[id %in% common_ids]
  mu <- mu[id %in% common_ids]

  X <- as.matrix(factor_chars_sub[, x_vars, with = FALSE])
  D_diag <- factor_chars_sub$res_vol^2
  mu_vec <- mu$pred

  # w_raw = Sigma^{-1} mu via Woodbury
  w_raw <- drop(woodbury_solve(D_diag, factor_cov_d, X, mu_vec))

  # Portfolio variance via factor structure: w'Sigma w = w'Dw + (X'w)' Sigma_f (X'w)
  Xw <- drop(t(X) %*% w_raw)
  port_var <- sum(w_raw^2 * D_diag) + drop(t(Xw) %*% factor_cov_d %*% Xw)

  # Scale to target annualized volatility (monthly variance = vol_ann^2 / 12)
  # Factor covariance is daily; multiply by 21 to get monthly
  port_var_monthly <- port_var * 21
  target_var_monthly <- vol_ann^2 / 12
  w <- w_raw * sqrt(target_var_monthly / port_var_monthly)

  data.table(id = factor_chars_sub$id, eom = factor_chars_sub$eom, w = w)
}

# Section 4: Main Entry Point -------------------------------------------------
main <- function(chars, features, daily_ret) {
  # ── XGBoost settings ──
  seed <- 1
  train_years <- 10
  folds <- 5
  xgb_hps <- 20
  iter1 <- 1000
  iter2 <- 10000
  eta1 <- 0.15
  eta2 <- 0.01
  es <- 25
  cores <- max(1, parallel::detectCores() - 4)
  test_period_length <- 12

  # ── Factor model settings ──
  ind <- TRUE
  ridge_lambda <- 1e-4
  cov_set <- list(
    obs = 2520,
    hl_cor = 504,
    hl_var = 84,
    hl_stock_var = 84,
    min_stock_obs = 252,
    initial_var_obs = 63
  )

  # ── Markowitz settings ──
  vol_ann <- 0.10  # target annualized volatility

  # Convert inputs to data.table
  chars <- as.data.table(chars)
  daily_ret <- as.data.table(daily_ret)
  features <- features$features

  # ═══════════════════════════════════════════════════════════════════════════════
  # Part A: XGBoost expected returns (same pipeline as factor_ml)
  # ═══════════════════════════════════════════════════════════════════════════════
  # Prepare data for XGBoost: percentile rank + impute (no min_obs guard)
  chars_xgb <- copy(chars)
  chars_xgb <- chars_xgb |>
    prepare_pred_data(features = features, feat_prank = TRUE, impute = TRUE)

  # HP grid
  set.seed(seed)
  xgb_hp_grid <- dials::parameters(
    dials::mtry(range = c(1, length(features))),
    dials::tree_depth(range = c(1, 7), trans = NULL),
    dials::sample_prop(range = c(0.2, 1), trans = NULL),
    dials::penalty(range = c(-2, 2), trans = scales::log10_trans())
  ) |>
    dials::grid_space_filling(size = xgb_hps, type = "max_entropy") |>
    mutate(
      mtry = mtry / length(features),
      min_n = 1,
      loss_reduction = 0,
      hp_set = 1:n()
    )

  xgb_params_base <- list(
    tree_method = "hist",
    objective = "reg:squarederror",
    base_score = 0,
    eval_metric = "rmse",
    booster = "gbtree"
  )

  # Identify test dates
  test_dates_ret <- chars_xgb[ctff_test == 1, sort(unique(eom_ret))]
  test_chunks <- split(test_dates_ret, ceiling(seq_along(test_dates_ret) / test_period_length))

  cat("=== Part A: XGBoost expected returns ===\n")
  all_preds <- test_chunks |> map(function(chunk_dates) {
    cat(sprintf("Training XGB for test period %s to %s\n", min(chunk_dates), max(chunk_dates)))
    d <- chunk_dates[1]
    train_first <- d + 1 - months(1) - years(train_years) + months(1) - 1
    data_list <- list()
    data_list$test <- chars_xgb[eom_ret %in% chunk_dates]
    data_list$train <- chars_xgb[eom_ret >= train_first & eom_ret < d]

    fold_dates <- fold_dates_fun(test_date = d, train_years = train_years, folds = folds)

    xgb_main(
      data_list = data_list,
      fold_dates = fold_dates,
      feat = features,
      params_base = xgb_params_base,
      hp_grid = xgb_hp_grid,
      seed = seed,
      cores = cores,
      es = es,
      iter1 = iter1,
      iter2 = iter2,
      eta1 = eta1,
      eta2 = eta2
    )
  }, .progress = "XGB test chunks") |> rbindlist()

  # ═══════════════════════════════════════════════════════════════════════════════
  # Part B: Barra factor covariance (same pipeline as minimum_variance)
  # ═══════════════════════════════════════════════════════════════════════════════
  cat("\n=== Part B: Barra factor covariance ===\n")

  # Prepare data for factor model: percentile rank + impute (min_obs = 10)
  chars_fm <- copy(chars)
  chars_fm <- chars_fm |>
    prepare_pred_data(features = features, feat_prank = TRUE, impute = TRUE, min_obs = 10)

  # Add FF12 industry classification
  chars_fm[, ff12 := ff12_class(sic)]

  # Create factor characteristics
  factors <- features
  factor_chars <- create_factor_chars(
    chars = chars_fm, factors = factors, ind = ind, seed = seed
  )

  # Prepare daily returns
  daily_ret[, eom := ceiling_date(date, unit = "month") - 1]

  # Factor regressions
  cat("Running factor regressions...\n")
  factor_regs <- create_factor_regs_ridge(
    chars = chars_fm, factor_chars = factor_chars, daily = daily_ret,
    factors = factors, ind = ind, lambda = ridge_lambda
  )

  # Specific risk
  cat("Computing specific risk...\n")
  spec_risk <- create_specific_risk(
    factor_res = factor_regs$factor_res, cov_set = cov_set
  )

  cat("Training specific risk models...\n")
  spec_risk_models <- create_specific_risk_models_ridge(
    factor_chars = factor_chars, spec_risk = spec_risk,
    factors = factors, ind = ind, lambda = ridge_lambda
  )

  test_dates <- chars_fm[ctff_test == 1, sort(unique(eom))]
  x_vars <- x_vars_fun(factors = factors, ind = ind)

  cat("Predicting specific risk for test dates...\n")
  spec_risk_preds <- create_spec_risk_preds(
    factor_chars = factor_chars, spec_risk_models = spec_risk_models,
    test_dates = test_dates, x_vars = x_vars
  )

  # Merge actual and predicted specific risk
  spec_risk_full <- spec_risk[spec_risk_preds, on = .(id, eom)]
  spec_risk_full[is.na(res_vol), res_vol := res_vol_pred]
  spec_risk_full[, res_vol_pred := NULL]
  factor_chars <- spec_risk_full[factor_chars, on = .(id, eom)]

  # Factor covariance matrices
  cat("Computing factor covariance matrices...\n")
  factor_cov <- create_factor_cov(
    factor_returns = factor_regs$factor_returns,
    cov_set = cov_set, test_dates = test_dates
  )

  # ═══════════════════════════════════════════════════════════════════════════════
  # Part C: Tangency portfolio
  # ═══════════════════════════════════════════════════════════════════════════════
  cat("\n=== Part C: Tangency portfolio optimization ===\n")

  weights <- test_dates |> map(function(d) {
    factor_cov_d <- factor_cov[[as.character(d)]]
    factor_chars_sub <- factor_chars[eom == d & !is.na(res_vol)]
    mu_d <- all_preds[eom == d]

    if (nrow(factor_chars_sub) == 0L || nrow(mu_d) == 0L) {
      warning(sprintf("Insufficient data for %s, skipping", d))
      return(data.table(id = integer(), eom = as.Date(character()), w = numeric()))
    }

    compute_markowitz_weights(
      factor_cov_d = factor_cov_d,
      factor_chars_sub = factor_chars_sub,
      x_vars = x_vars,
      mu = mu_d,
      vol_ann = vol_ann
    )
  }, .progress = "   Markowitz portfolios by date") |> rbindlist()

  return(weights[, .(id, eom, w)])
}

# Section 5: Local Testing -----------------------------------------------------
if (FALSE) {
  features <- arrow::read_parquet("data/raw/ctff_features.parquet")
  chars <- arrow::read_parquet("data/raw/ctff_chars.parquet")
  daily_ret <- arrow::read_parquet("data/raw/ctff_daily_ret.parquet")

  pf <- main(chars = chars, features = features, daily_ret = daily_ret)
  print(head(pf))
  print(paste("Unique months:", length(unique(pf$eom))))
  print(paste("Total rows:", nrow(pf)))

  # Save output
  pf |> fwrite("data/processed/markowitz_ml.csv")
}
