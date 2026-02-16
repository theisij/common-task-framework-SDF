# markowitz_ml.R — Mean-variance (tangency) portfolio
# Combines XGBoost expected returns with Barra USE4S factor covariance

# Section 1: Libraries ---------------------------------------------------------
library(arrow)
library(data.table)
library(tidyverse)
library(xgboost)
library(dials)
library(glmnet)

# Section 2: Shared Utilities --------------------------------------------------
source("utils/R/data_prep.R")
source("utils/R/xgb_utils.R")
source("utils/R/factor_model_utils.R")

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
  cores <- min(parallel::detectCores() - 4, 16) |> max(1)  # Setting fixed upper bound for reproducibility
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
