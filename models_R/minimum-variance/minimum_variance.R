# minimum_variance.R — Minimum-variance portfolio using Barra USE4S factor risk model
# Consolidated from models_R/minimum-variance/old_code/ (4-file pipeline)

# Section 1: Libraries ---------------------------------------------------------
library(arrow)
library(data.table)
library(tidyverse)
library(glmnet)

# Section 2: Shared Utilities --------------------------------------------------
source("utils/R/data_prep.R")
source("utils/R/factor_model_utils.R")

# Section 3: Portfolio Construction --------------------------------------------
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

# Section 4: Main Entry Point -------------------------------------------------
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

  # Prepare features: percentile rank + impute (min_obs = 10 for factor model)
  chars <- chars |>
    prepare_pred_data(features = features, feat_prank = TRUE, impute = TRUE, min_obs = 10)

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
    if (nrow(factor_chars_sub) == 0L) {
      warning(sprintf("No stocks with res_vol for %s, skipping", d))
      return(data.table(id = integer(), eom = as.Date(character()), w = numeric()))
    }
    compute_min_var_weights(factor_cov_d, factor_chars_sub, x_vars)
  }, .progress = "   Min-var portfolios by date") |> rbindlist()

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
  pf |> fwrite("data/processed/minimum_variance.csv")
}
