# factor_ml.R — XGBoost factor model for CTF competition
# Consolidated from models_R/factor-ml-old/ (7-file pipeline)

# Section 1: Libraries ---------------------------------------------------------
library(arrow)
library(data.table)
library(tidyverse)
library(xgboost)
library(dials)

# Section 2: Shared Utilities --------------------------------------------------
source("utils/R/data_prep.R")
source("utils/R/xgb_utils.R")

# Section 3: Portfolio Construction --------------------------------------------
predictions_to_weights <- function(preds, n_pfs = 10) {
  # Assign long/short using 10th/90th percentile thresholds for robustness
  preds[, pf_ls := case_when(
    pred <= quantile(pred, 1 / n_pfs) ~ "short",
    pred >= quantile(pred, 1 - 1 / n_pfs) ~ "long",
    TRUE ~ "none"
  ), by = .(excntry, eom)]

  # Equal-weight long-short
  weights <- copy(preds)
  weights[, n_side := .N, by = .(eom, pf_ls)]
  weights[, w := case_when(
    pf_ls == "long" ~ 1 / n_side,
    pf_ls == "short" ~ -1 / n_side,
    TRUE ~ 0
  )]
  weights[, c("pf_ls", "n_side", "pred") := NULL]
  return(weights[, .(id, eom, w)])
}

# Section 4: Main Entry Point -------------------------------------------------
main <- function(chars, features, daily_ret) {
  # Settings
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
  n_pfs <- 10
  test_period_length <- 12  # months per chunk: 1 = tune every month, 12 = tune once/year

  # Convert to data.table
  chars <- as.data.table(chars)
  features <- features$features

  # Prepare data: percentile rank + impute
  chars <- chars |>
    prepare_pred_data(features = features, feat_prank = T, impute = T)

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
  test_dates <- chars[ctff_test == 1, sort(unique(eom_ret))]

  # Group test dates into chunks of test_period_length months
  test_chunks <- split(test_dates, ceiling(seq_along(test_dates) / test_period_length))

  # Loop over chunks
  all_preds <- test_chunks |> map(function(chunk_dates) {
    cat(sprintf("Training model for test period %s to %s\n", min(chunk_dates), max(chunk_dates)))
    d <- chunk_dates[1]  # training window ends before first date in chunk
    train_first <- d + 1 - months(1) - years(train_years) + months(1) - 1
    data_list <- list()
    data_list$test <- chars[eom_ret %in% chunk_dates]
    data_list$train <- chars[eom_ret >= train_first & eom_ret < d]

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

  # Convert predictions to long-short portfolio weights
  # Need excntry for grouping — merge back from chars
  all_preds <- chars[, .(id, eom, excntry)][all_preds, on = .(id, eom)]
  weights <- predictions_to_weights(all_preds, n_pfs = n_pfs)

  return(weights)
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
  pf |> fwrite("data/processed/factor_ml.csv")
}
