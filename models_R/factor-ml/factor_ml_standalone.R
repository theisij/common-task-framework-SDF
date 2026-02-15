library(arrow)
library(data.table)
library(tidyverse)
library(xgboost)
library(dials)

# factor_ml.R — XGBoost factor model for CTF competition
# Consolidated from models_R/factor-ml-old/ (7-file pipeline)

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
