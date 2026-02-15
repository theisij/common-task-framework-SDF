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
