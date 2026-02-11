# Prepare prediction data -------------
prepare_pred_data <- function(data, features, feat_prank, impute) {
  # Convert to Percentile Ranks
  if (feat_prank) {
    data[, (features) := lapply(.SD, as.double), .SDcols = features]  # Convert feature columns to double to avoid loosing precision 
    for(f in features) {
      if (match(f, features) %% 10 == 0) print(paste0("Feature ", match(f, features), " out of ", length(features)))
      data[, zero := (get(f) == 0)]
      data[!is.na(get(f)), (f) := ecdf(get(f))(get(f)), by = .(excntry, eom)] # Didn't have by statement before!!
      data[zero == T, (f) := 0][, zero := NULL]  # Set exact zeros to 0 (ecdf always returns >0)
      # Subtract -0.5
      data[, (f) := get(f)-0.5]
    }
  }
  # Impute
  if (impute) {
    if (feat_prank) {
      data[, (features) := lapply(.SD, function(x) if_else(is.na(x), 0, x)), .SDcols=features]
    } else {
      data[, (features) := lapply(.SD, function(x) if_else(is.na(x), median(x, na.rm=T), x)), .SDcols=features, by=.(excntry, eom)]
    }
  }
  return(data)
}

# ML model --------------------------------------
# Fold dates
fold_dates_fun <- function(test_date, train_years, folds) {
  train_last <- test_date+1-months(1)
  train_dates <- seq.Date(
    from = train_last-years(train_years)+months(1), 
    to = train_last, 
    by = "1 month")-1
  fold_dates <- split(train_dates, cut(seq_along(train_dates), folds, labels = FALSE))
  return(fold_dates)
}

# Fold data 
fold_data_fun <- function(data, feat, fold_dates, y, i) {
  train_dates <- do.call(c, fold_dates[-i])
  val_dates <- fold_dates[[i]]
  train <- data[eom_ret %in% train_dates]
  train <- xgb.DMatrix(data=as.matrix(train[, feat, with=F]), 
                       label=train[[y]])
  val <- data[eom_ret %in% val_dates]
  val <- xgb.DMatrix(data=as.matrix(val[, feat, with=F]), 
                     label=val[[y]])
  # Output
  return(list(train=train, val=val))
}

# TWO STAGE XGB ----------------------------------------------------------------
# Fit ---
fit_xgb <- function(train, val, params_base, params, iter, es, cores, seed) { # train and val should be xgb.Dmatrix objects
  set.seed(seed)
  
  params_all <- list(
    tree_method = params_base$tree_method,
    objective = params_base$objective,
    base_score = params_base$base_score,            
    eval_metric = params_base$eval_metric, 
    booster = params_base$booster,
    max_depth = params$tree_depth, 
    eta = params$learn_rate, 
    gamma = params$loss_reduction,  
    subsample = params$sample_size,        # Row subsampling
    colsample_bytree = params$mtry,        # Column Subsampling
    min_child_weight = params$min_n,
    lambda = params$penalty
  )
  
  # Fit Model
  model <- xgb.train(
    data = train, 
    params = params_all,
    watchlist = list(train=train, val=val),
    nrounds = iter, 
    early_stopping_rounds = es,
    verbose = 0, 
    maximize = F,
    nthread = cores
  )
  return(model)
}


# Hp search ---
xgb_hp_search <- function(train, val, feat, params_base, hp_grid, 
                          iter, es, cores, seed, print=F) {
  val_y <- val |> getinfo("label")
  train_mean <- train |> getinfo("label") |> mean()
  # Stage 1: Find tree parameters
  search <- 1:nrow(hp_grid) %>% lapply(function(j) {
    if(print) print(paste0("HP: ", j))
    hps <- hp_grid[j, ]
    xgb_fit <- fit_xgb(train = train, val = val, params_base = params_base, 
                       params = hps, iter = iter, es = es, cores = cores, 
                       seed = seed)
    # Compute stats
    val_mse <- xgb_fit$best_score^2 
    stats <- data.table(
      val_mse = val_mse,
      val_rmse=val_mse^0.5, 
      r2 = 1 - val_mse / mean((val_y - mean(val_y))^2),
      r2_zero = 1 - val_mse / mean(val_y^2),
      r2_oos = 1 - val_mse / mean((val_y - train_mean)^2),
      best_iter=xgb_fit$best_iteration
    ) 
    if(print) print(stats)
    # Output
    cbind(hps, stats)
  }) %>% rbindlist()
  # Output
  return(search)
}

# Fold aggregation ---
search_best_fun <- function(search) {
  agg <- search |> 
    group_by(hp_set, mtry, tree_depth, sample_size, penalty, min_n, loss_reduction, learn_rate) |> 
    summarise(
      n = n(),
      best_iter_avg = floor(mean(best_iter)),
      mse_avg = mean(val_mse)
      , .groups = "drop"
    ) |> 
    ungroup() |> 
    filter(mse_avg == min(mse_avg))
  agg[1, ] # In the past, I've had one case with identical mse(!). Here I arbitrarily just choose the first
}

# Main XGB function to implement all steps ---
xgb_main <- function(data_list, fold_dates, feat, params_base, hp_grid, 
                     eta1, eta2, iter1, iter2, es, cores, seed) {
  # HP tuning stage 1: Find tree parameters ---
  tictoc::tic("   Stage 1 HP tuning took:")
  hp_grid1 <- hp_grid %>% mutate(learn_rate = eta1) # Add eta to hyper-parameter list
  search_stage1 <- 1:length(fold_dates) |> map(function(i) {
    # Create fold-specific train and val sets
    fold_data_list <- data_list$train |> 
      fold_data_fun(
        fold_dates = fold_dates, 
        feat = feat,
        y = "ret_exc_lead1m", 
        i = i
      )
    # Search over hyper-parameters
    search_stage1_i <- xgb_hp_search(
      train = fold_data_list$train,
      val = fold_data_list$val,
      feat = feat, 
      params_base = params_base,
      hp_grid = hp_grid1, 
      iter = iter1, 
      es = es, 
      cores = cores, 
      seed = seed
    )
    # Output
    search_stage1_i |> mutate(fold = i)
  }, .progress = "Stage 1") |> bind_rows()
  # Select best hyper-parameters
  best_hp1 <- search_stage1 |> search_best_fun()
  best_iter1 <- best_hp1$best_iter_avg
  tictoc::toc()
  
  # Stage 2: Find optimal number of iterations (with lower eta) ---
  tictoc::tic("   Stage 2 HP tuning took:")
  hp_grid2 <- best_hp1 %>% 
    select(mtry, tree_depth, sample_size, penalty, min_n, loss_reduction, hp_set) |> 
    mutate(learn_rate = eta2)
  search_stage2 <- 1:length(fold_dates) |> map(function(i) {
    # Create fold-specific train and val sets
    fold_data_list <- data_list$train |> 
      fold_data_fun(
        fold_dates = fold_dates, 
        feat = feat,
        y = "ret_exc_lead1m", 
        i = i
      )
    # Search over hyper-parameters
    search_stage2_i <- xgb_hp_search(
      train = fold_data_list$train,
      val = fold_data_list$val,
      feat = feat, 
      params_base = params_base,
      hp_grid = hp_grid2, 
      iter = iter2, 
      es = es, 
      cores = cores, 
      seed = seed
    )
    # Output
    search_stage2_i |> mutate(fold = i)
  }, .progress = "Stage 1") |> bind_rows()
  # Select best hyper-parameters
  best_hp2 <- search_stage2 |> search_best_fun()
  best_iter2 <- best_hp2$best_iter_avg
  tictoc::toc()
  
  # Re-fit to all training data
  tictoc::tic("   Final fit took:")
  train_all <- xgb.DMatrix(data=as.matrix(data_list$train[, feat, with=F]), 
                           label=data_list$train$ret_exc_lead1m)
  final_fit <- fit_xgb(train = train_all, val = train_all, 
                       params_base = params_base, params = best_hp2, 
                       iter = best_iter2, 
                       es = NULL, cores = cores, seed = seed)
  tictoc::toc()
  # Feature importance
  set.seed(seed)
  shap_contrib <- predict(
    object = final_fit, 
    newdata = as.matrix(sample_n(data_list$train[, feat, with=F], 10000)),
    predcontrib = T
  )
  global_imp <- abs(shap_contrib) %>% 
    colMeans() %>% 
    as_tibble(rownames = "char") %>% 
    filter(char != "BIAS") %>%
    mutate(
      rank = frank(-abs(value))
    )
  print(global_imp %>%
          filter(rank <= 20) %>% 
          ggplot(aes(reorder(char, abs(value)), abs(value))) + coord_flip() + geom_col()  + labs(y = "Global feature Importance") + theme(axis.title.y =  element_blank()))
  
  # Predictions
  pred <- final_fit %>% predict(newdata=data_list$test[, feat, with=F] %>% as.matrix())
  pred_op <- data_list$test[, .(id, eom, pred=drop(pred))]
  
  # Output
  list("fit"=final_fit, 
       "best_hp1"=best_hp1, "best_hp2"=best_hp2, 
       "best_iter1"=best_iter1, "best_iter2"=best_iter2, 
       "search_stage1"=search_stage1, "search_stage2"=search_stage2,
       "pred"=pred_op, "feat_imp"=global_imp)
}
