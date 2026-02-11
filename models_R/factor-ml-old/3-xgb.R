# XGB HP search grid -----------------------------------------------------------
set.seed(set$seed)
xgb_hp_grid <- dials::parameters(
  dials::mtry(range = c(1, length(features))),
  dials::tree_depth(range = c(1, 7), trans = NULL),
  # dials::learn_rate(range = c(-2, -0.5), trans = scales::log10_trans()),
  dials::sample_prop(range = c(0.2, 1), trans = NULL), # v
  dials::penalty(range = c(-2, 2), trans = scales::log10_trans())  # penalty(range = c(-1, 4), trans = log10_trans())
)  %>%
  dials::grid_space_filling(
    size = set$xgb$hps, 
    type = "max_entropy"
  ) %>%
  mutate(
    mtry = mtry / length(features),
    min_n=1,
    loss_reduction = 0,  # I control this via tree depth instead
    hp_set = 1:n()
  )

xgb_params_base <- list(
  tree_method = if(use_gpu) "gpu_hist" else "hist",
  objective = "reg:squarederror",
  base_score = 0,            
  eval_metric = "rmse", 
  booster = "gbtree"
) 
# Fit --------------------------------------------------------------------------
test_dates_all <- seq.Date(
  from = as.Date("1980-02-01"), 
  to = as.Date("2024-01-01"), 
  by = "month"
)-1

xgb_all <- test_dates_all |> map(function(d) {
  print(d)
  # First training date
  train_first <- d+1-months(1)-years(set$split$train_years)+months(1)-1
  # Create data list
  data_list <- list()
  data_list$test <- pred_x[eom_ret==d]
  data_list$train <- pred_x[eom_ret>=train_first & eom_ret < d]
  # Find fold dates
  fold_dates <- fold_dates_fun(test_date=d, 
                               train_years = set$split$train_years, 
                               folds = set$split$folds)
  # Build model 
  xgb_object <- xgb_main(
    data_list = data_list,
    fold_dates = fold_dates,
    feat = features, 
    params_base = xgb_params_base,
    hp_grid = xgb_hp_grid, 
    seed = set$seed,
    cores = set$xgb$cores,
    es = set$xgb$es,
    iter1 = set$xgb$iter1,
    iter2 = set$xgb$iter2,
    eta1 = set$xgb$eta1,
    eta2 = set$xgb$eta2
  )
}, .progress = "Progress looping through test dates")
names(xgb_all) <- test_dates_all

# Save
if (save) {
  xgb_all |> saveRDS(paste0(op_folder, "xgb_all.rds"))
}
