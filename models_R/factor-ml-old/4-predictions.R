# Prepare data -----------------------------------------------------------------
regions <- c("us", "dev_ex_us", "em")
data_list <- regions |> map(function(x) {
  # Load data
  pred_x <- read_parquet(
    file = paste0("../Data/CTF-data/", x, "/ctff_chars.parquet"),
    col_select = all_of(c("excntry", "id", "eom", "eom_ret", 
                          "ret_exc_lead1m", "ctff_test", features))
  ) |> setDT()
  # pred_x <- pred_x |> sample_n(10000)
  # Prepare data
  pred_x <- pred_x |> 
    prepare_pred_data(
      features = features, 
      feat_prank = T, 
      impute = T
    )
}, .progress = T)
names(data_list) <- regions


# Get predictions --------------------------------------------------------------
xgb_all <- readRDS(paste0(op_folder, "xgb_all.rds"))
preds <- names(xgb_all) |> map(function(test_date) {
  model <- xgb_all[[test_date]]$fit
  if (test_date >= as.Date("1990-01-01")) {
    regs <- c("us", "dev_ex_us", "em")
  } else {
    regs <- c("us")
  }
  regs |> map(function(x) {
    sub <- data_list[[x]][eom_ret==test_date]
    preds_sub <- model %>% predict(newdata=sub[, features, with=F] %>% as.matrix())
    sub[, .(region = x, excntry, id, eom, eom_ret, pred=drop(preds_sub), ret_exc_lead1m)]
  }) |> rbindlist()
}, .progress = T) |> rbindlist()
# Save raw predictions
preds |> write_parquet(paste0(op_folder, "preds.parquet"))
