# Features 
features_file <- paste0("../Data/CTF-data/ctff_features_", set$features, ".parquet") # Select either "../Data/CTF-data/ctff_features_original.parquet" or "../Data/CTF-data/ctff_features_expanded.parquet"
features <- arrow::read_parquet(features_file)$features
# features <- c(features[1:2], "age", "chcsho_1m, "chcsho_3m", "chcsho_6m") # for testing
# X-variables
x_vars <- x_vars_fun(factors = features, ind = set$ind)
# Define storage variables
spec_risk_models <- NULL
# Create variance-covariance matrix
c("us", "dev_ex_us", "em") |> walk(function(region) {
  print(region)
  # Test eoms
  if (region=="us") {
    test_eoms <- test_eoms_us
  } else {
    test_eoms <- test_eoms_other
  }
  # Generate data 
  data <- create_data(
    region = region,
    features = features,
    sample = F
  )
  # Factor characteristics
  factors <- features
  factor_chars <- create_factor_chars(
    chars = data$chars,
    char_into = char_info,
    factors = factors,
    ind = set$ind,
    seed = set$seed
  )
  if (region=="us") {
    # Factor returns and residuals
    if (set$ridge) {
      # Main reason for ridge: to avoid multicollinearity of zero_trades_126d and turnover_126d
      factor_regs <- create_factor_regs_ridge(
        chars = data$chars,
        factor_chars = factor_chars,
        daily = data$daily,
        factors = factors,
        ind = set$ind,
        lambda = set$ridge_lambda
      )
    } else {
      factor_regs <- create_factor_regs(
        chars = data$chars,
        factor_chars = factor_chars,
        daily = data$daily,
        factors = factors,
        ind = set$ind
      )
    }
    
    # Factor variance-covariance matrix
    factor_cov <- create_factor_cov(
      factor_returns = factor_regs$factor_returns,
      cov_set = set$cov_set,
      test_dates = test_eoms
    )
    # Specific risk
    spec_risk <- create_specific_risk(
      factor_res = factor_regs$factor_res,
      cov_set = set$cov_set
    )
    # Specific risk model (follow approach from eq. 5.3 in BARRA USE4 methodology)
    if (set$ridge) {
      spec_risk_models <<- create_specific_risk_models_ridge(  # Global assignment so I can use it in other regions
        factor_chars = factor_chars, 
        spec_risk = spec_risk, 
        factors = factors, 
        ind = set$ind,
        lambda = set$ridge_lambda
      )
    } else {
      spec_risk_models <<- create_specific_risk_models(  # Global assignment so I can use it in other regions
        factor_chars = factor_chars, 
        spec_risk = spec_risk, 
        factors = factors, 
        ind = set$ind
      ) 
    }
    # Save (for use by other regions)
    factor_cov |> saveRDS(paste0(op_folder_overall, "us/factor_cov.RDS"))
    # spec_risk_models |> saveRDS(paste0(op_folder_overall, "us/spec_risk_models.RDS"))
  } else {
    # Load (from US)
    factor_cov <- readRDS(paste0(op_folder_overall, "us/factor_cov.RDS"))
    # spec_risk_models <- readRDS(paste0(op_folder_overall, "us/spec_risk_models.RDS"))
    spec_risk <- NULL
  } 
  # Remove data to save memory
  rm(data)
  # Specific risk predictions 
  spec_risk_preds <- create_spec_risk_preds(
    factor_chars = factor_chars,
    spec_risk_models = spec_risk_models,
    test_dates = test_eoms,
    x_vars = x_vars
  )
  # Specific risk estimates for all stocks
  if (!is.null(spec_risk)) {
    spec_risk_full <- spec_risk[spec_risk_preds, on = .(id, eom)]
    spec_risk_full[is.na(res_vol), res_vol := res_vol_pred]
    spec_risk_full[, res_vol_pred := NULL]
  } else {
    spec_risk_full <- spec_risk_preds[, .(id, eom, res_vol=res_vol_pred)]
  }
  # Save outputs neccesary to create variance-covariance matrix
  factor_chars |> 
    write_parquet(paste0(op_folder_overall, region, "/factor_chars.parquet"))
  spec_risk_full |> 
    write_parquet(paste0(op_folder_overall, region, "/spec_risk_full.parquet"))
}, .progress = T)

