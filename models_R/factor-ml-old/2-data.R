# Load data --------------------------------------------------------------------
# Features 
feature_path <- file.path("data", "raw", "ctff_features.parquet")
features <- arrow::read_parquet(feature_path)$features

# Data
data_path <- file.path("data", "raw", "ctff_chars.parquet")
pred_x <- read_parquet(file = data_path) |> setDT()

# Prepare data -----------------------------------------------------------------
pred_x <- pred_x |> 
  prepare_pred_data(
    features = features, 
    feat_prank = T, 
    impute = T
  )
