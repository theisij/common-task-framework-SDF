source("covariance-estimation/1-setup.R")

op_folder <- "covariance-estimation/Generated/2025-01-28/original/"

factor_returns <- paste0(op_folder, "us/factor_returns.parquet") |> arrow::read_parquet()
