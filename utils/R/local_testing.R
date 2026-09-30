# local_testing.R — Shared testing utilities for CTF models
# Phase 1: run_toy_tests()    — run model on toy data and check output
# Phase 2: validate_portfolio() — validate full output and compute Sharpe ratio

library(data.table)
library(arrow)

#' Run toy-data tests (Phase 1)
#'
#' Sources the model, loads toy data from data/interim/, calls main(),
#' and runs universal checks on the output.
#'
#' @param model_path Path to the model R script (e.g. "models_R/factor-ml/factor_ml.R")
#' @return The portfolio data.table for any additional model-specific checks
run_toy_tests <- function(model_path) {
  source(model_path, echo = TRUE)
  features  <- read_parquet("data/interim/toy_ctff_features.parquet")
  chars     <- read_parquet("data/interim/toy_ctff_chars.parquet")
  daily_ret <- read_parquet("data/interim/toy_ctff_daily_ret.parquet")
  pf <- main(chars = chars, features = features, daily_ret = daily_ret)

  # Check: output has expected columns
  stopifnot(all(c("id", "eom", "w") %in% names(pf)))
  cat("PASS: output has id, eom, w columns\n")

  # Check: no NA weights
  stopifnot(!any(is.na(pf$w)))
  cat("PASS: no NA weights\n")

  # Check: non-zero exposure per month
  stopifnot(all(pf[, sum(abs(w)) > 0, by = eom]$V1))
  cat("PASS: non-zero exposure per month\n")

  cat("\nAll toy-data tests passed!\n")
  return(pf)
}

#' Validate full model output (Phase 2)
#'
#' Loads chars from data/raw/, reads the processed CSV, joins,
#' checks for NAs, computes and prints the annualised Sharpe ratio,
#' and plots cumulative returns.
#'
#' @param model_name Display name for the model (used in plot title)
#' @param csv_path   Path to the processed CSV file
#' @return The portfolio-returns data.table (invisibly)
validate_portfolio <- function(model_name, csv_path) {
  library(ggplot2)

  chars <- read_parquet(file.path("data", "raw", "ctff_chars.parquet"),
                        col_select = c("id", "eom", "eom_ret", "ret_exc_lead1m"))
  chars |> setnames(old = "ret_exc_lead1m", new = "r")

  pf <- fread(csv_path)
  pf <- chars[pf, on = .(id, eom)]
  if (any(is.na(pf$w))) stop(paste("NA weights found in", model_name, "output"))
  if (any(is.na(pf$r))) stop(paste("NA returns found in", model_name, "output"))

  pf <- pf[!is.na(r), .(ret = sum(w * r)), by = .(eom, eom_ret)]
  pf |> setorder(eom)

  # Annualised Sharpe ratio
  sr <- pf[, .(ret = mean(ret) * 12, sd = sd(ret) * sqrt(12))][, sr := ret / sd][]
  print(sr)

  # Cumulative returns plot
  p <- pf[, cumret := cumsum(ret)] |>
    ggplot(aes(x = eom, y = cumret)) +
    geom_line() +
    labs(title = paste("Cumulative Returns of", model_name),
         x = "Date",
         y = "Cumulative Return") +
    theme_minimal()
  print(p)

  invisible(pf)
}
