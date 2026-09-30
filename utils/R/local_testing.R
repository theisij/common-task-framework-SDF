# local_testing.R — Shared testing utilities for CTF models
# Phase 1: run_toy_tests()    — run the standalone model on toy data: output, determinism, lookahead
# Phase 2: validate_portfolio() — validate full output and compute Sharpe ratio

library(data.table)
library(arrow)

#' Weights agree within the CTF tolerance (docs/ctf_rules.md, Rule 18)
weights_match <- function(a, b, rtol = 1e-5, atol = 1e-8) {
  m <- merge(a, b, by = c("id", "eom"), all = TRUE, suffixes = c("_a", "_b"))
  !anyNA(m) && all(abs(m$w_a - m$w_b) <= atol + rtol * abs(m$w_b))
}

#' Run toy-data tests (Phase 1)
#'
#' Sources the model, loads the toy data from data/interim/ (created by
#' utils/toy_data.R to mimic the CTF validation run), calls main(), and checks:
#' the output contract (Rule 12), determinism (Rule 18: a second run gives the
#' same weights) and no lookahead (Rule 1: rerunning on data with the last test
#' month removed gives the same weights for the remaining months).
#'
#' @param model_path Path to the model script to test; use the *_standalone.R
#'   file, which is what the CTF runs (build it with scripts/build_model.R)
#' @return The portfolio data.table for any additional model-specific checks
run_toy_tests <- function(model_path) {
  source(model_path, echo = TRUE)
  features  <- read_parquet("data/interim/toy_ctff_features.parquet")
  chars     <- as.data.table(read_parquet("data/interim/toy_ctff_chars.parquet"))
  daily_ret <- as.data.table(read_parquet("data/interim/toy_ctff_daily_ret.parquet"))
  pf <- main(chars = copy(chars), features = features, daily_ret = copy(daily_ret))

  # Rule 12: output contract
  stopifnot(identical(names(pf), c("id", "eom", "w")))
  stopifnot(is.integer(pf$id), inherits(pf$eom, "Date"), is.double(pf$w))
  stopifnot(nrow(pf) > 0, !anyNA(pf), !any(duplicated(pf[, .(id, eom)])))
  cat("PASS: output is non-empty id (integer), eom (Date), w (double), no NAs or duplicates\n")
  test_rows <- chars[ctff_test == 1, .(id, eom)]
  test_eoms <- sort(unique(test_rows$eom))
  stopifnot(nrow(test_rows[!pf, on = .(id, eom)]) == 0, nrow(pf[!test_rows, on = .(id, eom)]) == 0)
  cat("PASS: a weight for every ctff_test observation (id, eom), and nothing else\n")

  # Non-zero exposure per month
  stopifnot(all(pf[, sum(abs(w)) > 0, by = eom]$V1))
  cat("PASS: non-zero exposure per month\n")

  # Rule 18: determinism
  pf_again <- main(chars = copy(chars), features = features, daily_ret = copy(daily_ret))
  stopifnot(weights_match(pf, pf_again))
  cat("PASS: deterministic (second run gives the same weights)\n")

  # Rule 1: no lookahead (truncation test)
  cutoff <- test_eoms[length(test_eoms) - 1]
  pf_trunc <- main(chars = chars[eom <= cutoff], features = features,
                   daily_ret = daily_ret[date <= cutoff])
  stopifnot(weights_match(pf[eom <= cutoff], pf_trunc))
  cat(sprintf("PASS: no lookahead (same weights up to %s when later data is removed)\n", cutoff))

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
