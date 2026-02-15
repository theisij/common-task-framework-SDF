# markowitz_ml_testing.R — Test markowitz-ml model
# Phase 1: Unit tests on toy data
# Phase 2: Validation on full output

# Phase 1: Unit tests on toy data ─────────────────────────────────────────────
source("models_R/markowitz-ml/markowitz_ml.R", echo = TRUE)
features  <- arrow::read_parquet("data/interim/ctff_features.parquet")
chars     <- arrow::read_parquet("data/interim/ctff_chars.parquet")
daily_ret <- arrow::read_parquet("data/interim/ctff_daily_ret.parquet")
pf <- main(chars = chars, features = features, daily_ret = daily_ret)

# Check: output has expected columns
stopifnot(all(c("id", "eom", "w") %in% names(pf)))
cat("PASS: output has id, eom, w columns\n")

# Check: no NA weights
stopifnot(!any(is.na(pf$w)))
cat("PASS: no NA weights\n")

# Check: non-zero exposure per month (portfolio is not all zeros)
stopifnot(all(pf[, sum(abs(w)) > 0, by = eom]$V1))
cat("PASS: non-zero exposure per month\n")

cat("\nAll toy-data tests passed!\n")

# Phase 2: Validation on full output ──────────────────────────────────────────
if (FALSE) {
  library(data.table)
  library(ggplot2)
  library(arrow)
  chars <- read_parquet(file.path("data", "raw", "ctff_chars.parquet"),
                        col_select = c("id", "eom", "eom_ret", "ret_exc_lead1m"))
  setnames(chars, old = "ret_exc_lead1m", new = "r")
  pf <- fread(file.path("data", "processed", "markowitz_ml.csv"))
  pf <- chars[pf, on = .(id, eom)]
  if (any(is.na(pf$w))) stop("NA weights found in markowitz_ml output")
  if (any(is.na(pf$r))) stop("NA returns found in markowitz_ml output")

  pf <- pf[!is.na(r), .(
    ret = sum(w * r)
  ), by = .(eom, eom_ret)]
  setorder(pf, eom)
  # Check Sharpe ratio
  pf[, .(
    ret = mean(ret) * 12,
    sd = sd(ret) * sqrt(12)
  )][, sr := ret / sd][]
  # Check cumulative returns
  pf[, cumret := cumsum(ret)]
  pf |>
    ggplot(aes(x = eom, y = cumret)) +
    geom_line() +
    labs(title = "Cumulative Returns of Markowitz-ML Portfolio",
         x = "Date",
         y = "Cumulative Return") +
    theme_minimal()
}
