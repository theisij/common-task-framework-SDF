# minimum_variance_testing.R — Test minimum-variance model
# Phase 1: Unit tests on toy data
# Phase 2: Validation on full output

# Phase 1: Unit tests on toy data ─────────────────────────────────────────────
source("models_R/minimum-variance/minimum_variance.R", echo = TRUE)
features  <- arrow::read_parquet("data/interim/ctff_features.parquet")
chars     <- arrow::read_parquet("data/interim/ctff_chars.parquet")
daily_ret <- arrow::read_parquet("data/interim/ctff_daily_ret.parquet")
pf <- main(chars = chars, features = features, daily_ret = daily_ret)

# Check: weights sum to 1 per month (fully invested)
stopifnot(all(
  pf[, all.equal(sum(w), 1), by = eom]$V1
))
cat("PASS: weights sum to 1 per month\n")

# Check: output has expected columns
stopifnot(all(c("id", "eom", "w") %in% names(pf)))
cat("PASS: output has id, eom, w columns\n")

# Check: no NA weights
stopifnot(!any(is.na(pf$w)))
cat("PASS: no NA weights\n")

cat("\nAll toy-data tests passed!\n")

# Phase 2: Validation on full output ──────────────────────────────────────────
if (FALSE) {
  library(data.table)
  library(ggplot2)
  library(arrow)
  chars <- read_parquet(file.path("data", "raw", "ctff_chars.parquet"),
                        col_select = c("id", "eom", "eom_ret", "ret_exc_lead1m"))
  setnames(chars, old = "ret_exc_lead1m", new = "r")
  pf <- fread(file.path("data", "processed", "minimum_variance.csv"))
  pf <- chars[pf, on = .(id, eom)]
  if (any(is.na(pf$w))) stop("NA weights found in minimum_variance output")
  if (any(is.na(pf$r))) stop("NA returns found in minimum_variance output")

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
    labs(title = "Cumulative Returns of Minimum-Variance Portfolio",
         x = "Date",
         y = "Cumulative Return") +
    theme_minimal()
}
