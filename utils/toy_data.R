# create_toy_data.R — Subsample real CTF data into a tiny toy dataset
# for quick end-to-end testing of factor_ml.R
#
# Usage: source("utils/toy_data.R")

library(arrow)
library(tidyverse)
library(data.table)

# ── Config ──────────────────────────────────────────────────────────────────
N_FEATURES    <- 10    # Number of features to sample
N_TEST_MONTHS <- 2     # Number of test months to keep
N_STOCKS      <- 50    # Target number of stocks per month
TRAIN_YEARS   <- 10    # Must match factor_ml.R setting
SEED          <- 1
OUT_DIR       <- file.path("data", "interim")

set.seed(SEED)

# ── Step 1: Read features and sample a subset ───────────────────────────────
cat("Reading features...\n")
features_dt <- as.data.table(read_parquet("data/raw/ctff_features.parquet"))
all_features <- features_dt$features
sampled_features <- sort(sample(all_features, N_FEATURES))
cat(sprintf("  Sampled %d features: %s\n", N_FEATURES, paste(sampled_features, collapse = ", ")))

# ── Step 2: Read chars (only needed columns to avoid loading full 2.4 GB) ──
cat("Reading chars (selected columns only)...\n")
required_cols <- c("id", "eom", "eom_ret", "excntry", "ctff_test", "ret_exc_lead1m", "sic")
cols_to_read <- unique(c(required_cols, sampled_features))
chars <- as.data.table(read_parquet("data/raw/ctff_chars.parquet",
                                     col_select = all_of(cols_to_read)))
cat(sprintf("  Full chars: %s rows x %s cols\n",
            format(nrow(chars), big.mark = ","), ncol(chars)))

# ── Step 3: Select date range ───────────────────────────────────────────────
# Take the last N_TEST_MONTHS test dates
all_test_dates <- chars[ctff_test == 1, sort(unique(eom_ret))]
selected_test_dates <- tail(all_test_dates, N_TEST_MONTHS)
cat(sprintf("  Selected test dates: %s\n",
            paste(selected_test_dates, collapse = ", ")))

# Training window: TRAIN_YEARS before the earliest test date
earliest_test <- min(selected_test_dates)
# Mirror the date arithmetic from factor_ml.R fold_dates_fun:
#   train_last = test_date + 1 - months(1)
#   train_first = train_last - years(train_years) + months(1) - 1
# For the earliest test date, this gives the earliest training date needed
train_start <- as.Date(earliest_test) + 1
train_start <- train_start - months(1) - years(TRAIN_YEARS) + months(1) - 1
cat(sprintf("  Training window starts: %s\n", train_start))

# Filter to date range
chars <- chars[eom_ret >= train_start & eom_ret <= max(selected_test_dates)]

# Update ctff_test: only the selected test dates are test months
chars[, ctff_test := as.integer(eom_ret %in% selected_test_dates)]
cat(sprintf("  After date filter: %s rows\n", format(nrow(chars), big.mark = ",")))

# ── Step 4: Select stocks ──────────────────────────────────────────────────
# Pick 2-3 top countries by row count (exercises excntry grouping)
country_counts <- chars[, .N, by = excntry][order(-N)]
top_countries <- head(country_counts$excntry, 3)
chars <- chars[excntry %in% top_countries]
cat(sprintf("  Countries kept: %s\n", paste(top_countries, collapse = ", ")))

# Pick stocks with good temporal coverage
all_months <- sort(unique(chars$eom_ret))
n_months <- length(all_months)
stock_coverage <- chars[, .(coverage = .N / n_months), by = id]

# Select stocks that appear in at least 80% of months
good_stocks <- stock_coverage[coverage >= 0.8, id]
cat(sprintf("  Stocks with >= 80%% coverage: %d\n", length(good_stocks)))

if (length(good_stocks) > N_STOCKS) {
  selected_stocks <- sample(good_stocks, N_STOCKS)
} else if (length(good_stocks) >= 30) {
  # If fewer than target but still reasonable, use what we have
  selected_stocks <- good_stocks
  cat(sprintf("  Warning: only %d stocks with good coverage, using all\n",
              length(good_stocks)))
} else {
  # Fallback: take top N_STOCKS by coverage
  selected_stocks <- stock_coverage[order(-coverage)][1:min(N_STOCKS, nrow(stock_coverage)), id]
  cat(sprintf("  Warning: falling back to top %d stocks by coverage\n",
              length(selected_stocks)))
}

chars <- chars[id %in% selected_stocks]
cat(sprintf("  After stock filter: %s rows, %d unique stocks\n",
            format(nrow(chars), big.mark = ","),
            uniqueN(chars$id)))

# ── Step 5: Create daily_ret from real data ──────────────────────────────────
# Filter real daily returns to selected stock IDs and date range
cat("Reading daily returns (filtered)...\n")
daily_ret <- as.data.table(read_parquet("data/raw/ctff_daily_ret.parquet"))
daily_ret <- daily_ret[id %in% selected_stocks & date >= train_start & date <= max(selected_test_dates)]
cat(sprintf("  Daily returns: %s rows, %d unique stocks\n",
            format(nrow(daily_ret), big.mark = ","),
            uniqueN(daily_ret$id)))

# ── Step 6: Save parquet files ──────────────────────────────────────────────
cat("\nSaving parquet files...\n")

# Features
features_out <- data.table(features = sampled_features)
write_parquet(features_out, file.path(OUT_DIR, "toy_ctff_features.parquet"))
cat(sprintf("  ctff_features.parquet: %d rows\n", nrow(features_out)))

# Chars
write_parquet(chars, file.path(OUT_DIR, "toy_ctff_chars.parquet"))
cat(sprintf("  ctff_chars.parquet: %s rows x %d cols\n",
            format(nrow(chars), big.mark = ","), ncol(chars)))

# Daily returns
write_parquet(daily_ret, file.path(OUT_DIR, "toy_ctff_daily_ret.parquet"))
cat(sprintf("  ctff_daily_ret.parquet: %s rows\n", format(nrow(daily_ret), big.mark = ",")))

# ── Step 7: Print summary ──────────────────────────────────────────────────
cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  Toy dataset created successfully!\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("  Location:       %s/\n", OUT_DIR))
cat(sprintf("  Features:       %d\n", N_FEATURES))
cat(sprintf("  Stocks:         %d\n", uniqueN(chars$id)))
cat(sprintf("  Total months:   %d\n", uniqueN(chars$eom_ret)))
cat(sprintf("  Test months:    %d\n", N_TEST_MONTHS))
cat(sprintf("  Train months:   ~%d\n", uniqueN(chars$eom_ret) - N_TEST_MONTHS))
cat(sprintf("  Countries:      %s\n", paste(top_countries, collapse = ", ")))
cat(sprintf("  Total rows:     %s\n", format(nrow(chars), big.mark = ",")))
cat("\n")
cat("  Usage:\n")
cat("    source(\"models_R/factor-ml/factor_ml.R\")\n")
cat("    features  <- arrow::read_parquet(\"data/interim/toy_ctff_features.parquet\")\n")
cat("    chars     <- arrow::read_parquet(\"data/interim/toy_ctff_chars.parquet\")\n")
cat("    daily_ret <- arrow::read_parquet(\"data/interim/toy_ctff_daily_ret.parquet\")\n")
cat("    pf <- main(chars = chars, features = features, daily_ret = daily_ret)\n")
cat("═══════════════════════════════════════════════════════════\n")
