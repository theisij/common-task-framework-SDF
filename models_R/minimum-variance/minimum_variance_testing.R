# minimum_variance_testing.R — Test minimum-variance model
source("utils/R/local_testing.R")

# Phase 1: Toy-data tests ─────────────────────────────────────────────────────
pf <- run_toy_tests("models_R/minimum-variance/minimum_variance.R")

# Model-specific check: weights sum to 1 per month (fully invested)
stopifnot(all(pf[, all.equal(sum(w), 1), by = eom]$V1))
cat("PASS: weights sum to 1 per month\n")

# Phase 2: Validation on full output ───────────────────────────────────────────
if (FALSE) {
  validate_portfolio("Minimum-Variance", "data/processed/minimum_variance.csv")
}
