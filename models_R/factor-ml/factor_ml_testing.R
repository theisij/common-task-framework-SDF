# factor_ml_testing.R — Test factor-ml model
source("utils/R/local_testing.R")

# Phase 1: Toy-data tests ─────────────────────────────────────────────────────
pf <- run_toy_tests("models_R/factor-ml/factor_ml.R")

# Model-specific checks: weights sum to 0, absolute weights sum to 2
stopifnot(all(c(
  pf[, all.equal(sum(w), 0), by = eom]$V1,
  pf[, all.equal(sum(abs(w)), 2), by = eom]$V1
)))
cat("PASS: weights sum to 0 and abs(weights) sum to 2 per month\n")

# Phase 2: Validation on full output ───────────────────────────────────────────
if (FALSE) {
  validate_portfolio("Factor-ML", "data/processed/factor_ml.csv")
}
