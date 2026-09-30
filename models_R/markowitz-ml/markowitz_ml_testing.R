# markowitz_ml_testing.R — Test markowitz-ml model
source("utils/R/local_testing.R")

# Phase 1: Toy-data tests ─────────────────────────────────────────────────────
pf <- run_toy_tests("models_R/markowitz-ml/markowitz_ml.R")

# Phase 2: Validation on full output ───────────────────────────────────────────
if (FALSE) {
  validate_portfolio("Markowitz-ML", "data/processed/markowitz_ml.csv")
}