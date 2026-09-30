# data_prep.R — Shared data preparation utilities
# Used by: factor_ml, minimum_variance, markowitz_ml

prepare_pred_data <- function(data, features, feat_prank, impute, min_obs = NULL) {
  if (feat_prank) {
    data[, (features) := lapply(.SD, as.double), .SDcols = features]
    cat(sprintf("Percentile-ranking %d features...\n", length(features)))
    data[, (features) := lapply(.SD, function(x) {
      non_na <- !is.na(x)
      # Optionally require a minimum number of non-missing observations to rank
      if (!is.null(min_obs) && sum(non_na) < min_obs) return(rep(NA_real_, length(x)))
      is_zero <- non_na & (x == 0)
      x[non_na] <- frank(x[non_na], ties.method = "max") / sum(non_na)
      # Override zeros universally: while 0 may not literally be the lowest
      # value for some variables (e.g., chcsho_12m), it gives XGBoost a
      # consistent way to identify exact zeros, which is likely a special value.
      x[is_zero] <- 0
      x - 0.5
    }), .SDcols = features, by = .(excntry, eom)]
  }
  if (impute) {
    if (feat_prank) {
      setnafill(data, fill = 0, cols = features)
    } else {
      data[, (features) := lapply(.SD, function(x) {
        fifelse(is.na(x), median(x, na.rm = TRUE), x)
      }), .SDcols = features, by = .(excntry, eom)]
    }
  }
  return(data)
}
