# Function -------
pf_mw_fun <- function(dates, ers, covs, vol_ann) {
  dates <- dates + 1 - months(1) - 1# Dates are on eom_ret rather than eom
  dates |> map(function(d) {
    mu <- ers[eom==d][order(id)]
    sigma <- covs[[as.character(d)]]*21
    stopifnot(sum(rownames(sigma)!=mu$id)==0)  # Check that the order is the same
    w <- drop(solve(sigma) %*% mu$pred)
    # Scale to ex-ante variance of 10% annualized
    w <- w * drop(sqrt((vol_ann^2/12) / (t(w) %*% sigma %*% w)))
    # Output
    mu[, .(id, eom, w=w)]
  }, .progress = "markowitz portfolios") |> rbindlist()
} 
# Implement
pf_mw <- val_ends %>% sapply(simplify = F, USE.NAMES = T, function(val_end) {
  print(val_end)
  # Date split
  split <- all_dates %>% date_split(val_end = val_end, 
                                val_years = set$split$val_yrs, 
                                train_start = set$split$train_start, 
                                train_lookback = set$split$train_yrs, 
                                retrain_lookback = set$split$retrain_yrs,
                                test_inc = test_inc, 
                                test_end = set$split$test_end)
  # Implement method
#  split$test |> pf_mw_fun(ers = er_ens, covs = cov_diag, vol_ann = 0.1)  
  pf_dates |> pf_mw_fun(ers = er_ens, covs = cov_diag, vol_ann = 0.1)  
}) |> rbindlist()
# Returns 
pf_mw <- pf_mw |> pf_ret_fun(name = "Markowitz-ML")
