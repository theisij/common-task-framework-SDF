# performance_stats.R — Out-of-sample performance statistics for CTF portfolios
# Used by: scripts/performance_stats.R (documentation tables and figures)

#' Monthly portfolio excess returns
#'
#' @param pf   data.table with id, eom, w (portfolio weights formed at eom)
#' @param rets data.table with id, eom, r (excess return from eom to the next month end)
#' @return data.table with eom, ret (portfolio excess return over the following month)
pf_returns <- function(pf, rets) {
  check_returns(pf, rets)
  rets[pf, on = .(id, eom)][, .(ret = sum(w * r)), by = eom][order(eom)]
}

#' Stop if any held position lacks a realized return (as validate_portfolio() does)
check_returns <- function(pf, rets) {
  n_miss <- rets[pf[w != 0], on = .(id, eom)][, sum(is.na(r))]
  if (n_miss > 0) stop(sprintf("%d held positions have no realized return", n_miss))
}

#' Maximum drawdown of compounded returns
max_drawdown <- function(ret) {
  wealth <- cumprod(1 + ret)
  max(1 - wealth / cummax(c(1, wealth))[-1])
}

#' Average monthly turnover
#'
#' Turnover in month t is sum_i |w_{i,t} - w~_{i,t}|, where w~ are last month's
#' weights after drifting with realized returns: w~_i = w_{i,t-1} (1 + r_i) / (1 + R_p).
#' Stocks entering or leaving the portfolio count in full.
pf_turnover <- function(pf, rets) {
  check_returns(pf, rets)
  eoms <- sort(unique(pf$eom))
  prev <- rets[pf[w != 0], on = .(id, eom)]
  prev[, w_drift := w * (1 + r) / (1 + sum(w * r)), by = eom]
  prev[, eom := eoms[match(eom, eoms) + 1L]]  # drifted weights apply to the next rebalancing date
  prev <- prev[!is.na(eom), .(id, eom, w_drift)]
  both <- merge(pf[, .(id, eom, w)], prev, by = c("id", "eom"), all = TRUE)
  both <- both[eom > min(eoms)]
  both[is.na(w), w := 0][is.na(w_drift), w_drift := 0]
  both[, .(to = sum(abs(w - w_drift))), by = eom][, mean(to)]
}

#' Summary performance statistics
#'
#' @param pf       data.table with id, eom, w
#' @param rets     data.table with id, eom, r
#' @param vol_scale annualized volatility used for the rescaled drawdown
#' @return one-row data.table
perf_stats <- function(pf, rets, vol_scale = 0.10) {
  ts <- pf_returns(pf, rets)
  mean_ann <- mean(ts$ret) * 12
  sd_ann <- sd(ts$ret) * sqrt(12)
  data.table(
    start = min(ts$eom), end = max(ts$eom), months = nrow(ts),
    mean = mean_ann,
    sd = sd_ann,
    sharpe = mean_ann / sd_ann,
    avg_stocks = pf[, .(n = sum(w != 0)), by = eom][, mean(n)],  # holdings, not rows (Factor-ML keeps zero weights)
    gross_leverage = pf[, sum(abs(w)), by = eom][, mean(V1)],
    turnover = pf_turnover(pf, rets),
    max_dd = max_drawdown(ts$ret),
    max_dd_scaled = max_drawdown(ts$ret * vol_scale / sd_ann)
  )
}
