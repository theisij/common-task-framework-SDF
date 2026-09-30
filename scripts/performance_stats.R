# performance_stats.R — Write performance tables and figures for model documentation
#
# Reads each model's weights from data/processed/ and the realized returns from
# data/raw/ctff_chars.parquet, and writes to documentation/{model}/:
#   performance_stats.md    — table included by the model's .qmd
#   cumulative_returns.pdf  — figure included by the model's .qmd
#
# Usage: Rscript scripts/performance_stats.R [model ...]   (default: all models below)

library(arrow)
library(data.table)
library(ggplot2)
library(lubridate)
source("utils/R/performance_stats.R")

models <- list(
  "factor-ml"        = list(name = "Factor-ML",        csv = "data/processed/factor_ml.csv"),
  "minimum-variance" = list(name = "Minimum Variance", csv = "data/processed/minimum_variance.csv"),
  "markowitz-ml"     = list(name = "Markowitz-ML",     csv = "data/processed/markowitz_ml.csv")
)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) models <- models[args]

rets <- read_parquet("data/raw/ctff_chars.parquet",
                     col_select = c("id", "eom", "ret_exc_lead1m", "ctff_test")) |> setDT()
rets <- rets[ctff_test == 1, .(id, eom, r = ret_exc_lead1m)]

pct <- function(x) sprintf("%.1f%%", 100 * x)
num <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")

for (m in names(models)) {
  cat("Computing performance statistics for", m, "\n")
  pf <- fread(models[[m]]$csv)[, eom := as.Date(eom)]
  s <- perf_stats(pf, rets)
  out_dir <- file.path("documentation", m)

  # Table (Markdown, included by the .qmd)
  tbl <- c(
    "| Statistic | Value |",
    "|:--|--:|",
    sprintf("| Test period (portfolio formation dates) | %s to %s (%d months) |",
            format(s$start, "%b %Y"), format(s$end, "%b %Y"), s$months),
    sprintf("| Average return (annualized) | %s |", pct(s$mean)),
    sprintf("| Standard deviation (annualized) | %s |", pct(s$sd)),
    sprintf("| Sharpe ratio (annualized) | %s |", num(s$sharpe)),
    sprintf("| Average number of stocks | %s |", num(s$avg_stocks, 0)),
    sprintf("| Gross leverage (average $\\sum_i \\lvert w_{i,t} \\rvert$) | %s |", num(s$gross_leverage)),
    sprintf("| Turnover (average monthly) | %s |", pct(s$turnover)),
    sprintf("| Maximum drawdown | %s |", pct(s$max_dd)),
    sprintf("| Maximum drawdown, scaled to 10%% volatility | %s |", pct(s$max_dd_scaled))
  )
  writeLines(tbl, file.path(out_dir, "performance_stats.md"))

  # Cumulative returns figure
  ts <- pf_returns(pf, rets)
  ts[, eom_ret := ceiling_date(eom + 1, "month") - 1]  # return is realized by the next month end
  p <- ggplot(ts, aes(eom_ret, cumsum(ret))) +
    geom_line(linewidth = 0.4) +
    labs(x = NULL, y = "Cumulative excess return\n(sum of monthly returns)") +
    theme_bw(base_size = 10)
  ggsave(file.path(out_dir, "cumulative_returns.pdf"), p, width = 6.5, height = 3)
  print(s)
}
