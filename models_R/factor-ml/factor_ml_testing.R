# Test model on toy data set -----------------------------------------------------------
source("models_R/factor-ml/factor_ml.R", echo = TRUE)
features  <- arrow::read_parquet("data/interim/ctff_features.parquet")
chars     <- arrow::read_parquet("data/interim/ctff_chars.parquet")
daily_ret <- arrow::read_parquet("data/interim/ctff_daily_ret.parquet")
pf <- main(chars = chars, features = features, daily_ret = daily_ret)

# Test
stopifnot(all(c(
  pf[, all.equal(sum(w), 0), by = eom]$V1,
  pf[, all.equal(sum(abs(w)), 2), by = eom]$V1
)))

# Test output from actual run ----------------------------------------------------------
chars <- read_parquet(file.path("data", "raw", "ctff_chars.parquet"), 
                      col_select = c("id", "eom", "eom_ret", "ret_exc_lead1m", "div_me"))
chars |> setnames(old = "ret_exc_lead1m", new = "r")
pf <- fread(file.path("data", "processed", "factor_ml.csv"))
pf <- chars[pf, on = .(id, eom)]
if (any(is.na(pf$w))) stop("NA weights found in factor_ml output")
if (any(is.na(pf$r))) stop("NA returns found in factor_ml output")

pf <- pf[!is.na(r), .(
  ret = sum(w * r)
), by = .(eom, eom_ret)]
pf |> setorder(eom)
# Check Sharpe ratio
pf[, .(
  ret = mean(ret)*12,
  sd = sd(ret)*sqrt(12)
)][, sr := ret / sd][]
# Check cumulative returns
pf[, cumret := cumsum(ret)] |> 
  ggplot(aes(x = eom, y = cumret)) +
  geom_line() +
  labs(title = "Cumulative Returns of Factor ML",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal()