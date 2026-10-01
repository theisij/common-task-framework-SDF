# Test model on toy data set -----------------------------------------------------------
source("models_R/KNS/kns_pc.R", echo = TRUE)
features  <- arrow::read_parquet("data/interim/toy_ctff_features.parquet")
chars     <- arrow::read_parquet("data/interim/toy_ctff_chars.parquet")
warning("me is hardcoded. need to decide how to handle this in the future (see https://github.com/theisij/common-task-framework-SDF/pull/7#issuecomment-3916900035")
chars[, me := runif(1)]
daily_ret <- arrow::read_parquet("data/interim/toy_ctff_daily_ret.parquet")
pf <- main(chars = chars, features = features, daily_ret = daily_ret)

# Test output from actual run ----------------------------------------------------------
chars <- read_parquet(file.path("data", "raw", "ctff_chars.parquet"),
                      col_select = c("id", "eom", "eom_ret", "ret_exc_lead1m", "div_me"))
chars |> setnames(old = "ret_exc_lead1m", new = "r")
pf <- fread(file.path("data", "processed", "kns_pc.csv"))
pf <- chars[pf, on = .(id, eom)]
if (any(is.na(pf$w))) stop("NA weights found in kns_pc output")
if (any(is.na(pf$r))) stop("NA returns found in kns_pc output")

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
  labs(title = "Cumulative Returns of KNS PC",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal()
