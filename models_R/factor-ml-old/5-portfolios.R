# Build standard long-short portfolios -----------------------------------------
preds <- read_parquet(paste0(op_folder, "preds.parquet"))
# Portfolio
preds[, pf := ceiling(ecdf(pred)(pred)*set$n_pfs), by = .(region, excntry, eom_ret)]
# Portfolio return
pfs <- preds[, .(
  n = .N, 
  ret = mean(ret_exc_lead1m)
), by = .(region, pf, eom_ret)]
# Check that portfolio 1 and 10 are always available
check <- pfs[pf %in% c(1, 10), .N, by = .(region, eom_ret)][order(N)]
if (!all(check$N == 2)) {
  warning("Not all portfolios have correct number of observations (will be modified later)")
  print(check[N != 2])
}

# In some cases, we have less than 10 outputs, which means that we'll get less 
# than 10 portfolios. To handle this, I ensure that everything at/below the 10th 
# percentile and at/above the 90th percentile is in the 1st and 10th portfolios
preds[, pf_ls := case_when(
  pred <= quantile(pred, 0.1) ~ "short",
  pred >= quantile(pred, 0.9) ~ "long",
  TRUE ~ NA
), by = .(region, excntry, eom_ret)]

lms <- preds[!is.na(pf_ls), .(
  ret = mean(ret_exc_lead1m)
), by = .(region, pf_ls, eom_ret)] |>
  pivot_wider(names_from = pf_ls, values_from = ret) |>
  mutate(long_minus_short = long - short) |> 
  arrange(region, eom_ret) |> 
  setDT()

# Save 
lms[, .(region, eom_ret, long_minus_short)] |> 
  write_parquet(file.path("data", "processed", "pf_ml.parquet"))