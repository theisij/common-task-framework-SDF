# Generate setup and settings --------------------------------------------------
library(WoodburyMatrix)

# Test Dates
test_eoms_us <- seq.Date(
  from = as.Date("1980-01-01"), 
  to = as.Date("2023-12-01"), 
  by = "month"
)-1
test_eoms_dev <- seq.Date(
  from = as.Date("1990-01-01"), 
  to = as.Date("2023-12-01"), 
  by = "month"
)-1
test_eoms_em <- seq.Date(
  from = as.Date("1994-01-01"), 
  to = as.Date("2023-12-01"), 
  by = "month"
)-1

# Get predictions --------------------------------------------------------------
pred_folder <- paste0("return-prediction/Generated/XGB/2025-02-03/", set$features, "/")

c("us", "dev_ex_us", "em") |> walk(function(x) {
  print(paste0("Creating portfolio for ", x))
  # Test dates 
  if (x=="us") {
    test_eoms <- test_eoms_us
  } 
  if (x=="dev_ex_us") {
    test_eoms <- test_eoms_dev
  }
  if (x=="em") {
    test_eoms <- test_eoms_em
  }
  # Returns 
  chars <- read_parquet(
    file = paste0("../Data/CTF-data/", x, "/ctff_chars.parquet"),
    col_select = all_of(c("excntry", "id", "eom", "eom_ret", "ret_exc_lead1m")))
  
  # Covariance information
  factor_cov <- readRDS(paste0(op_folder_overall, "us/factor_cov.RDS"))
  spec_risk <- read_parquet(paste0(op_folder_overall, x, "/spec_risk_full.parquet")) |> 
    setDT()
  factor_chars <- read_parquet(paste0(op_folder_overall, x, "/factor_chars.parquet")) |> 
    setDT()
  preds <- read_parquet(paste0(pred_folder, "/preds.parquet")) |> filter(region==x)
  # Add specific risk
  factor_chars <- spec_risk[factor_chars, on = .(id, eom)]
  factor_chars[, res_vol := res_vol*sqrt(21)]
  # Add ER
  factor_chars <- preds[, .(id, eom, eom_ret, pred, ret_exc_lead1m)][factor_chars, on = .(id, eom)]
  
  # Portfolios
  pfs <- test_eoms |> map(function(d) {
    # Extract inputs
    factor_cov_sub <- factor_cov[[as.character(d)]]*21
    factor_chars_sub <- factor_chars[eom == d]
    # Factor covariance matrix
    fct_cov <- factor_cov[[as.character(d)]]
    # Factor chars 
    factor_char_sub <- factor_chars[eom == as.Date(d)]
    # Create X
    factor_char_sub %>% setorder(id)  # Important to align ids across different datasets
    X <- factor_char_sub[, str_remove_all(colnames(fct_cov), "`"), with=F] %>% as.matrix() # Using colnames(fct_cov) is important so char and factor is aligned across x and fct_cov
    # Stock covariance matrix
    D_inv <- diag(1/factor_char_sub$res_vol^2)
    # Create portfolios
    n <- nrow(factor_char_sub)
    ones <- matrix(1, n, 1)
    u <- factor_char_sub$pred
    # Force symmetry (but always check that difference is tiny)
    fct_cov_inv <- solve(fct_cov)
    if (!isSymmetric(fct_cov_inv)) {
      print(paste0("Had to force symmetric for ", x, "-", set$features, "-", d))
      check <- all.equal(fct_cov_inv, t(fct_cov_inv)) # Check for near symmetry (so error is just numerical)
      if (!check) {
        stop("fct_cov_inv is not symmetric")
      } else {
        fct_cov_inv <- forceSymmetric(fct_cov_inv)
      }
    }
    W <- WoodburyMatrix(
      A = D_inv,
      B = fct_cov_inv,
      X = X,
      symmetric = T
    )
    markowitz_w <- drop(WoodburyMatrix::solve(W, u))
    min_var_w <- drop(as.matrix(WoodburyMatrix::solve(W, ones)))
    min_var_w <- min_var_w/sum(min_var_w)
    op <- factor_char_sub[, .(id, eom, eom_ret, ret_exc_lead1m, markowitz_w, min_var_w)]
    # Added EPO method
    if (TRUE) {
      # Stock covariance matrix
      D <- diag(factor_char_sub$res_vol^2)
      var_cov_sub <- X %*% fct_cov %*% t(X) + D
      # Stock covariance matrix adjusted
      vol_sub <- sqrt(diag(var_cov_sub))
      cor_sub <- var_cov_sub / (vol_sub %*% t(vol_sub))
      s <- 0.5
      cor_new <- cor_sub * (1 - s) + s * diag(nrow(cor_sub)) 
      var_cov_new <- diag(vol_sub) %*% cor_new %*% diag(vol_sub)
      # Create portfolios
      epo_w <- solve(var_cov_new) %*% u
      op$epo_w <- epo_w
    }
    # Output
    return(op)
  }, .progress = T) |> rbindlist()
  
  # Save 
  pfs |> write_parquet(paste0(op_folder_overall, "pfs_", x, ".parquet"))
}, .progress = T)

# Aggregate portfolios
pfs <- c("us", "dev_ex_us", "em") |> map(function(x) {
  read_parquet(paste0(op_folder_overall, "pfs_", x, ".parquet")) |> mutate(region = x)
}) |> rbindlist()

pfs_ts <- pfs[, .(
  markowitz = sum(markowitz_w*ret_exc_lead1m),
  min_var = sum(min_var_w*ret_exc_lead1m),
  epo = sum(epo_w*ret_exc_lead1m)
), by = .(region, eom_ret)][order(region, eom_ret)]

pfs_ts[, .(region, eom_ret, markowitz)] |> write_parquet(paste0(op_folder_overall, "pf_epo.parquet"))
pfs_ts[, .(region, eom_ret, markowitz)] |> write_parquet(paste0(op_folder_overall, "pf_markowitz.parquet"))
pfs_ts[, .(region, eom_ret, min_var)]  |> write_parquet(paste0(op_folder_overall, "pf_min_var.parquet"))

pfs_ts |> 
  pivot_longer(c(markowitz, min_var, epo)) |> 
  group_by(region, name) |>
  summarise(
    start = min(eom_ret),
    end = max(eom_ret),
    ret = mean(value)*12,
    sd = sd(value)*sqrt(12),
    sr = ret/sd
  )
