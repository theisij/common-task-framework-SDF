# Prepare prediction data -------------
prepare_pred_data <- function(data, features, feat_prank, impute) {
  # Convert to Percentile Ranks
  if (feat_prank) {
    data[, (features) := lapply(.SD, as.double), .SDcols = features]  # Convert feature columns to double to avoid loosing precision 
    for(f in features) {
      if (match(f, features) %% 10 == 0) print(paste0("Feature ", match(f, features), " out of ", length(features)))
      # Check for zeros
      data[, zero := (get(f) == 0)]
      # Require at least 10 non-missing observations to rank. This is neccesary 
      # to estimate the associated factor return in factor_regs(). If this 
      # requirement is not there, we get NAs because we lack sufficient variation 
      # In the factor characterisic
      data[, n := sum(!is.na(get(f))), by = .(excntry, eom)]
      data[n<10, (f) := NA_real_][, n := NULL] 
      # Rank
      data[!is.na(get(f)), (f) := ecdf(get(f))(get(f)), by = .(excntry, eom)] # Didn't have by statement before!!
      # Set exact zeros to 0 (ecdf always returns >0)
      data[zero == T, (f) := 0][, zero := NULL] 
      # Subtract -0.5
      data[, (f) := get(f)-0.5]
    }
  }
  # Impute
  if (impute) {
    if (feat_prank) {
      data[, (features) := lapply(.SD, function(x) if_else(is.na(x), 0, x)), .SDcols=features]
    } else {
      data[, (features) := lapply(.SD, function(x) if_else(is.na(x), median(x, na.rm=T), x)), .SDcols=features, by=.(excntry, eom)]
    }
  }
  return(data)
}

# Fama-French 12 industry classification-----------------
ff12_class <- function(sic) {
  case_when(
    sic %in% c(100:999, 2000:2399, 2700:2749, 2770:2799, 3100:3199, 3940:3989) ~ "NoDur",
    sic %in% c(2500:2519, 3630:3659, 3710:3711, 3714, 3716, 3750:3751, 3792, 3900:3939, 3990:3999) ~ "Durbl",
    sic %in% c(2520:2589, 2600:2699, 2750:2769, 3000:3099, 3200:3569, 3580:3629,
               3700:3709, 3712:3713, 3715, 3717:3749, 3752:3791, 3793:3799, 3830:3839, 3860:3899) ~ "Manuf",
    sic %in% c(1200:1399, 2900:2999) ~ "Enrgy",
    sic %in% c(2800:2829, 2840:2899) ~ "Chems",
    sic %in% c(3570:3579, 3660:3692, 3694:3699, 3810:3829, 7370:7379) ~ "BusEq",
    sic %in% c(4800:4899) ~ "Telcm",
    sic %in% c(4900:4949) ~ "Utils",
    sic %in% c(5000:5999, 7200:7299, 7600:7699) ~ "Shops",
    sic %in% c(2830:2839, 3693, 3840:3859, 8000:8099) ~ "Hlth",
    sic %in% c(6000:6999) ~ "Money",
    TRUE ~ "Other"
  )
}

# Covariance functions ---------------------------------------------------------
create_factor_chars <- function(chars, char_into, factors, ind, seed) {
  # Base data set
  data <- chars[, c("id", "eom", "eom_ret", "ff12", factors), with=F]
  # If all factors are zero, create a random factor
  set.seed(seed)
  data[, (factors) := lapply(.SD, function(x) {
    check <- (round(sd(x), 6)==0)
    if (check) {
      x <- x + rnorm(length(x))
    } 
    return(x)
  }), .SDcols=factors, by = eom]
  # Standardize factors
  data[, (factors) := lapply(.SD, function(x) (x-mean(x))/sd(x)), by=eom, .SDcols=factors]  
  # Add Industry/market dummies
  if (ind) {
    # Create industry dummies
    industries <- sort(unique(data$ff12))
    for (ind in industries) {
      data[, (ind) := as.integer(ff12==ind)]
    }
  } else {
    # Add market factor
    data[, mkt := 1]
  }
  # Output
  return(data)
}

# Factor regressions -----------------------------------------------------------
create_factor_regs <- function(chars, factor_chars, daily, factors, ind) {
  # Add chars to daily data
  data <- daily[date>=min(factor_chars$eom) & id %in% unique(chars$id)][
    , .(id, date, ret_exc, "eom_ret"=eom)]
  factor_chars_d <- factor_chars[data, on = .(id, eom_ret), nomatch=0L]
  # Factor return formula
  form <- factor_formula(y = "ret_exc", factors = factors, ind = ind)
  # Factor returns
  fct_regs <- factor_chars_d |> 
    group_by(date) |> 
    nest() %>%
    mutate(
      fit = data %>% map(~lm(form, data=.x)),
      res = fit %>% map(residuals),
      tidied = fit %>% map(broom::tidy)
    ) 
  # Factor returns
  factor_returns <- fct_regs |>  
    unnest(tidied) |> 
    select(date, term, estimate) |> 
    pivot_wider(names_from = term, values_from = estimate) |> 
    arrange(date) |> 
    setDT()
  # Factor residuals
  factor_res <- fct_regs %>%
    mutate(id = data %>% map(~.x$id)) %>% 
    select(id, date, res) %>% 
    unnest(c(id, res)) %>%
    arrange(id, date) %>%
    setDT()
  # Output
  list(factor_returns = factor_returns, factor_res = factor_res)
}
# Factor regressions (ridge) -----------------------------------------------------------
create_factor_regs_ridge <- function(chars, factor_chars, daily, factors, ind, lambda) {
  # Add chars to daily data
  data <- daily[date>=min(factor_chars$eom) & id %in% unique(chars$id)][
    , .(id, date, ret_exc, "eom_ret"=eom)]
  factor_chars_d <- factor_chars[data, on = .(id, eom_ret), nomatch=0L]
  # Factor return formula
  form <- factor_formula(y = "ret_exc", factors = factors, ind = ind)
  x_vars <- form |> str_remove("ret_exc~-1\\+") |> str_remove_all("`")
  x_vars <- strsplit(x_vars, "\\+")[[1]]
  # Factor returns
  days <- factor_chars_d$date |> unique() |> sort()
  fct_regs <- days |> map(function(d) {
    sub <- factor_chars_d[date==d]
    X <- model.matrix(as.formula(form), data = sub)
    y <- sub$ret_exc
    fit_ridge <- glmnet(X, y, alpha=0, lambda=lambda, standardize=F, intercept=F)
    # Check ridge penalty
    if (FALSE) {
      # Check this date for a crazy lm result: d <- as.Date("2011-11-03")
      # Check difference relative to lm
      op <- c(0, 10^(-6:-2)) |> map(function(l) {
        glmnet(X, y, alpha=0, lambda=l, standardize=F, intercept=F) |> 
          broom::tidy() |> 
          mutate(l = paste0("ridge_", l))
      }) |> bind_rows()
      
      op <- op |> 
        select(term, estimate, l) |> 
        rbind(
          lm(form, data=sub) |> broom::tidy() |> mutate(l="lm") |> select(term, estimate, l)
        )  
      # Seems like l=10^-4 is just enough to regularize without massively changing anything
      op |> 
        group_by(term) |>
        mutate(sort_var = estimate[l=="ridge_0"]) |> 
        filter(l!="lm") |>
        ggplot(aes(reorder(term, sort_var), estimate, colour=l, group=l)) +
        geom_line() +
        coord_flip() 
    }
    # Factor returns
    factor_returns <- fit_ridge |> 
      broom::tidy() |> 
      mutate(date = d) |> 
      select(date, term, estimate) |> 
      pivot_wider(names_from = term, values_from = estimate) |> 
      setDT()
    # Factor residuals
    y_pred <- predict(fit_ridge, X, s = lambda)  # Use the same lambda as in the model
    residuals <- y - y_pred
    factor_res <- data.table(id = sub$id, date = d, res = drop(residuals))
    # Output
    list(factor_returns = factor_returns, factor_res = factor_res)
  }, .progress = "   Factor regression by date")
  # Combine
  factor_returns <- fct_regs |> 
    map("factor_returns") |> 
    rbindlist()
  factor_res <- fct_regs |>
    map("factor_res") |>
    rbindlist()
  # Output
  list(factor_returns = factor_returns, factor_res = factor_res)
}
# Factor variance-covariance matrix --------------------------------------------
create_factor_cov <- function(factor_returns, cov_set, test_dates) {
  # Factor Risk -----------------------------
  fct_dates <- sort(factor_returns$date)
  w_cor <- (0.5^(1/cov_set$hl_cor))^(cov_set$obs:1)
  w_var <- (0.5^(1/cov_set$hl_var))^(cov_set$obs:1)
  
  factor_cov_est <- test_dates |> map(function(d) {
    first_obs <- min(tail(fct_dates[fct_dates <= d], cov_set$obs))
    cov_data <- factor_returns[date >= first_obs & date <= d]
    t <- nrow(cov_data)
    if (t < cov_set$obs-30) warning("INSUFFICIENT NUMBER OF OBSERVATIONS!!") # Only an issue with the first calc_date
    cov_est <- cov_data %>% select(-date) %>% cov.wt(wt = tail(w_cor, t), cor=T, center=T, method = "unbiased")
    cor_est <- cov_est$cor
    var_est <- cov_data %>% select(-date) %>% cov.wt(wt = tail(w_var, t), cor=F, center=T, method = "unbiased") # inefficient solution but super fast with few factors
    sd_diag <- diag(sqrt(diag(var_est$cov)), ncol = ncol(cor_est))
    # Prepare cov
    cov_est <- sd_diag %*% cor_est %*% sd_diag
    rownames(cov_est) <- colnames(cov_est) <- colnames(cor_est)
    # Output
    return(cov_est)
  }, .progress = "   Factor covariance estimates by date")
  names(factor_cov_est) <- as.character(test_dates)
  # Output
  return(factor_cov_est)
}

# Specific risk ----------------------------------------------------------------
create_specific_risk <- function(factor_res, cov_set) {
  # Stock specific return variance
  data <- copy(factor_res)
  data |> setorder(id, date)
  # EWMA variance
  data[, res_vol := ewma_c(res, lambda = 0.5^(1/cov_set$hl_stock_var), start = cov_set$initial_var_obs), by = id] # Lambda=exp(log(0.5)/half_life)
  # Require a minimum amount of non-mi
  fct_dates <- sort(unique(factor_res$date))
  # Require that the observation at least 200 non-missing observations out of the last 252 trading days
  td_range <- data.table(date=fct_dates, td_252d = lag(fct_dates, 252))
  data <- td_range[data, on = "date"]
  data[, date_200d := lag(date, 200), by = id]
  data <- data[date_200d>=td_252d & !is.na(res_vol)][, .(id, date, res_vol)]
  # Extract specific risk by month end
  data[, eom_ret := date %>% ceiling_date(unit="month")-1]
  data[, max_date := max(date), by = .(id, eom_ret)]
  data <- data[date==max_date, .(id, "eom"=eom_ret, res_vol)]
  # Output
  return(data)
}

# Create specific risk model ---------------------------------------------------
# (follow approach from eq. 5.3 in BARRA USE4 methodology)
create_specific_risk_models <- function(factor_chars, spec_risk, factors, ind) {
  # Create data
  data <- spec_risk[factor_chars, on = .(id, eom)]
  data <- data[!is.na(res_vol)]
  data[, res_vol_log := log(res_vol)]
  # Create formula
  form <- factor_formula(y = "res_vol_log", factors = factors, ind = ind)
  # Create models
  models <- data |> 
    group_by(eom) |> 
    nest() %>%
    mutate(
      fit = data %>% map(~lm(form, data=.x))
    ) |> 
    select(eom, fit) 
  # Output
  return(models)
}

# (follow approach from eq. 5.3 in BARRA USE4 methodology but with ridge)
create_specific_risk_models_ridge <- function(factor_chars, spec_risk, factors, ind, lambda) {
  # Create data
  data <- spec_risk[factor_chars, on = .(id, eom)]
  data <- data[!is.na(res_vol)]
  data[, res_vol_log := log(res_vol)]
  # Create formula and extract x-vars
  form <- factor_formula(y = "res_vol_log", factors = factors, ind = ind)
  x_vars <- form |> str_remove("res_vol_log~-1\\+") |> str_remove_all("`")
  x_vars <- strsplit(x_vars, "\\+")[[1]]
  # Factor returns
  eoms <- data$eom |> unique() |> sort()
  models <- eoms |> map(function(m) {
    sub <- data[eom==m]
    X <- model.matrix(as.formula(form), data = sub)
    y <- sub$res_vol_log
    fit_ridge <- glmnet(X, y, alpha=0, lambda=lambda, standardize=F, intercept=F)
    # Check ridge penalty
    if (FALSE) {
      # Check difference relative to lm
      op <- c(0, 10^(-6:-2)) |> map(function(l) {
        glmnet(X, y, alpha=0, lambda=l, standardize=F, intercept=F) |> 
          broom::tidy() |> 
          mutate(l = paste0("ridge_", l))
      }) |> bind_rows()
      
      op <- op |> 
        select(term, estimate, l) |> 
        rbind(
          lm(form, data=sub) |> broom::tidy() |> mutate(l="lm") |> select(term, estimate, l)
        )  
      # Seems like l=10^-4 is just enough to regularize without massively changing anything
      op |> 
        group_by(term) |>
        mutate(sort_var = estimate[l=="ridge_0"]) |> 
        mutate(
          type = if_else(l=="lm", "lm", "ridge"),
          param = if_else(term %in% x_vars[1:12], "Industry intercepts", "Factor parameter")
        ) |> 
        # filter(l!="lm") |>
        ggplot(aes(reorder(term, sort_var), estimate, colour=l, group=l)) +
        geom_line() +
        coord_flip() +
        facet_wrap(type~param, scales = "free")
    }
    # Output
    tibble(eom = m, fit = list(fit_ridge))
  }, .progress = "Spec risk models") |> bind_rows()
  # Output
  return(models)
}

# Specific risk predictions ----------------------------------------------------
create_spec_risk_preds <- function(factor_chars, spec_risk_models, test_dates, x_vars) {
  # Predictions
  preds_full <- test_dates |> map(function(d) {
    data <- factor_chars[eom==d]
    model <- spec_risk_models |> filter(eom==d) |> pull(fit)
    preds <- predict(model[[1]], as.matrix(data[, x_vars, with=F]))
    data[, .(id, eom, res_vol_pred=drop(exp(preds)))]
  }, .progress = T) |> rbindlist()
  # Output
  return(preds_full)
}

# Full variance-covariance matrix ----------------------------------------------
create_var_cov <- function(factor_chars, factor_cov, spec_risk_full, test_dates) {
  # Add to char data
  char_data <- spec_risk_full[factor_chars, on = .(id, eom)]
  # Create full variance-covariance matrix
  var_cov <- as.character(test_dates) |> map(function(d) {
    # Factor covariance matrix
    fct_cov <- factor_cov[[d]]
    # Factor chars 
    factor_char_sub <- char_data[eom == as.Date(d)]
    # Add specific risk
    factor_char_sub %>% setorder(id)  # Important to align ids across different datasets
    X <- factor_char_sub[, str_remove_all(colnames(fct_cov), "`"), with=F] %>% as.matrix() # Using colnames(fct_cov) is important so char and factor is aligned across x and fct_cov
    # Stock covariance matrix
    cov_est <- X %*% fct_cov %*% t(X) + diag(factor_char_sub$res_vol^2)
    rownames(cov_est) <- colnames(cov_est) <- as.character(factor_char_sub$id)
    # Change to long format
    cov_est[upper.tri(cov_est, diag=F)] <- NA_real_ # Limit redundant rows
    cov_est <- cov_est %>% as_tibble(rownames = "id1")
    cov_est <- cov_est %>% 
      pivot_longer(-id1, names_to = "id2", values_to = "cov") %>% 
      filter(!is.na(cov)) %>% 
      mutate(eom=as.Date(d)) |> 
      setDT()
    # Output
    return(cov_est)
  }, .progress = T) |> 
    rbindlist()
  # Final Output
  return(var_cov)
}

# Factor regression formula ----------------------------------------------------
x_vars_fun <- function(factors, ind) {
  if (ind) {
    ind_factors <- c("BusEq", "Chems", "Durbl", "Enrgy", "Hlth",  "Manuf", 
                     "Money", "NoDur", "Other", "Shops", "Telcm", "Utils")
  } else {
    ind_factors <- "mkt"
  }
  # Output
  c(ind_factors, factors)
}

factor_formula <- function(y, factors, ind) {
  # Industry factors 
  if (ind) {
    ind_factors <- c("BusEq", "Chems", "Durbl", "Enrgy", "Hlth",  "Manuf", 
                     "Money", "NoDur", "Other", "Shops", "Telcm", "Utils")
  } else {
    ind_factors <- "mkt"
  }
  # Factor return formula
  if (is.null(factors)) {
    form <- paste0(y, "~-1+", paste0(ind_factors, collapse = "+"))
  } else {
    cls_names <- paste0("`", factors, "`")
    form <- paste0(y, "~-1+", paste0(c(ind_factors, cls_names), collapse = "+"))
  }
  # Output
  return(form)
}

# Long to square covariance ----------------------------------------------------
#    Takes a long data.table of variances and covariances 
#    and creates the associated covariance matrix
cov_long_to_square <- function(sub) {
  cov_mat <- sub |> 
    dcast(id1 ~ id2, value.var = "cov") 
  rn <- cov_mat$id1
  cov_mat <- cov_mat |> 
    select(-id1) |>
    as.matrix()
  rownames(cov_mat) <- rn
  cov_mat[upper.tri(cov_mat)] <- t(cov_mat)[upper.tri(cov_mat)]
  return(cov_mat)
}

# Generate data ----------------------------------------------------------------
create_data <- function(region, features, sample=FALSE) {
  # Prepare characteristics 
  chars <- read_parquet(
    file = paste0("../Data/CTF-data/", region, "/ctff_chars.parquet"),
    col_select = all_of(c("excntry", "id", "eom", "eom_ret", "sic",
                          "ret_exc_lead1m", "ctff_test", features))
  ) |> setDT()
  if (sample) {
    # sample_ids <- chars$id |> sample(1000)
    # chars <- chars[id %in% sample_ids]
    chars <- chars[year(eom) %in% 2011:2012]
  }
  # normalize and impute
  chars <- chars |> 
    prepare_pred_data(
      features = features, 
      feat_prank = T, 
      impute = T
    )
  # Add FF12 industry classification
  chars[, ff12 := sic |> ff12_class()]
  # Exclude days with less than five stocks (only relevant for EM)
  chars <- chars[, n := .N, by = eom]
  chars <- chars[n>=5][, n := NULL]
  
  # Daily returns 
  daily <- read_parquet(
    file = paste0("../Data/CTF-data/", region, "/ctff_daily_ret.parquet"), 
    col_select = c("id", "date", "ret_exc")
  )
  daily <- daily[date>=as.Date("1965-01-01")]
  daily[, eom := date %>% ceiling_date(unit = "month")-1]
  
  # Output
  list("chars"=chars, "daily"=daily)
}
