# common_data.R — Generate the CTF common data (ctff_chars, ctff_daily_ret)
#
# Copy of the script that produced data/raw/ctff_*.parquet (run 2025-09-02 on the
# JKP release of 2025-02-22); it reproduces the published files exactly. Paths
# are relative to the original private project folder.

# Libraries --------------------------
library(arrow)
library(tidyverse)
library(data.table)

# Settings ---------------------------------------------------------------------
data_set <- list(
  # Based on eom_ret
  sample_start = list(
    "us" = as.Date("1952-01-31"),        # First year with more than 100 obs in all months
    "dev_ex_us" = as.Date("1990-01-31"), # Start of test period 
    "em" = as.Date("1994-01-31")         # First year with more than 100 obs in all months
  ),
  sample_end = as.Date("2023-12-31"),
  test_start = as.Date("1990-01-31"),
  screens = list(
    size_grps = c("small", "large", "mega"),
    min_feat = 75
  )
)

data_folder <- "../../../../International Stock Data/Public/Data/"  # JKP USA.csv release of 2025-02-22 (data through 2024)

# Regions ----------------------------------------------------------------------
regions <- c("us", "dev_ex_us", "em")

# Countries --------------------------------------------------------------------
country_class <- fread("../Data/updated_country_classification.csv")
country_class[, end := end |> as.Date(format="%m/%d/%Y")]
country_class[, start := start |> as.Date(format="%m/%d/%Y")]
country_class[is.na(start), start := as.Date("1800-01-01")]   # Backfill MSCI classification

# Features ---------------------------------------------------------------------
features_orig <- read_parquet("../Data/CTF-data/ctff_features_original.parquet")
features_orig <- features_orig$feature
features <- read_parquet("../Data/CTF-data/ctff_features_expanded.parquet")
features <- features$feature

# Return cutoffs ---------------------------------------------------------------
# Monthly
retcut_m <- fread("../Data/return_cutoffs.csv", 
                  colClasses = c("eom"="character"))
retcut_m <- retcut_m[, .(eom_ret = as.Date(eom, "%Y%m%d"), 
                         "wins_low"=ret_exc_0_1, 
                         "wins_high"=ret_exc_99_9)]
# Daily
retcut_d <- fread("../Data/return_cutoffs_daily.csv")
retcut_d <- retcut_d[, .(year, month, 
                         "wins_low"=ret_exc_0_1, 
                         "wins_high"=ret_exc_99_9)]

# Generate data ----------------------------------------------------------------
regions |> walk(function(x) {
  print(x)
  # Create a dedicated folder 
  folder <- paste0("../Data/CTF-data/", x, "/")
  if (!dir.exists(folder)) dir.create(folder)
  # Extract relevant countries
  if (x=="us") {
    countries <- country_class[excntry == "USA"]
  } else if (x=="dev_ex_us") {
    countries <- country_class[excntry != "USA" & msci_development=="developed"]
  } else if (x=="em") {
    countries <- country_class[msci_development=="emerging"]
  }
  # Relevant start date
  sample_start <- data_set$sample_start[[x]]
  
  # Create data ---
  data_list <- 1:nrow(countries) |> map(function(i) {
    # info
    cntry <- countries$excntry[i]
    start <- countries$start[i]
    end <- countries$end[i]
    print(cntry)
    
    # Daily returns --------
    daily_ret <- fread(paste0(data_folder, "Daily Returns/", cntry, ".csv"), 
                 colClasses = c("date"="character"),
                 select = c("id", "date", "ret_exc"))
    if (cntry == "USA") {
      daily_ret <- daily_ret[id<=99999] # If US, then only CRSP observations
    } 
    daily_ret <- daily_ret[!is.na(ret_exc)]
    daily_ret[, date := date %>% fast_strptime(format="%Y%m%d") %>% as.Date()]
    # Winsorize Compustat returns
    daily_ret[, year := year(date)]
    daily_ret[, month := month(date)]
    daily_ret <- retcut_d[daily_ret, on = .(year, month)]
    daily_ret[id>99999, ret_exc := pmin(pmax(ret_exc, wins_low), wins_high)]
    daily_ret[, c("year", "month", "wins_low", "wins_high") := NULL]
    # Filter
    daily_ret <- daily_ret[date >= start & date <= end & date <= data_set$sample_end]
    
    # Monthly characteristics and returns --- 
    chars <- fread(paste0(data_folder, "Characteristics/", cntry, ".csv"), 
                   select = c("excntry", "id", "eom", "me", "sic", "size_grp", 
                              "ret_exc_lead1m", features), 
                   colClasses = c("eom"="character", "sic"="character"))
    chars[, eom := eom |> fast_strptime("%Y%m%d") |> as.Date()]
    chars[, eom_ret := eom+1+months(1)-1]
    # Screens ---
    # CRSP screen (US only)
    if (cntry=="USA") {
      print(paste0("   CRSP screen excludes ", round(mean(chars$id>99999) * 100, 2), "% of the observations"))
      chars <- chars[id <= 99999] # Only CRSP observations
    }
    # Date screen
    print(paste0("   Date screen excludes ", round(mean(chars$eom_ret < sample_start | chars$eom_ret > data_set$sample_end) * 100, 2), "% of the observations"))
    chars <- chars[eom_ret >= sample_start  & eom_ret <= data_set$sample_end]
    # Monitor screen impact
    n_start <- nrow(chars)
    me_start <- sum(chars$me, na.rm = T)
    # Require me and valid next-month ret
    print(paste0("   Non-missing me and ret excludes ", round(mean(is.na(chars$me) | is.na(chars$ret_exc_lead1m)) * 100, 2), "% of the observations"))
    chars <- chars[!is.na(me) & !is.na(ret_exc_lead1m)]
    # Size screen
    print(paste0("   Size screen excludes ", round(mean(!(chars$size_grp %in% data_set$screens$size_grps)) * 100, 2), "% of the observations"))
    chars <- chars[size_grp %in% data_set$screens$size_grps]
    # Feature Screens
    feat_available <- chars %>% select(all_of(features_orig)) %>% apply(1, function(x) sum(!is.na(x)))
    print(paste0("   At least ", data_set$screens$min_feat, " features excludes ", round(mean(feat_available < data_set$screens$min_feat)*100, 2), "% of the observations"))
    chars <- chars[feat_available >= data_set$screens$min_feat]
    # Non-missing returns over past 252 trading days
    past_td_fun <- function(daily_ret, ids, past_n, past_n_min) {
      ret_sub <- daily_ret[id %in% ids]
      ret_sub[, eom := date %>% ceiling_date(unit = "month")-1]
      # Official trading days
      tds <- unique(ret_sub[, .(date, eom)])[order(date)]
      tds[, date_lagn := lag(date, past_n-1)]
      tds[, max_date := max(date), by = eom]
      tds <- tds[date==max_date]
      # Output
      unique(tds$eom) |> map(function(d) {
        # Valid return data?
        date_range <- tds[eom==d]
        sub <- ret_sub[date >= date_range$date_lagn & date <= d]
        sub <- sub[, .N, by = id][N>=past_n_min]
        sub[, .(id, eom=d, valid_ret = T)]
      }, .progress = T) |> rbindlist()
    }
    valid_stocks <- daily_ret |> past_td_fun(ids = unique(chars$id), past_n = 252, past_n_min = 200)
    chars <- valid_stocks[chars, on = .(id, eom)]
    print(paste0("   Non-missing past daily returns removes ", round(mean(is.na(chars$valid_ret)) * 100, 2), "% of the observations"))
    chars <- chars[!is.na(valid_ret)]
    # Summary
    print(paste0("   In total, the final dataset has ", 
                 round( (nrow(chars) / n_start)*100, 2), 
                 "% of the observations and ", 
                 round((sum(chars$me) / me_start)*100, 2), 
                 "% of the market cap in the post ", 
                 sample_start, " data"))
    # Add indicator for test data
    chars[, ctff_test := (eom_ret >= data_set$test_start)]
    # Winsorize Compustat returns
    chars <- retcut_m[chars, on = "eom_ret"]
    chars[id>99999, ret_exc_lead1m := pmin(pmax(ret_exc_lead1m, wins_low), wins_high)]
    chars[, c("wins_low", "wins_high") := NULL]
    # Filter
    chars <- chars[eom >= start & eom <= end]
    # Keep specific columns in natural order
    id_vars <- c("id", "eom", "eom_ret", "excntry", "sic", "size_grp", 
                 "ret_exc_lead1m", "ctff_test")
    chars <- chars[, c(id_vars, features), with=F]
    # Output
    list("daily_ret" = daily_ret, "chars" = chars)
  }, .progress = T)
 
  # Save daily returns ---
  daily_ret <- data_list |> map("daily_ret") |> rbindlist()
  daily_ret |> setorder(id, date)
  daily_ret |> write_parquet(paste0(folder, "ctff_daily_ret.parquet"))
  
  # Save monthly chars ---
  chars <- data_list |> map("chars") |> rbindlist()
  chars |> setorder(excntry, id, eom)
  chars |> write_parquet(paste0(folder, "ctff_chars.parquet"))
}, .progress = T)


if (FALSE) {
  theme_set(theme_bw())
  chars <- regions |> map(function(x) {
    print(x)
    folder <- paste0("../Data/CTF-data/", x, "/")
    read_parquet(paste0(folder, "ctff_chars.parquet"), 
                 col_select = c("id", "eom_ret")) |> mutate(region=x)
  }) |> 
    bind_rows() |> 
    setDT()
  
  # Observations over time
  chars[, .N, by = .(region, eom_ret)] |> 
    ggplot(aes(eom_ret, N, colour=region)) + 
    geom_point() +
    geom_line() +
    labs(title = "Coverage with all filters (new)") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(
      legend.position = "bottom",
      axis.title.x = element_blank()
    )
  
}
