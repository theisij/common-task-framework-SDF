library(glmnet)
library(arrow)
library(tidyverse)
library(data.table)
Rcpp::sourceCpp("ewma.cpp")

source("covariance-estimation/0-functions.R")

# Layout Settings --------------------------------------------------------------
theme_set(theme_bw(base_size = 10))
colours_theme <- c("#0C6291", "#A63446", "#1B9E77", "#D95F02", "#7570B3",
                   "#89CFFA", "#66A61E", "#E6AB02", "#A6761D", "#666666", 
                   "darkslategrey", "blue3", "red3", "purple2", "yellow2",
                   "aquamarine", "grey", "salmon", "antiquewhite", "chartreuse") # The first is dark blue, the next is dark red. The last is light blue. Consider addinng babyblue: #89CFF0
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = colours_theme)
}

scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = colours_theme)
}

fig_height <- 3  # Used to be 4
fig_width <- 6.5 # 6.5in is roughly the page width

# Choosing half-life's ---------------------------------------------------------
# Barra's most recent US Model: https://www.top1000funds.com/wp-content/uploads/2011/09/USE4_Methodology_Notes_August_2011.pdf
# Barra uses 48 months as half-life, seems high. https://www.alacra.com/alacra/help/barra_handbook_GEM.pdf, p.37
# Check US (p. 18):
#   - Say prediction horison is one-month
#   - Also say that they use Newey-West method to estimate correlations (to allow for non-zero auto-cross-correlation)
#   - Table 4.1 contains half-lifes
#   - Two models: USE4L and USE4S. On p. 3 they say that USE4S is most accurate on a monthly horizon, while USE4L is designed for longer-term investors. I use the USE4S settings

# Settings ---------------------------------------------------------------------
set <- list(
  feat_prank = T,
  feat_impute = T,
  ind = T, # Use industry factors (T) or market factor only (F)
  cov_set = list(
    obs = 252*10,           # Check tibble(t = 1:(set$cov_set$obs), w = (0.5^(1/(set$cov_set$hl_cor)))^t) %>% ggplot(aes(t, w)) + geom_point() + geom_hline(yintercept = 0)
    hl_cor = 504,           # Barra's US model USE4S, Table 4.1
    hl_var = 84,            # Barra's US model USE4S, Table 4.1   
    hl_stock_var = 84,      # Barra's US model USE4S, Table 5.1
    min_stock_obs = 252,
    initial_var_obs = 21*3
  ),
  ridge = T, # Use ridge regression (T) or OLS (F) to estimate factor returns?
  ridge_lambda = 10^(-4)
)

test_eoms_us <- seq.Date(
  from = as.Date("1980-01-01"), 
  to = as.Date("2023-12-01"), 
  by = "month"
)-1
test_eoms_other <- seq.Date(
  from = as.Date("1990-01-01"), 
  to = as.Date("2023-12-01"), 
  by = "month"
)-1
