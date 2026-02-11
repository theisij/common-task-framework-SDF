library(arrow)
library(tidyverse)
library(data.table)
library(xgboost)

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
fig_width <- 6.5 # 6.5in is rouhghly the page width

# Settings ---------------------------------------------------------------------
set <- list(
  seed = 1,
  split = list(
    # Based on eom_ret
    train_years = 10,
    folds = 5
  ),
  xgb = list(
    hps = 20,
    iter1 = 1000,
    iter2 = 10000,
    eta1 = 0.15,
    eta2 = 0.01,
    es = 25,
    cores = parallel::detectCores()-4
  ),
  n_pfs = 10 # Number of portfolios for long-short factor
)


