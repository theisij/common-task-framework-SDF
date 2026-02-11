# Run this code to fit models in parallel on a slurm HPC
save <- T
use_gpu <- F

# Run code
tictoc::tic("Total run time")
source("models_R/factor-ml-old/0-functions.R", echo = T)
source("models_R/factor-ml-old/1-setup.R", echo = T)
source("models_R/factor-ml-old/2-data.R", echo = T)
source("models_R/factor-ml-old/3-xgb.R", echo = T)
source("models_R/factor-ml-old/4-predictions.R", echo = T)
source("models_R/factor-ml-old/5-portfolios.R", echo = T)
tictoc::toc()