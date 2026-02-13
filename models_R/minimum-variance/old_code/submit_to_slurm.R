# Run this code to estimate in parallel on a slurm-based HPC
source("covariance-estimation/1-setup.R")

# Read configuration 
if (TRUE) {
  args <- commandArgs(trailingOnly = TRUE)
  string <- paste0(args, collapse = " ")
  set$features <- string |> 
    str_extract("\\b(?<=features=)[a-z_]+") 
} else {
  set$features <- "expanded_ex_original"
}
# Print the outcome variable (for testing)
print(paste0("Fitting model for ", set$features))

# Create overall folder with name=today() unless it's already there
# op_folder_overall <- paste0("covariance-estimation/Generated/2025-02-11/", set$features, "/")
op_folder_overall <- paste0("covariance-estimation/Generated/", Sys.Date(), "/", set$features, "/")
if (!dir.exists(op_folder_overall)) {
  dir.create(op_folder_overall, recursive = T)
  # Create sub-folders
  dir.create(paste0(op_folder_overall, "us"))
  dir.create(paste0(op_folder_overall, "dev_ex_us"))
  dir.create(paste0(op_folder_overall, "em"))
}

tictoc::tic("Total run time")
source("covariance-estimation/2-var_cov.R", echo = T)
source("covariance-estimation/3-portfolios.R", echo = T)
tictoc::toc()