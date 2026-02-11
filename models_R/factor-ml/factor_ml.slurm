#!/bin/bash
#SBATCH -J factor-ml
#SBATCH -o slurm/output_factor-ml.txt
#SBATCH -e slurm/error_factor-ml.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=300G
#SBATCH --partition=normal
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=theis.jensen@yale.edu

echo '-------------------------------'
cd ${SLURM_SUBMIT_DIR}
echo ${SLURM_SUBMIT_DIR}
echo Running on host $(hostname)
echo Time is $(date)
echo SLURM_NODES are ${SLURM_NODELIST}
echo '-------------------------------'
echo -e '\n\n'

# Restore renv packages
R -q -e "renv::restore(prompt=FALSE)"

# Run factor_ml in a single R session
Rscript -e '
source("models_R/factor-ml/factor_ml.R")

#features  <- arrow::read_parquet("data/raw/ctff_features.parquet")
#chars     <- arrow::read_parquet("data/raw/ctff_chars.parquet")
#daily_ret <- arrow::read_parquet("data/raw/ctff_daily_ret.parquet")
features  <- arrow::read_parquet("private/toy_dataset/ctff_features.parquet")
chars     <- arrow::read_parquet("private/toy_dataset/ctff_chars.parquet")
daily_ret <- arrow::read_parquet("private/toy_dataset/ctff_daily_ret.parquet")

pf <- main(chars = chars, features = features, daily_ret = daily_ret)

print(head(pf))
print(paste("Unique months:", length(unique(pf$eom))))
print(paste("Total rows:", nrow(pf)))

data.table::fwrite(pf, "data/processed/factor_ml.csv")
'
