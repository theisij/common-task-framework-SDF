#!/bin/bash
#SBATCH -J minimum-variance
#SBATCH -o slurm/output_minimum-variance.txt
#SBATCH -e slurm/error_minimum-variance.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=300G
#SBATCH --partition=gpunormal
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

# Load Apptainer
module load apptainer/1.4.1-nkna

# Apptainer image (R 4.5.1 tidyverse)
IMAGE="${SLURM_SUBMIT_DIR}/r451-tidyverse-extended.sif"
if [[ ! -f "$IMAGE" ]]; then
  echo "ERROR: Apptainer image not found: $IMAGE"
  exit 1
fi

# Threading for BLAS/OpenMP-aware packages
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export OPENBLAS_NUM_THREADS=${SLURM_CPUS_PER_TASK}
export MKL_NUM_THREADS=${SLURM_CPUS_PER_TASK}

# Restore renv packages
echo "[bootstrap] renv::restore() inside container..."
apptainer exec -B "$PWD" "$IMAGE" \
  R -q -e "renv::restore(prompt=FALSE)"

# Run minimum_variance
echo "[run] Running minimum_variance..."
START_TIME=$(date +%s)
apptainer exec -B "$PWD" "$IMAGE" \
  Rscript -e '
source("models_R/minimum-variance/minimum_variance.R")

features  <- arrow::read_parquet("data/raw/ctff_features.parquet")
chars     <- arrow::read_parquet("data/raw/ctff_chars.parquet")
daily_ret <- arrow::read_parquet("data/raw/ctff_daily_ret.parquet")

pf <- main(chars = chars, features = features, daily_ret = daily_ret)

print(head(pf))
print(paste("Unique months:", length(unique(pf$eom))))
print(paste("Total rows:", nrow(pf)))

data.table::fwrite(pf, "data/processed/minimum_variance.csv")
'

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))
echo "[done] Elapsed time: $((ELAPSED / 3600))h $(((ELAPSED % 3600) / 60))m $((ELAPSED % 60))s"
