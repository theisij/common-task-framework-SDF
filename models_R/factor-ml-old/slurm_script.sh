#!/bin/bash
#SBATCH -J return-pred                           # Job name
#SBATCH --array=1-3                       # Number of tasks (4 tasks for 4 outcomes)
#SBATCH -o return-prediction/slurm_output/output_%A_%a.txt                # Output file (%A=job ID, %a=array task ID)
#SBATCH -e return-prediction/slurm_output/error_%A_%a.txt                 # Error file 
#SBATCH --ntasks=1                      # Number of tasks (processes)
#SBATCH --cpus-per-task=48              # Number of CPU cores per task
#SBATCH --mem=200G                      # Memory per node (450 GB)
#SBATCH --partition=normal
#SBATCH --time=5-00:00:00                # HH:MM:SS
#SBATCH --mail-type=ALL                 # Send email on start, end and fail
#SBATCH --mail-user=theis.jensen@yale.edu

OUTCOME=$(sed -n "${SLURM_ARRAY_TASK_ID}p" 'return-prediction/joblist.txt')

echo '-------------------------------'
cd ${SLURM_SUBMIT_DIR}                         # Change directory to slurm submit directory
echo ${SLURM_SUBMIT_DIR}
echo Running on host $(hostname)
echo Time is $(date)
echo SLURM_NODES are ${SLURM_NODELIST}
echo '-------------------------------'
echo -e '\n\n'

# Load any necessary modules
module load anaconda3/2023.09-0-gcc-13.2.0-6gotrib

# Activate environment
conda activate r-xgb-return-env

# Your R script execution command
Rscript "return-prediction/slurm-recipe-xgb.R" --outcome ${OUTCOME}