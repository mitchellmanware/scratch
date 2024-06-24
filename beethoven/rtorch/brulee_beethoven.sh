#!/bin/bash
#SBATCH --partition=geo
#SBATCH --mail-user=mitchell.manware@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --cpus-per-task=16

export LD_LIBRARY_PATH=/ddn/gs1/biotools/R/lib64/R/customlib:$LD_LIBRARY_PATH
R CMD BATCH /ddn/gs1/home/manwareme/scratch/beethoven/rtorch/brulee_beethoven.R