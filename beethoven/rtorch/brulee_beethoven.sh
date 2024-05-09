#!/bin/bash
#SBATCH --partition=geo
#SBATCH --mail-user=mitchell.manware@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --cpus-per-task=12

R CMD BATCH /ddn/gs1/home/manwareme/scratch/beethoven/rtorch/brulee_beethoven.R