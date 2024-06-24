#!/bin/bash
#SBATCH --partition=geo
#SBATCH --mail-user=mitchell.manware@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --cpus-per-task=16

# conatiner image
brulee_sif="/ddn/gs1/home/manwareme/scratch/beethoven/rtorch/brulee_apptainer.sif"
# R script
brulee_R="/ddn/gs1/home/manwareme/scratch/beethoven/rtorch/brulee_beethoven.R"

# run
singularity exec $brulee_sif Rscript $brulee_R