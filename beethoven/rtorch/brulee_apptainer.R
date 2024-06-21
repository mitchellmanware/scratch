# simple R script for exploring apptainer functionality

library(torch)
torch::torch_tensor(matrix(1:10))
library(brulee)

# # libraries
# install.packages("qs")
# library(qs)

# # working directory
# setwd("/ddn/gs1/home/manwareme/scratch/beethoven/rtorch/")

# # read data
# data <- qs::qread("data/sf_feat_calc_design_imputed_061224.qs")

# # check
# class(data)
# head(data[, 1:10])