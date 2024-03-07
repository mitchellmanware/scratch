# explore creating torch tensor with sample covariate data
# March 06, 2024

# import covariate data
source("../../beethoven/covariates_data_frame.R")
d <- covariates_data_frame(
  covariates = list.files("data/", full.names = TRUE),
  type = "rds",
  dates = c("2020-12-01", "2021-01-31"),
  sample = 10
)
# summary
summary(d)
head(d)
tail(d)
# check for NAs
all(!is.na(d))

# simulate PM outcome data
d$pm2.5 <- rgamma(10 * 62, 32, 1.6)

# libraries
library(torch)
library(luz)
library(data.table)
devtools::load_all("/Volumes/SET/Projects/NRT-AP-Model")

# covariate tensor
xcol <- which(!(colnames(d) %in% c("site_id", "time", "pm2.5")))
xtensor <- 
  d[, xcol] |>
  # data.table::melt(measure.vars = colnames(d)[xcol]) |>
  # _[, 2] |>
  as.matrix() |>
  torch::torch_tensor(dtype = torch::torch_float()) |>
  torch::torch_reshape(shape = list(1, 10, 62, 4))
xtensor

# response tensor
ycol <- which(colnames(d) == "pm2.5")
ytensor <-
  d[,ycol] |>
  as.matrix() |>
  torch::torch_tensor(dtype = torch::torch_float()) |>
  torch::torch_reshape(shape = list(1, 10, 62, 1))
ytensor
