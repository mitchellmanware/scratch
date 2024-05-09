###############################################################################
# example utilizing `base_mlp` with mushroom data
# packages
library(brulee)
library(recipes)
library(yardstick)
library(ggplot2)
library(torch)
library(tune)
library(caret)
library(tidymodels)
library(workflows)
library(rsample)
library(dplyr)
library(ggpubr)
library(ggpointdensity)
library(viridis)

# set working directory
setwd("/ddn/gs1/home/manwareme/scratch/beethoven/rtorch/")

# source functions
source("./brulee_function.R")
source("../covariates_data_frame.R")

# identify files
files <- list.files(
  "/ddn/gs1/group/set/Projects/NRT-AP-Model/output/",
  full.names = TRUE,
  pattern = ".rds"
)
# remove problematic NEI data and old NLCD and TRI data
files_sub <- files[-c(12, 13, 15, 20)]

nei <- readRDS(files[12])
head(nei)

# import and clean
covar <- covariates_data_frame(
  covariates = files_sub,
  type = "rds",
  dates = c("2018-01-01", "2018-12-31")
)
head(covar[, 1:5])

# summary
summary(covar)
head(covar)
tail(covar)
# check for NAs
all(!is.na(covar))
# generate PM2.5 outcome data
covar$pm2.5 <- rgamma(10 * 62, 32, 1.6)
covar <- covar[complete.cases(covar), ]

saveRDS(covar, "cov_data.RDS")

# # split train and test data
# train_i <- sample(nrow(cov), size = floor(0.8 * nrow(cov)))
# train <- pollution[train_i, ]
# test <- pollution[-train_i, ]
# nrow(test) + nrow(train) == nrow(pollution)

# # build recipe
# cov_recipe <- recipe(
#   data = cov,
#   pm2.5 ~ .
# ) %>%
#   # Convert "cbwd" to factor
#   recipes::step_dummy("cbwd") %>%
#   # Normalize all predictors
#   step_normalize(all_predictors())
# print(cov_recipe$var_info)

# # implement new function with sample data
# pollution_mlp <- base_mlp(
#   recipe = cov_recipe,
#   train = train,
#   test = test,
#   epochs = c(100, 150),
#   hidden_units = list(32, 64, c(32, 32), c(32, 64), c(64, 64)),
#   activation = "relu",
#   learn_rate = c(0.01, 0.005, 0.001),
#   folds = 5,
#   importance = "permutations",
#   engine = "brulee",
#   outcome = "regression"
# )

# # save
# saveRDS(cov_mlp, "./cov_mlp.RDS")

# # inspect performance
# yardstick::metrics(cov_mlp[[2]], pm2.5, .pred)

# # inspect model
# cov_mlp[[1]]

# plot
# ggplot(
#   data = pollution_mlp[[2]],
#   aes(x = pm2.5, y = .pred)
# ) +
#   geom_pointdensity() +
#   scale_color_viridis(option = "D", name = "Density Estimation") +
#   geom_smooth(method = "lm", se = TRUE, col = "tomato") +
#   labs(
#     x = "Observed PM2.5",
#     y = "Predicted PM2.5"
#   ) +
#   theme_pubr() +
#   theme(legend.position = "right")
