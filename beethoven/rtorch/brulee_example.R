###############################################################################
# example utilizing `base_mlp` with pollution data
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
# setwd("/Volumes/manwareme/scratch/beethoven/rtorch")

# source mlp function
source("./brulee_function.R")

# import and clean
pollution_full <- read.csv("./data/pollution.csv")
head(pollution_full)
tail(pollution_full)
pollution_full <- pollution_full[
  complete.cases(pollution_full), !names(pollution_full) %in% "No"
]
pollution_full$cbwd <- as.factor(pollution_full$cbwd)

# subset for examples
# sample_id <- sample(pollution_full$No, size = 5000, replace = FALSE)
# pollution <- pollution_full[pollution_full$No %in% sample_id, ]
pollution <- pollution_full
summary(pollution)

# build recipe
pollution_recipe <- recipe(
  data = pollution,
  pm2.5 ~ .
) %>%
  # Convert "cbwd" to factor
  recipes::step_dummy("cbwd") %>%
  # Normalize all predictors
  step_normalize(all_predictors())
pollution_recipe$var_info

# set hyperparameters
epochs <- c(100, 150, 200, 250)
hidden_units <- list(
  c(32, 32), c(32, 64), c(64, 64), c(64, 128), c(128, 128)
)
learn_rate <- c(0.01, 0.005, 0.001)

# implement new function with sample data
pollution_mlp <- base_mlp(
  recipe = pollution_recipe,
  data = pollution,
  epochs = epochs,
  hidden_units = hidden_units,
  activation = "relu",
  learn_rate = learn_rate,
  folds = 5,
  importance = "permutations",
  engine = "brulee",
  outcome = "regression",
  metric = "rmse"
)

# pollution_mlp <- readRDS("data/pollution_mlp.RDS")

# inspect performance
yardstick::metrics(pollution_mlp[[2]], pm2.5, .pred)

# inspect model
pollution_mlp[[2]]

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

# save
saveRDS(
  pollution_mlp,
  paste0(
    "./data/model_output/pollution_mlp_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".rds"
  )
)
