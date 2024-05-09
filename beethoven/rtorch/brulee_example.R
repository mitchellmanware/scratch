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

# source mlp function
source("./brulee_function.R")

# import and clean
pollution <- read.csv("./data/pollution.csv")
head(pollution)
pollution <- pollution[complete.cases(pollution), !names(pollution) %in% "No"]
pollution$cbwd <- as.factor(pollution$cbwd)

# split train and test data
train_i <- sample(nrow(pollution), size = floor(0.8 * nrow(pollution)))
train <- pollution[train_i, ]
test <- pollution[-train_i, ]
nrow(test) + nrow(train) == nrow(pollution)

# build recipe
pollution_recipe <- recipe(
  data = pollution,
  pm2.5 ~ .
) %>%
  # Convert "cbwd" to factor
  recipes::step_dummy("cbwd") %>%
  # Normalize all predictors
  step_normalize(all_predictors())
print(pollution_recipe$var_info)

# implement new function with sample data
pollution_mlp <- base_mlp(
  recipe = pollution_recipe,
  train = train,
  test = test,
  epochs = c(100, 150),
  hidden_units = list(32, 64, c(32, 32), c(32, 64), c(64, 64)),
  activation = "relu",
  learn_rate = c(0.01, 0.005, 0.001),
  folds = 5,
  importance = "permutations",
  engine = "brulee",
  outcome = "regression",
  metric = "rmse"
)

# save
saveRDS(pollution_mlp, "./pollution_mlp.RDS")

# read
pollution_read <- readRDS("./pollution_mlp.RDS")

# inspect performance
yardstick::metrics(pollution_read[[2]], pm2.5, .pred)

# inspect model
pollution_read[[1]]

# plot
ggplot(
  data = pollution_read[[2]],
  aes(x = pm2.5, y = .pred)
) +
  geom_pointdensity() +
  scale_color_viridis(option = "D", name = "Density Estimation") +
  geom_smooth(method = "lm", se = TRUE, col = "tomato") +
  labs(
    x = "Observed PM2.5",
    y = "Predicted PM2.5"
  ) +
  theme_pubr() +
  theme(legend.position = "right")
