###############################################################################
# example utilizing `base_mlp` with covariates for 2018
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
library(spatialsample)
library(sf)

setwd("/ddn/gs1/home/manwareme/scratch")
getwd()
source("./beethoven/rtorch/brulee_function.R")

# import covariate data (all but NDVI data)
beethoven_data <- readRDS("./beethoven/rtorch/data/beethoven_data.rds")

# remove columns with missing data
beethoven_data <- beethoven_data[, !colSums(is.na(beethoven_data)) > 0]

# sample for local development
sample_id <- sample(unique(beethoven_data$site_id), 20)
beethoven_sample <- beethoven_data[
  beethoven_data$site_id %in% sample_id,
]
nrow(beethoven_sample)
ncol(beethoven_sample)

# build recipe
beethoven_recipe <- recipe(beethoven_train, pm2.5 ~ .) |>
  update_role(site_id, new_role = "ID") |>
  update_role(pm2.5, new_role = "outcome") |>
  step_time(time) |>
  step_zv(all_predictors()) |>
  step_normalize(all_predictors())

print(beethoven_recipe$var_info, n = nrow(beethoven_recipe$var_info))

# set hyperparameters
epochs <- c(100, 150, 200, 250)
hidden_units <- list(
  c(16, 16), c(16, 32), c(32, 32), c(32, 64), c(64, 64)
)
learn_rate <- c(0.01, 0.005, 0.001)

expand.grid(
  epochs,
  hidden_units,
  learn_rate
)

epochs <- c(100)
hidden_units <- list(
  c(16, 16), c(32, 32)
)
learn_rate <- c(0.01)


# implement new function with sample data
beethoven_mlp <- base_mlp(
  recipe = beethoven_recipe,
  train = beethoven_train,
  test = beethoven_test,
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

# inspect performance
yardstick::metrics(beethoven_mlp[[2]], pm2.5, .pred)

# inspect model
beethoven_mlp[[1]]

# plot
ggplot(
  data = beethoven_mlp[[2]],
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

# save
beethoven_filename <- paste0(
  "./beethoven/rtorch/data/model_output/beethoven_mlp_",
  format(Sys.time(), "%Y%m%d_%H%M%S"),
  ".rds"
)
saveRDS(
  beethoven_mlp,
  beethoven_filename
)
