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
library(doParallel)
library(colorRamps)

setwd("/ddn/gs1/home/manwareme/scratch")
getwd()
source("./beethoven/rtorch/brulee_function.R")

# register cores
registerDoParallel(cores = (Sys.getenv("SLURM_CPUS_PER_TASK")))

# import covariate data (excludes NDVI and TRI data)
beethoven_data <- sf::st_as_sf(
  readRDS("./beethoven/rtorch/data/beethoven_geom.rds"),
  coords = c("lon", "lat"),
  crs = 4326
)

# sample for local development
beethoven_id <- sample(unique(beethoven_data$site_id), 500)
beethoven_data <- beethoven_data[beethoven_data$site_id %in% beethoven_id, ]

# site identifiers as factor
beethoven_data <- beethoven_data |>
  mutate(site_id = as.factor(site_id))

# remove columns with missing data
beethoven_data <- beethoven_data[, !colSums(is.na(beethoven_data)) > 0]

# manual spatial clustering
n_clusters <- 5
beethoven_cluster <- create_spatial_clusters(beethoven_data, n_clusters)
beethoven_folds <- spatial_clustering_cv_manual(
  beethoven_cluster,
  v = n_clusters
)

# extract splits (and drop "cluster")
beethoven_split <- extract_splits(beethoven_folds)

# prepare train and test data
beethoven_train_sf <- beethoven_split[[1]]$train_data
beethoven_test_sf <- beethoven_split[[2]]$test_data

# build recipe
beethoven_recipe <- recipe(
  pm2.5 ~ .,
  data = st_drop_geometry(beethoven_train_sf) |> select(-cluster)
) |>
  step_date(
    time,
    features = c("dow", "month", "year"),
    keep_original_cols = FALSE
  ) |>
  step_mutate(time_year = as.factor(time_year)) |>
  step_dummy(time_dow, time_month, time_year, site_id) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())
print(beethoven_recipe$var_info, n = nrow(beethoven_recipe$var_info))

# set hyperparameters
epochs <- c(200, 250)
hidden_units <- list(
  c(16, 16), c(32, 32), c(64, 64)
)
learn_rate <- c(0.01)

# implement new function with sample data
beethoven_mlp <- base_mlp(
  recipe = beethoven_recipe,
  train = beethoven_train_sf,
  test = beethoven_test_sf,
  epochs = epochs,
  hidden_units = hidden_units,
  activation = "relu",
  learn_rate = learn_rate,
  cv = "spatial_clustering_cv_manual",
  folds = n_clusters - 1,
  importance = "permutations",
  engine = "brulee",
  outcome = "regression",
  metric = "rmse"
)

# inspect performance
yardstick::metrics(beethoven_mlp[[2]], pm2.5, .pred)

# inspect model
beethoven_mlp[[1]]
beethoven_mlp[[2]]

# plot
ggplot(
  data = beethoven_mlp[[3]],
  aes(x = pm2.5, y = .pred)
) +
  geom_pointdensity() +
  scale_color_gradientn(
    colors = matlab.like(11),
    name = "Density Estimation"
  ) +
  geom_smooth(method = "lm", se = TRUE, col = "black") +
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
