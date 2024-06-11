###############################################################################
# example utilizing `base_mlp` with covariates for January
# packages
library(sf)
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
library(doParallel)
library(colorRamps)

# set working directory
setwd("/ddn/gs1/home/manwareme/scratch")
getwd()

# source functions
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
beethoven_id <- sample(unique(beethoven_data$site_id), 100)
beethoven_data <- beethoven_data[beethoven_data$site_id %in% beethoven_id, ]

# site identifiers as factor
beethoven_data <- beethoven_data |>
  mutate(site_id = as.factor(site_id))

# remove columns with missing data
beethoven_data <- beethoven_data[, !colSums(is.na(beethoven_data)) > 0]

# spatial cross validation methods
cv_type <- c("cluster", "block", "lolo", "buffer", "nndm")

# hyperparameters
beethoven_params <- list(
  # cluster
  c(cv = "cluster", folds = 5, n_clusters = 5),
  # block
  c(cv = "block", n_blocks = 5),
  # lolo
  c(cv = "lolo", locs_id = "site_id"),
  # buffer
  c(cv = "buffer", buffer_distance = 1, folds = 5),
  # nndm
  c(cv = "nndm", n_neighbors = 50, folds = 5)
)

# for loop to utilize all spatiotemoral cross validation methods
for (c in seq_along(cv_type)) {

  # split data according to cross validation type
  # predict cannot predict values for factors it has not seen (?)
  # if training does not include at least 1 date from 2022, all 2022 values
  # are returned as NA -- exploring this error further now
  beethoven_split <- temporal_split(
    beethoven_data,
    time_id = "time",
    prop = 0.83
  )

  # prepare train and test data
  beethoven_train_sf <- beethoven_split[[1]]
  beethoven_test_sf <- beethoven_split[[2]]

  # build recipe
  beethoven_recipe <- st_recipe(
    data = beethoven_train_sf,
    outcome_id = "pm2.5",
    locs_id = "site_id",
    time_id = "time"
  )
  print(beethoven_recipe$var_info)

  # set hyperparameters
  epochs <- c(500)
  hidden_units <- list(c(16, 32, 16))
  learn_rate <- c(0.01)

  # fit mlp
  beethoven_mlp <- do.call(
    base_mlp,
    c(
      list(
        recipe = beethoven_recipe,
        train = beethoven_train_sf,
        test = beethoven_test_sf,
        epochs = epochs,
        hidden_units = hidden_units,
        activation = "relu",
        learn_rate = learn_rate,
        importance = "permutations",
        engine = "brulee",
        outcome = "regression",
        metric = "rmse"
      ),
      beethoven_params[[c]]
    )
  )

  # inspect performance
  yardstick::metrics(beethoven_mlp[[3]], pm2.5, .pred)

  # inspect model
  beethoven_mlp[[1]]
  beethoven_mlp[[2]]
  beethoven_mlp[[3]]

  # save
  beethoven_filename <- paste0(
    "./beethoven/rtorch/data/model_output/beethoven_mlp_",
    cv_type[c],
    "_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".rds"
  )
  saveRDS(
    beethoven_mlp,
    beethoven_filename
  )
}
