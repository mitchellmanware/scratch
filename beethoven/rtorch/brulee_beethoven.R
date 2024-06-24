###############################################################################
# example utilizing `base_mlp` with covariates
# packages
library(rlang)
library(torch, lib.loc = .libPaths()[2])
library(tidymodels)
library(brulee)
library(recipes)
library(yardstick)
library(ggplot2)
library(tune)
library(caret)
library(workflows)
library(rsample)
library(dplyr)
library(viridis)
library(doParallel)
library(colorRamps)
library(qs)
.libPaths(c("/ddn/gs1/biotools/R/lib64/R/custompkg", .libPaths()))
library(sf)

# set working directory
setwd("/ddn/gs1/home/manwareme/scratch/beethoven/rtorch/")

# source functions
source("./brulee_function.R")

# register cores
registerDoParallel(cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1)

# import covariate data
beethoven_file <- list.files(
  "./data",
  pattern = "sf_",
  full.names = TRUE
)
beethoven_data <- qs::qread(beethoven_file)

# remove Event.Type
if ("Event.Type" %in% names(beethoven_data)) {
  beethoven_data <- beethoven_data |> select(-Event.Type)
}

# spatiotemporal cross validation methods
beethoven_cv <- list(
  # spatial cv method (cluster example)
  c(cv = "cluster", folds = 5, n_clusters = 5)
  # temporal cv method
  # ...
  # edge case cv method
  # ...
)

# bootstrap data
beethoven_bootstrap <- bootstrap(
  beethoven_data,
  p = 0.1,
  n = length(beethoven_cv)
)

# initiate empty list for model storage
beethoven_models <- list()

# for loop to utilize all spatiotemoral cross validation methods
for (b in seq_along(beethoven_bootstrap)) {

  # time
  beethoven_bootstrap[[b]]$time <-
    as.Date(beethoven_bootstrap[[b]]$time)
  
  # split data
  beethoven_split <- temporal_split(
    beethoven_bootstrap[[b]],
    time_id = "time",
    prop = 0.8
  )

  # prepare train and test data
  beethoven_train_sf <- beethoven_split[[1]]
  beethoven_test_sf <- beethoven_split[[2]]

  # build recipe
  beethoven_recipe <- st_recipe(
    data = beethoven_train_sf,
    outcome_id = "Arithmetic.Mean",
    locs_id = "site_id",
    time_id = "time"
  )
  print(beethoven_recipe$var_info)

  # set hyperparameters
  epochs <- c(1000)
  hidden_units <- list(c(16, 16), c(32, 32), c(64, 64))
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
      beethoven_cv[[b]]
    )
  )

  # inspect performance
  yardstick::metrics(beethoven_mlp[[3]], Arithmetic.Mean, .pred)

  # inspect model
  beethoven_mlp[[1]]
  beethoven_mlp[[2]]
  beethoven_mlp[[3]]

  # store best model
  beethoven_models[[b]] <- beethoven_mlp[[3]]
}

# RDS file path
beethoven_filename <- paste0(
  "./beethoven/rtorch/data/model_output/beethoven_mlp_",
  format(Sys.time(), "%Y%m%d_%H%M%S"),
  ".rds"
)

# save
saveRDS(
  beethoven_models,
  beethoven_filename
)
