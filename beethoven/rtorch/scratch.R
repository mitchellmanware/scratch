# Mitchell Manware
# June 24, 2024

setwd("/ddn/gs1/home/manwareme/scratch/")
getwd()
.libPaths()

# libraries
library(brulee)
library(qs)

# source base_mlp and auxiliary functions
source("./beethoven/rtorch/brulee_function.R")

#### local development
# import imputed covariate data
beethoven_file <- list.files(
  "/ddn/gs1/group/set/Projects/NRT-AP-Model/output",
  pattern = "sf_",
  full.names = TRUE
)
beethoven_data <- qs::qread(beethoven_file)
nrow(beethoven_data)
ncol(beethoven_data)
head(sort(beethoven_data$time))
tail(sort(beethoven_data$time))

# remove "Event.Type" column
if ("Event.Type" %in% names(beethoven_data)) {
  beethoven_data <- beethoven_data |> select(-Event.Type)
}

# spatiotemporal cross validation methods
beethoven_cv <- list(
  # cluster
  c(cv = "cluster", folds = 5, n_clusters = 5)
  # # block
  # c(cv = "block", n_blocks = 5),
  # # lolo
  # c(cv = "lolo", locs_id = "site_id")
)

# bootstrap data
beethoven_bootstrap <- bootstrap(
  beethoven_data,
  p = 0.01,
  n = length(beethoven_cv)
)

# initiate empty list for model storage
beethoven_models <- list()

# fit models with different bootstraps for each CV method
for (b in seq_along(beethoven_bootstrap)) {
  # prepare train and test data
  beethoven_split <- temporal_split(beethoven_bootstrap[[1]], "time", 0.8)
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
  epochs <- c(250)
  hidden_units <- list(c(16, 16))
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
      beethoven_cv[[1]]
    )
  )

  # model performance
  yardstick::metrics(beethoven_mlp[[3]], Arithmetic.Mean, .pred)

  # model output
  beethoven_mlp[[1]]
  beethoven_mlp[[2]]
  beethoven_mlp[[3]]

  # store best model
  beethoven_models[[b]] <- beethoven_mlp[[3]]
}
