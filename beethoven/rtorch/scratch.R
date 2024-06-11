# Mitchell Manware
# May 17, 2024

getwd()
source("./beethoven/rtorch/brulee_function.R")
source("./beethoven/covariates_data_frame.R")

"srun --partition=normal --cpus-per-task=1 --pty top"

mlps <- list.files("./beethoven/rtorch/data/model_output/", full.names = TRUE)
mlps
beethoven_mlp <- readRDS(mlps[5])

# inspect performance
beethoven_performance <- yardstick::metrics(beethoven_mlp[[2]], pm2.5, .pred)

# inspect model
beethoven_mlp[[1]]
head(beethoven_mlp[[2]])
nrow(beethoven_mlp[[2]])
length(unique(beethoven_mlp[[2]]$site_id))

# local development
# import covariate data (excludes NDVI and TRI data)
beethoven_data <- sf::st_as_sf(
  readRDS("./beethoven/rtorch/data/beethoven_geom.rds"),
  coords = c("lon", "lat"),
  crs = 4326
)

# sample for local development
beethoven_id <- sample(unique(beethoven_data$site_id), 10)
beethoven_data <- beethoven_data[beethoven_data$site_id %in% beethoven_id, ]

# site identifiers as factor
beethoven_data <- beethoven_data |>
  mutate(site_id = as.factor(site_id))

# remove columns with missing data
beethoven_data <- beethoven_data[, !colSums(is.na(beethoven_data)) > 0]

# prepare train and test data
beethoven_split <- temporal_split(beethoven_data, "time", 0.83)
beethoven_train_sf <- beethoven_split[[1]]
beethoven_test_sf <- beethoven_split[[2]]
nrow(beethoven_train_sf) / (nrow(beethoven_train_sf) + nrow(beethoven_test_sf))

sort(unique(beethoven_train_sf$time))
sort(unique(beethoven_test_sf$time))

# build recipe
beethoven_recipe <- st_recipe(
  data = beethoven_train_sf,
  outcome_id = "pm2.5",
  locs_id = "site_id",
  time_id = "time"
)
print(beethoven_recipe$var_info)

# set hyperparameters
epochs <- c(250)
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
      metric = "rmse",
      cv = "cluster",
      folds = 5,
      n_clusters = 5
    )
  )
)

# model performance
yardstick::metrics(beethoven_mlp[[3]], pm2.5, .pred)

# model output
beethoven_mlp[[1]]
beethoven_mlp[[2]]
beethoven_mlp[[3]]
beethoven_mlp[[4]]
