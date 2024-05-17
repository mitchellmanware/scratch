# Mitchell Manware
# May 17, 2024

getwd()
source("./beethoven/rtorch/brulee_function.R")
source("./beethoven/covariates_data_frame.R")

beethoven_locs <- readRDS("./beethoven/rtorch/data/beethoven_geom.rds")

beethoven_sf <- sf::st_as_sf(
  beethoven_locs,
  coords = c("lon", "lat"),
  crs = 4326
)
beethoven_sf
beethoven_proj <- sf::st_transform(beethoven_sf, 5070)
plot(beethoven_proj$geometry)

us <- tigris::states(cb = TRUE)
us <- us[us$NAME %in% c(state.name, "District of Columbia"), ]
us <- us[!us$NAME %in% c("Alaska", "Hawaii"), ]
us_proj <- sf::st_transform(us, 5070)
plot(us_proj$geometry)
plot(beethoven_proj$geometry, add = TRUE)

nrow(beethoven_proj)

beethoven_folds <- spatial_clustering_cv(
  beethoven_proj[1:10000, ],
  v = 10
)

beethoven_train <- training(beethoven_folds)

extract_splits <- function(spatial_cv) {
  splits <- spatial_cv$splits
  folds <- lapply(splits, function(split) {
    train_data <- rsample::training(split)
    test_data <- rsample::testing(split)
    list(train_data = train_data, test_data = test_data)
  })
  return(folds)
}

folds <- extract_splits(beethoven_folds)

beethoven_train <- folds[[1]]$train_data
beethoven_train <- st_drop_geometry(beethoven_train)
head(beethoven_train)
beethoven_test <- folds[[1]]$test_data
beethoven_test <- st_drop_geometry(beethoven_test)

?assessment
class(beethoven_train$time)
?step_time
?step_timeseries
