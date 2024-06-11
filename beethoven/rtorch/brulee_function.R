#' Fit and tune multilayer perceptron
#' @description
#' Fit a multilayer perceptron with `workflows::workflow`, and tune
#' hyperparameters with `tune::tune`. Multilayer perceptron engine can be one
#' of "nnet", "keras", or "brulee".
#' @param recipe
#' @param train
#' @param test
#' @param epochs vector
#' @param hidden_units list
#' @param activation vector
#' @param learn_rate vector
#' @param cv
#' @param folds
#' @param importance
#' @param engine
#' @param outcome
#' @param metric
#' @param ...
#' @importFrom methods is
#' @importFrom rsample vfold_cv
#' @importFrom parsnip mlp
#' @importFrom tune tune
#' @importFrom parsnip set_engine
#' @importFrom parsnip set+mode
#' @importFrom workflows workflow
#' @importFrom workflows add_recipe
#' @importFrom workflows add_model
#' @importFrom tune tune_grid
#' @importFrom tune select_best
#' @importFrom workflows finalize_workflow
#' @importFrom parsnip fit
#' @importFrom dplyr bind_cols
#' @author Mitchell Manware
#' @returns
#' @export
base_mlp <- function(
  recipe,
  train,
  test,
  epochs = c(100, 150, 200),
  hidden_units = list(c(8, 8), c(8, 16), c(16, 16)),
  activation = c("relu"),
  learn_rate = c(0.01, 0.005, 0.001),
  cv = c("cluster", "block", "lolo", "buffer", "nndm"),
  folds = 5,
  importance,
  engine = c("keras", "nnet", "brulee"),
  outcome = c("regression", "classification"),
  metric = "rmse",
  n_clusters = NULL,
  n_blocks = NULL,
  buffer_distance = NULL,
  n_neighbors = NULL,
  locs_id = NULL,
  ...
) {
  # check inputs
  stopifnot(methods::is(recipe, "recipe"))
  stopifnot(methods::is(train, "data.frame"))
  stopifnot(methods::is(test, "data.frame"))
  stopifnot(activation %in% c("relu"))
  stopifnot(engine %in% c("keras", "nnet", "brulee"))
  stopifnot(outcome %in% c("regression", "classification"))

  # set folds cross validation
  mlp_folds <- switch(
    cv,
    cluster = cluster_cv_man(train, n_clusters, folds),
    block = block_cv_man(train, n_blocks),
    lolo = lolo_cv_man(train, locs_id),
    buffer = buffer_cv_man(train, buffer_distance, folds),
    nndm = nndm_cv_man(train, n_neighbors, folds),
    stop("Unknown cross-validation type")
  )

  cat(
    paste0(
      "Utilizing ",
      cv,
      " spatiotemporal cross validation...\n" 
    )
  )

  # drop cluster_id column after cv sampling
  cluster_ids <- c(
    "cluster", "block", "location", "buffer_id", "buffer_cluster", "nndm"
  )
  cluster_ids_drop <- cluster_ids[which(cluster_ids %in% names(train))]
  train <- train |> dplyr::select(-dplyr::one_of(cluster_ids_drop))

  # create tuning grid
  mlp_grid <- expand.grid(
    list(
      epochs = epochs,
      hidden_units = hidden_units,
      activation = activation,
      learn_rate = learn_rate,
      ...
    )
  )

  # apply `tune` flexibility to all parameters
  mlp_parameters <- lapply(
    names(mlp_grid),
    function(param) {
      setNames(list(tune::tune()), param)
    }
  )

  cat(
    "Initializing multilayer perceptron model with tunable hyperparameters...\n"
  )

  # intiate multilayer perceptron with tunable parameters
  mlp_fit <- do.call(
    parsnip::mlp,
    unlist(mlp_parameters)
  ) |>
    parsnip::set_engine(engine = engine, importance = importance) |>
    parsnip::set_mode(outcome)

  cat("Applying recipe to model workflow...\n")

  # implement recipe to `mlp_fit` with `workflows::workflow`
  mlp_workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(mlp_fit)

  cat(
    paste0(
      "Fitting ",
      nrow(mlp_grid),
      " unique model(s)...\n"
    )
  )

  # apply tuning grid to workflow
  mlp_tune <- tune::tune_grid(
    mlp_workflow,
    resamples = mlp_folds,
    grid = mlp_grid
  )

  cat("Selecting optimal hyperparameters...\n")

  # select best hyperparameters
  mlp_best <- tune::select_best(mlp_tune, metric = metric)
  cat("Optimal hyperparameters:\n")
  print(mlp_best)

  # best hyperparameters as list
  mlp_best_list <- as.list(mlp_best)
  mlp_best_list$hidden_units <- unlist(mlp_best_list$hidden_units)

  cat("Refitting model with optimal hyperparamters...\n")

  # apply best hyperparameters to new workflow
  mlp_workflow_best <- mlp_workflow |>
    tune::finalize_workflow(mlp_best_list)

  # fit best model
  mlp_fit_best <- parsnip::fit(mlp_workflow_best, sf::st_drop_geometry(train))

  cat("Predicting outcomes with refit model and test data...\n")

  # predict with test data
  mlp_prediction <- predict(mlp_fit_best, sf::st_drop_geometry(test)) |>
    dplyr::bind_cols(test)

  # measure performance
  mlp_performance <- data.frame(
    yardstick::metrics(mlp_prediction, pm2.5, .pred)
  )

  # create plot for performance visualization
  mlp_plot <-
    ggplot2::ggplot(
      data = mlp_prediction,
      aes(x = pm2.5, y = .pred)
    ) +
    ggpointdensity::geom_pointdensity() +
    ggplot2::scale_color_gradientn(
      colors = colorRamps::matlab.like(11),
      name = "Density Estimation"
    ) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, col = "black") +
    ggplot2::labs(
      x = "Observed PM2.5",
      y = "Predicted PM2.5"
    ) +
    ggpubr::theme_pubr() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::annotate(
      "label",
      x = 0,
      y = Inf,
      label = paste0(
        "CV = ", cv,
        "\nR² = ", round(mlp_performance[2, 3], 3),
        "\nRMSE = ", round(mlp_performance[1, 3], 3),
        "\nMAE = ", round(mlp_performance[3, 3], 3)
      ),
      hjust = 0,
      vjust = 1.1,
      size = 5,
      color = "black",
      fill = "white",
      label.size = 0.5
    )

  # return prediction results
  return(list(mlp_workflow_best, mlp_fit_best, mlp_prediction, mlp_plot))
}

#' Prepare spatiotemporal recipe
#' @description
#' Prepare a spatiotemporal recipe with the `recipes` package for use in a
#' multilayer perceptron.
#' @param data A `data.frame` or `sf` object containing the dataset to be
#' preprocessed.
#' @param outcome_id character(1). Name of the outcome variable.
#' @param locs_id character(1). Name of the location identifier variable.
#' @param time_id character(1). Name of the time identifier variable.
#' Defaults to "time".
#' @return A `recipe` object
#' @author Mitchell Manware
#' @importFrom methods is
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select
#' @import recipes
#' @export
st_recipe <- function(
  data,
  outcome_id,
  locs_id,
  time_id = "time"
) {
  # check inputs
  stopifnot(methods::is(data, "data.frame"))
  stopifnot(methods::is(c(locs_id, time_id), "character"))
  if (methods::is(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }

  # remove cluster column if in data
  if ("cluster" %in% names(data)) {
    data <- data |> dplyr::select(all_of(-cluster))
  }
  # create recipe
  recipe_prepared <- recipes::recipe(
    data = data,
    as.formula(paste(outcome_id, "~ ."))
  )

  # prepare time as split factor
  if (!is.null(time_id)) {
    stopifnot(methods::is(time_id, "character"))
    recipe_prepared <- recipe_prepared |>
      recipes::step_date(
        time,
        features = c("dow", "month", "year"),
        keep_original_cols = FALSE
      ) |>
      recipes::step_mutate(time_year = as.factor(time_year)) |>
      recipes::step_dummy(time_dow, time_month, time_year)
  }

  # prepare locations as factor
  if (!is.null(locs_id)) {
    stopifnot(methods::is(locs_id, "character"))
    recipe_prepared <- recipe_prepared |>
      recipes::step_dummy(locs_id)
  }

  # remove zero variance predictors
  # normalize numeric predictors
  recipe_prepared <- recipe_prepared |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())

  # return recipe
  return(recipe_prepared)
}

# manual spatiotemporal cross validation functions
# spatial clustering cross validation
cluster_cv_man <- function(
  data,
  n_clusters,
  folds
) {
  # create spatial clusters
  kmeans_result <- kmeans(sf::st_coordinates(data), centers = n_clusters)
  data$cluster <- kmeans_result$cluster

  # sample spatially clustered data
  folds <-
    rsample::group_vfold_cv(data, group = "cluster", v = as.numeric(folds))

  return(folds)
}

# block cross validation
block_cv_man <- function(
  data,
  n_blocks
) {
  # Calculate bounding box of the study area
  bbox <- sf::st_bbox(data)

  # Calculate block sizes based on the number of blocks
  x_block_size <- (bbox["xmax"] - bbox["xmin"]) / as.numeric(n_blocks)
  y_block_size <- (bbox["ymax"] - bbox["ymin"]) / as.numeric(n_blocks)

  # Assign blocks based on coordinates and block sizes
  data$block <- as.factor(
    floor((sf::st_coordinates(data)[, 1] - bbox["xmin"]) / x_block_size) +
    floor((sf::st_coordinates(data)[, 2] - bbox["ymin"]) / y_block_size) *
    as.numeric(n_blocks)
  )

  # Perform group k-fold cross-validation based on the blocks
  folds <- rsample::group_vfold_cv(
    data, group = "block", v = length(unique(data$block))
  )

  return(folds)
}

# leave one location out cross validation
lolo_cv_man <- function(
  data,
  locs_id
) {
  data$location <- data[[locs_id]]
  folds <- rsample::group_vfold_cv(
    data,
    group = "location",
    v = length(unique(data$location))
  )
  return(folds)
}

# buffer cross validation
buffer_cv_man <- function(
  data,
  buffer_distance,
  folds
) {
  data$buffer_id <- seq_len(nrow(data))
  buffer_zones <- sf::st_buffer(data, dist = as.numeric(buffer_distance))
  data$buffer_cluster <- apply(
    sf::st_intersects(data, buffer_zones, sparse = FALSE),
    1,
    which
  )
  folds <- rsample::group_vfold_cv(
    data,
    group = "buffer_cluster",
    v = length(unique(data$buffer_cluster))
  )
  return(folds)
}

# nearest neighbor distance matching cross validation
nndm_cv_man <- function(
  data,
  n_neighbors,
  folds
) {
  coords <- sf::st_coordinates(data)
  dist_matrix <- as.matrix(dist(coords))
  data$nndm <- apply(dist_matrix, 1, function(row) {
    which(row <= sort(row)[n_neighbors + 1])
  })
  folds <- rsample::group_vfold_cv(
    data,
    group = "nndm",
    v = length(unique(data$nndm))
  )
  return(folds)
}

extract_splits <- function(spatial_cv) {
  splits <- spatial_cv$splits
  cluster_ids <- c(
    "cluster", "block", "location", "buffer_id", "buffer_cluster", "nndm"
  )
  folds <- lapply(splits, function(split) {
    train_data <- rsample::training(split)
    test_data <- rsample::testing(split)

    # Remove cluster-related columns from training and testing data
    train_data <- train_data |> dplyr::select(-dplyr::any_of(cluster_ids))
    test_data <- test_data |> dplyr::select(-dplyr::any_of(cluster_ids))

    list(train_data = train_data, test_data = test_data)
  })
  return(folds)
}

st_cross_validation <- function(
  data,
  cv_type,
  folds,
  n_clusters = NULL,
  n_blocks = NULL,
  buffer_distance = NULL,
  n_neighbors = NULL,
  locs_id = NULL
) {
  # Check for spatial inputs
  stopifnot(methods::is(data, "sf"))

  # Spatial clustering and cross-validation
  folds <- switch(
    cv_type,
    cluster = cluster_cv_man(data, n_clusters, folds),
    block = block_cv_man(data, n_blocks),
    lolo = lolo_cv_man(data, locs_id),
    buffer = buffer_cv_man(data, buffer_distance, folds),
    nndm = nndm_cv_man(data, n_neighbors, folds),
    stop("Unknown cross-validation type")
  )

  # Extract splits
  splits <- extract_splits(folds)

  # Return splits
  return(splits)
}

temporal_split <- function(
  data,
  time_id = "time",
  prop
) {
  # check inputs
  stopifnot(methods::is(data, "data.frame"))
  stopifnot(prop > 0 & prop < 1)

  # extract time data
  time <- sort(unique(data[[time_id]]))

  # split time
  train_time <- time[1:floor((length(time) * prop))]
  test_time <- as.Date(setdiff(time, train_time))

  # split data based on time
  train_data <- data[data[[time_id]] %in% train_time, ]
  test_data <- data[data[[time_id]] %in% test_time, ]

  # return
  return(list(train_data, test_data))
}