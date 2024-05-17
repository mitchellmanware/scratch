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
  epochs = c(25, 50, 75, 100),
  hidden_units = list(c(8, 8), c(8, 16), c(16, 16)),
  activation = c("relu"),
  learn_rate = c(0.01, 0.005, 0.001),
  folds = 5,
  importance,
  engine = c("keras", "nnet", "brulee"),
  outcome = c("regression", "classification"),
  metric,
  ...
) {
  # check inputs
  stopifnot(methods::is(recipe, "recipe"))
  stopifnot(
    methods::is(train, "data.frame"),
    methods::is(test, "data.frame")
  )
  stopifnot(activation %in% c("relu"))
  stopifnot(engine %in% c("keras", "nnet", "brulee"))
  stopifnot(outcome %in% c("regression", "classification"))

  # split train and test data
  # train_i <- sample(nrow(data), size = floor(0.8 * nrow(data)))
  # train <- data[train_i, ]
  # test <- data[-train_i, ]

  # set folds cross validation
  mlp_folds <- rsample::vfold_cv(train, v = folds)

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
      " unique models...\n"
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
  mlp_fit_best <- parsnip::fit(mlp_workflow_best, train)

  cat("Predicting outcomes with refit model and test data...\n")

  # predict with test data
  mlp_prediction <- predict(mlp_fit_best, test) |>
    dplyr::bind_cols(test)

  # return prediction results
  return(list(mlp_workflow_best, mlp_prediction))
}

#' Prepare spatiotemporal recipe
#' @description
#' Prepare a spatiotemporal recipe with the `recipes` package for use in a
#' multilayer perceptron.
#' @param data
#' @param outcome_id character
#' @param locs_id character
#' @param time_id character
#' @author Mitchell Manware
#' @importFrom methods is
#' @importFrom recipes recipe
#' @returns
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

  # create recipe
  recipe_prepared <- recipes::recipe(
    data = data,
    as.formula(paste(outcome_id, "~ ."))
  ) |>
    recipes::update_role(time_id, locs_id, new_role = "ID") |>
    recipes::update_role(outcome_id, new_role = "outcome") |>
    recipes::step_zv(all_predictors()) |>
    recipes::step_normalize(all_predictors())

  return(recipe_prepared)
}
