# function to implement brulee multilayer perceptron with hyperparameter tuning

#' Fit and tune multilayer perceptron
#' @description
#' Fit a multilayer perceptron with `workflows::workflow`, and tune
#' hyperparameters with `tune::tune`. Multilayer perceptron engine can be one
#' of "nnet", "keras", or "brulee".
#' @param recipe
#' @param train
#' @param test
#' @param epochs
#' @param hidden_units
#' @param activation
#' @param learn_rate
#' @param folds
#' @param importance
#' @param engine
#' @param outcome
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
#' @author Mitchell Manware
#' @returns
base_mlp <- function(
  recipe,
  train,
  test,
  epochs = c(25, 50, 75, 100),
  hidden_units = c(c(8, 8), c(8, 16), c(16, 16)),
  activation = c("relu"),
  learn_rate = c(0.01, 0.005, 0.001),
  folds = 5,
  importance,
  engine = c("keras", "nnet", "brulee"),
  outcome = c("regression", "classification"),
  ...
) {
  # check inputs
  stopifnot(methods::is(recipe, "recipe"))
  stopifnot(methods::is(train, "data.frame"))
  stopifnot(methods::is(test, "data.frame"))

  # set folds cross validation
  mlp_folds <- rsample::vfold_cv(train, v = folds)

  # create tuning grid
  mlp_grid <- expand.grid(
    epochs = epochs,
    hidden_units,
    activation = activation,
    learn_rate = learn_rate,
    ...
  )

  # initiate multilayer perceptron with `parsnip`
  mlp_fit <- parsnip::mlp(
    epochs = tune::tune(),
    hidden_units = tune::tune(),
    activatiion = tune::tune(),
    learn_rate = tune::tune(),
    ...
  ) |>
    parsnip::set_engine(engine, importance = importance) |>
    parsnip::set_mode(outcome)

  # implement recipe to `mlp_fit` with `workflows::workflow`
  mlp_workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(mlp_fit)

  # apply tuning grid to workflow
  mlp_tune <- tune::tune_grid(
    mlp_workflow,
    resamples = mlp_folds,
    grid = mlp_grid
  )

  # select best hyperparameters
  mlp_best <- tune::select_best(mlp_tune, method = "rmse")
  cat("Optimal hyperparameters:\n")
  print(mlp_best)

  # best hyperparameters as list
  mlp_best_list <- as.list(mlp_best)
  mlp_best_list$hidden_units <- unlist(mlp_best_list$hidden_units)

  # apply best hyperparameters to new workflow
  mlp_workflow_best <- mlp_workflow |>
    workflows::finalize_workflow(mlp_best_list)

  # fit best model
  mlp_fit <- parsnip::fit(mlp_workflow_best, train)

  # predict with test data
  mlp_prediction <- predict(mlp_fit, test)

  # return prediction results
  return(mlp_prediction)
}
