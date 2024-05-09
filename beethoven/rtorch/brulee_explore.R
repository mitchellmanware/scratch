setwd("../rtorch/")

# packages
# install.packages(
#   c("brulee", "recipes", "yardstick", "workflow",
#     "ggplot2", "torch", "tune", "tidymodels")
# )
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

# check torch install
torch::install_torch()

# import geos data as covariate samples
geos <- readRDS(
  "/ddn/gs1/group/set/Projects/NRT-AP-Model/output/NRTAP_Covars_GEOS.rds"
)
head(geos)
summary(geos)
length(unique(geos$site_id))
length(unique(geos$time))

# generate PM2.5 outcome data
geos$pm2.5 <- rgamma(nrow(geos), 32, 1.6)
head(geos)

# data columns
col_i <- colnames(geos)[which(!(colnames(geos) %in% c("time", "site_id")))]
col_i

# split train and test data
train_i <- sample(nrow(geos), size = floor(0.8 * nrow(geos)))
train <- geos[train_i, ]
test <- geos[-train_i, ]
nrow(test) + nrow(train) == nrow(geos)

# build recipe
geos_recipe <- recipes::recipe(
  data = geos[, 3:ncol(geos)],
  pm2.5 ~ .
) %>%
  step_normalize(all_predictors())
print(geos_recipe$var_info, n = 56)

###############################################################################
# manual hyperparameter tuning
fit_manual <- brulee::brulee_mlp(
  geos_recipe,
  data = train,
  epochs = 10,
  hidden_units = c(2, 2),
  activation = "relu",
  learn_rate = 0.01
)

# predict with test data
pred_manual <-
  predict(fit_manual, test) %>%
  bind_cols(test)

# check r squared
rsq(geos_mlp, truth = pm2.5, estimate = .pred)

# plot
ggplot(
  data = geos_mlp,
  aes(x = .pred, y = pm2.5)
) +
  geom_abline(col = "#3f7ba6") +
  geom_point(alpha = 0.3)


###############################################################################
# hyperparameter tuning with `tune`
# validation folds
geos_folds <- vfold_cv(train, v = 5)
geos_folds

# create tuning grid with hyperparameter selections
geos_grid <- expand.grid(
  hidden_units = list(c(8, 8), c(8, 16), c(16, 16)),
  epochs = c(25, 50, 75, 100),
  learn_rate = c(0.001, 0.005, 0.01)
)
geos_grid

geos_grid <- expand.grid(
  hidden_units = list(c(2, 2), c(2, 4)),
  epochs = c(25),
  learn_rate = c(0.01)
)
geos_grid

# initate mlp parsnip
# use brulee engine
geos_mlp <- mlp(
  hidden_units = tune(),
  activation = "relu",
  epochs = tune(),
  learn_rate = tune()
) %>%
  set_engine("brulee", importance = "permutation") %>%
  set_mode("regression")

# apply recipe to mlp with workflows
geos_workflow <- workflow() %>%
  add_recipe(geos_recipe) %>%
  add_model(geos_mlp)

# apply tune grid with hyperparameter combinations
geos_tune <- tune_grid(
  geos_workflow,
  resamples = geos_folds,
  grid = geos_grid
)

geos_best <- tune::select_best(geos_tune, metric = "rmse")
geos_best$hidden_units

cat("Best model:\n")
print(geos_best)


geos_best_list <- as.list(geos_best)
geos_best_list$hidden_units <- unlist(geos_best_list$hidden_units)
geos_best_list

geos_finalize <- finalize_workflow(geos_workflow, geos_best_list)
geos_finalize

geos_mlp_finalize <- parsnip::fit(geos_finalize, train)

geos_pred <-
  predict(geos_mlp_finalize, test) |>
  bind_cols(test)
geos_pred

ggplot(
  data = geos_pred,
  aes(x = .pred, y = pm2.5)
) +
  geom_abline(col = "#3f7ba6") +
  geom_point(alpha = 0.3)

# implement new function with sample data (geos covariates only)
geos_mlp <- base_mlp(
  recipe = geos_recipe,
  train = train,
  test = test,
  epochs = 5,
  hidden_units = list(c(2, 2), c(4, 4)),
  activation = "relu",
  learn_rate = 0.01,
  folds = 5,
  importance = "permutations",
  engine = "brulee",
  outcome = "regression"
)
