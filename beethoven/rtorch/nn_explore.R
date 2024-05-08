# explore creating torch tensor with sample covariate data
# March 06, 2024
# https://torch.mlverse.org/docs/articles/examples/dataset

## libraries and functions
setwd("/Volumes/manwareme/workbench/amadeus/rtorch")
library(torch)
library(luz)
library(data.table)
# devtools::load_all("/Volumes/SET/Projects/NRT-AP-Model")
source("../../beethoven/covariates_data_frame.R")


## import covariate data
cov <- covariates_data_frame(
  covariates = list.files("data/", full.names = TRUE),
  type = "rds",
  dates = c("2020-12-01", "2021-01-31"),
  sample = 10
)
# summary
summary(cov)
head(cov)
tail(cov)
# check for NAs
all(!is.na(cov))
# generate PM2.5 outcome data
cov$pm2.5 <- rgamma(10 * 62, 32, 1.6)

# covariate columns
xcol <- which(!(colnames(cov) %in% c("site_id", "time", "pm2.5")))
# response column
ycol <- which(colnames(cov) == "pm2.5")


## torch dataset
model_dataset <- torch::dataset(
  name = "model_dataset",
  
  initialize = function(data, xcol, ycol) {
    self$data <- data
    self$x <- self$data[, xcol]
    self$y <- self$data[, ycol]
  },
  
  .getitem = function(index) {
    xtensor <- 
      self$x[index, ] |>
      as.matrix() |>
      torch::torch_tensor(dtype = torch::torch_float()) |>
      torch::torch_reshape(shape = list(1, ncol(self$x)))
    ytensor <- 
      self$y[index] |>
      as.matrix() |>
      torch::torch_tensor(dtype = torch::torch_float()) |>
      torch::torch_reshape(shape = list(1, 1))
    list(x = xtensor, y = ytensor)
  },
  
  .length = function() {
    nrow(self$y)
  }
)

## training and validation sample
# training and validation indicies
train_i <- sample(1:nrow(cov), size = floor(0.8 * nrow(cov)))
valid_i <- setdiff(1:nrow(cov), train_i)

# training data
train_ds <- model_dataset(cov[train_i,], xcol, ycol)
train_ds$.getbatch(1)
train_dl <-
  train_ds |>
  dataloader(batch_size = 128, shuffle = TRUE)

# validation data
valid_ds <- model_dataset(cov[valid_i,], xcol, ycol)
valid_ds$.getbatch(1)
valid_dl <-
  valid_ds |>
  dataloader(batch_size = 128, shuffle = FALSE)


## create model
model <- nn_module(
  "testConv2d",
  initialize = function() {
    self$conv1 <- nn_conv2d(4, 16, 5, 2) # N * 4 * 50 * 60
    self$conv2 <- nn_conv2d(16, 48, 3, 1) # N * 16 * 23 * 28
    # self$dropout1 <- nn_dropout2d(0.25)
    # self$dropout2 <- nn_dropout2d(0.33)
    self$fc1 <- nn_linear(4928, 100)
    self$fc2 <- nn_linear(100, 1)
  },
  forward = function(input) {
    input %>%
      self$conv1() %>%
      nnf_relu() %>%
      self$conv2() %>%
      nnf_relu() %>%
      nnf_max_pool2d(2) %>%
      self$fc1() %>%
      nnf_relu() %>%
      self$fc2()
    
  }
)


## fit model
fitted <-
  model %>%
  luz::setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam,
    metrics = list(
      luz_metric_rmse()
    )
  ) %>%
  fit(data = train_dl, epochs = 10)


luz::set



?fit
