# Mitchell Manware
# May 17, 2024

getwd()
source("./beethoven/rtorch/brulee_function.R")
source("./beethoven/covariates_data_frame.R")

mlps <- list.files("./beethoven/rtorch/data/model_output/", full.names = TRUE)
mlps
beethoven_mlp <- readRDS(mlps[5])

# inspect performance
yardstick::metrics(beethoven_mlp[[2]], pm2.5, .pred)

# inspect model
beethoven_mlp[[1]]
head(beethoven_mlp[[2]])
nrow(beethoven_mlp[[2]])
length(unique(beethoven_mlp[[2]]$site_id))

beethoven_obs_pred <- data.frame(
  pm2.5 = beethoven_mlp[[2]]$pm2.5,
  pred = beethoven_mlp[[2]]$.pred
)
head(beethoven_obs_pred)


ggplot(
  data = beethoven_mlp[[2]],
  aes(x = pm2.5, y = .pred)
) +
  geom_pointdensity() +
  scale_color_gradientn(
    colors = matlab.like(11),
    name = "Density Estimation"
  ) +
  geom_smooth(method = "lm", se = TRUE, col = "black") +
  labs(
    x = "Observed PM2.5",
    y = "Predicted PM2.5"
  ) +
  theme_pubr() +
  theme(legend.position = "right")

beethoven_mlp[[2]]

beethoven_mlp

# model with simple fold cross validation
beethoven_mlp_vfold <- readRDS(mlps[1])

# inspect performance
yardstick::metrics(beethoven_mlp_vfold[[2]], pm2.5, .pred)

# inspect model
beethoven_mlp_vfold[[1]]
head(beethoven_mlp_vfold[[2]])
nrow(beethoven_mlp_vfold[[2]])
length(unique(beethoven_mlp_vfold[[2]]$site_id))

ggplot(
  data = beethoven_mlp_vfold[[2]],
  aes(x = pm2.5, y = .pred)
) +
  geom_pointdensity() +
  scale_color_gradientn(
    colors = matlab.like(11),
    name = "Density Estimation"
  ) +
  geom_smooth(method = "lm", se = TRUE, col = "black") +
  labs(
    x = "Observed PM2.5",
    y = "Predicted PM2.5"
  ) +
  theme_pubr() +
  theme(legend.position = "right")

ggplot(
  data = beethoven_mlp_vfold[[2]],
  aes(x = pm2.5, y = .pred)
) +
  geom_pointdensity() +
  scale_color_gradientn(
    colors = matlab.like(11),
    name = "Density Estimation"
  ) +
  geom_smooth(method = "lm", se = TRUE, col = "black") +
  labs(
    x = "Observed PM2.5",
    y = "Predicted PM2.5"
  ) +
  theme_pubr() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 15),
    legend.key.size = unit(2, "lines"),
    legend.key.height = unit(3, "lines"),
    legend.key.width = unit(2, "lines")
  )
