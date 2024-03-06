# function to create single data frame for all covariates

covariates_data_frame <-
  function(
      covariates,
      type = "rds",
      id = "site_id",
      dates = c("2018-01-01", "2022-01-01")) {
    # define dates of interest
    dates_of_interest <- seq(
      as.Date(dates[1]),
      as.Date(dates[2]),
      by = 1
    )
    for (c in seq_along(covariates)) {
      # import data from rds files
      if (type == "rds") {
        data <- readRDS(covariates[c])
        # import data from R objects
      } else {
        data <- covariates[c]
      }
      # process time dimension of data
      data_time <- covariates_time(
        data,
        id = id,
        dates = dates_of_interest
      )
      # filter to dates of interest
      data_time_filter <- data_time[
        which(data_time$time %in% dates_of_interest),
      ]
      # create merge identifier based on site and date
      data_time_filter$merge <- paste0(
        data_time_filter[,id],
        data_time_filter$time
      )
      if (c == 1) {
        covariates_df <- data_time_filter
      } else if (c > 1) {
        covariates_df <- merge(
          covariates_df,
          data_time_filter,
          by = "merge"
        )
        # remove redundant time and identifier columns
        drop_y <- c("time.y", paste0(id, ".y"))
        covariates_df <-
          covariates_df[, !(colnames(covariates_df) %in% drop_y)]
        # rename ".x" columns from merge
        colnames(covariates_df) <- gsub(".x", "", colnames(covariates_df))
      }
    }
    # retain only one identifier column
    drop_id <- c("merge", "lon", "lat", "geom")
    covariates_df_1 <-
     covariates_df[, !(colnames(covariates_df) %in% drop_id)]
    return(covariates_df_1)
  }

covariates_time <-
  function(
      data,
      id = "site_id",
      dates) {
    n <- colnames(data)
    if (!(id %in% n)) {
      stop(paste0("Identifier not detected in column name.\n"))
    }
    if ("time" %in% n) {
      cat(paste0("'time' defined data detected...\n"))
      return(data)
    }
    if ("year" %in% n) {
      cat(paste0("'year' defined data detected...\n"))
      years <- as.integer(data$year)
      data_dates <- NULL
      for (y in seq_along(years)) {
        data_year <- data[data$year == years[y]]
        dates_year <- seq(
          as.Date(paste0(years[y], "01-01")),
          as.Date(paste0(years[y], "12-31")),
          by = 1
        )
        data_merge <- merge(dates_year, data_year)
        colnames(data_merge)[1] <- "time"
        data_dates <- rbind(data_dates, data_merge)
      }
      return(data_dates)
    }
    if (all((c("time", "year") %in% n) == c(FALSE, FALSE))) {
      cat(paste0("Temporally undefined data detected...\n"))
      data_dates <- merge(dates, data)
      colnames(data_dates)[1] <- "time"
      return(data_dates)
    }
  }
