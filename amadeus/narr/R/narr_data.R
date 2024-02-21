#### load devtools
library(devtools)
#### import amadeus from GitHub
install_github("Spatiotemporal-Exposures-and-Toxicology/amadeus")

#### weasd
download_data(
  dataset_name = "narr_monolevel",
  variables = "weasd",
  year_start = 2018,
  year_end = 2018,
  directory_to_save = "../data/raw/",
  data_download_acknowledgement = TRUE,
  download = TRUE,
  remove_command = TRUE
)

#### omega
download_data(
  dataset_name = "narr_p_levels",
  variables = "omega",
  year_start = 2018,
  year_end = 2018,
  directory_to_save = "../data/raw/",
  data_download_acknowledgement = TRUE,
  download = TRUE,
  remove_command = TRUE
)
#### retain only January 2018 data
file.remove(
  grep(
    pattern = "201801",
    list.files("../"),
    invert = TRUE,
    value = TRUE
  )
)
