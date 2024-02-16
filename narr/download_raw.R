#### Steps to import raw weasd and omega data
devtools::load_all("~/amadeus")

download_data(
  dataset_name = "narr_monolevel",
  variables = "weasd",
  year_start = 2018,
  year_end = 2018,
  directory_to_save = "../data/",
  data_download_acknowledgement = TRUE,
  download = TRUE,
  remove_command = TRUE
)

download_data(
  dataset_name = "narr_p_levels",
  variables = "omega",
  year_start = 2018,
  year_end = 2018,
  directory_to_save = "../data/",
  data_download_acknowledgement = TRUE,
  download = TRUE,
  remove_command = TRUE
)
# retain only 201801 omega data
file.remove(
  grep(
    pattern = "201801",
    list.files("../"),
    invert = TRUE,
    value = TRUE
  )
)
