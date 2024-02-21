# Mitchell Manware
# Template for creating a spatial subset of NCEP North American Regional
# Reanalysis (NARR) monolevel data using the `ncdf4` package
# Code sourced from
# https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-projected-netcdf-file

#####
# import libraries
library(ncdf4)
library(terra)

#####
# identify spatial subset extent
# import data with terra
weasd_terra <- terra::rast("../data/raw/weasd/weasd.2018.nc")
# import North Carolina counties
nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))
# subset to Durham, Wake and Orange counties
# project to NARR custom coordinate reference system
nc_p <- terra::project(
  nc[nc$NAME %in% c("Durham", "Wake", "Orange")],
  terra::crs(weasd_terra)
)
# define subset extent
sub_ext <- terra::ext(nc_p)
# check plot
terra::plot(sub_ext, border = "blue")
terra::plot(nc_p, add = TRUE, border = "red")

#####
# define file variables
# raw netCDF file downloaded in narr_data.R
in_name <- "../data/raw/weasd/weasd.2018.nc"
# netCDF file to be created in a new folder
out_name <- "../data/subset/weasd/weasd.2018.nc"
# variable of interest
variable <- "weasd"

#####
# open raw netCDF
ncin <- nc_open(in_path)
# inspect contents
print(ncin)

#####
# get x variable and attributes (long name and units)
x <- ncvar_get(ncin, "x")
xlname <- ncatt_get(ncin, "x", "long_name")
xunits <- ncatt_get(ncin, "x", "units")
# store dimensions of x
nx <- dim(x)
head(x)

# get y variable and attributes (long name and units)
y <- ncvar_get(ncin,"y")
ylname <- ncatt_get(ncin, "y", "long_name")
yunits <- ncatt_get(ncin, "y", "units")
# store dimensions of y
ny <- dim(y)
head(y)

# get time variable and units
time <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
# store dimensions of time
nt <- dim(time)
nt

# get variable of interest (weasd) and attributes
# weasd_array stores the data values as a three dimensional array ([x, y, time])
weasd_array <- ncvar_get(ncin, variable)
dlname <- ncatt_get(ncin, variable, "long_name")
dunits <- ncatt_get(ncin, variable, "units")
fillvalue <- ncatt_get(ncin, variable, "_FillValue")
dim(weasd_array)

# get lon and lat variables
lon <- ncvar_get(ncin, "lon")
dim(lon)
lat <- ncvar_get(ncin, "lat")
dim(lat)

#####
# subset x and y values according to sub_ext
x <- x[which(x > sub_ext[1] & x < sub_ext[2])]
y <- y[which(y > sub_ext[3] & y < sub_ext[4])]
# re-store x and y dimensions after subsetting
nx <- dim(x)
ny <- dim(y)
# subset the array with data values according to sub_ext
weasd_array <- weasd_array[
  which(x > sub_ext[1] & x < sub_ext[2]),     # x values in range
  which(y > sub_ext[3] & y < sub_ext[4]),     # y values in range
  ]                                           # all time values
# subset lon and lat values according to sub_ext
lon <- lon[
  which(x > sub_ext[1] & x < sub_ext[2]), 
  which(y > sub_ext[3] & y < sub_ext[4])]
lat <- lat[
  which(x > sub_ext[1] & x < sub_ext[2]),
  which(y > sub_ext[3] & y < sub_ext[4])]

#####
# get coordinate reference system attributes
grid_mapping_name <- ncatt_get(
  ncin,
  "Lambert_Conformal",
  "grid_mappping_name")
standard_parallel <- ncatt_get(
  ncin,
  "Lambert_Conformal",
  "standard_parallel")
longitude_of_central_meridian <- ncatt_get(
  ncin,
  "Lambert_Conformal",
  "longitude_of_central_meridian")
latitude_of_projection_origin <- ncatt_get(
  ncin,
  "Lambert_Conformal",
  "latitude_of_projection_origin")
false_easting <- ncatt_get(
  ncin,
  "Lambert_Conformal",
  "false_easting")
false_northing <- ncatt_get(
  ncin,
  "Lambert_Conformal",
  "false_northing")

#####
# define the x, y, and time dimensions for the new netCDF
# xdim and ydim will include only the subsetted region
xdim <- ncdim_def(
  "x",
  units = "m",
  longname = paste0(
    "eastward distance from southwest corner ",
    "of domain in projection coordinates"
    ),
  as.double(x)
)
ydim <- ncdim_def(
  "y",
  units = "m",
  longname = paste0(
    "northward distance from southwest corner ",
    "of domain in projection coordinates")
  ,
  as.double(y)
)
# timedim will retain all time dimensions
timedim <- ncdim_def(
  "time",
  tunits$value,
  as.double(time)
)

#####
# define variables
fillvalue <- 1e32
weasd_name <- "Daily Accumulated Snow at Surface"
weasd_def <- ncvar_def(
  "weasd",
  dunits$value,
  list(xdim, ydim, timedim),
  fillvalue,
  weasd_name,
  prec = "single")
lon_name <- "Longitude of cell center"
lon_def <- ncvar_def(
  "lon", 
  "degrees_east",
  list(xdim, ydim), 
  NULL, 
  lon_name,
  prec = "double")
lat_name <- "Latitude of cell center"
lat_def <- ncvar_def(
  "lat",
  "degrees_north", 
  list(xdim, ydim),
  NULL, 
  lat_name,
  prec = "double")
proj_name <- "Lambert_Conformal"
proj_def <- ncvar_def(
  "Lambert_Conformal", 
  "1", 
  NULL,
  NULL, 
  longname = proj_name,
  prec = "char")

#####
# create new netCDF file with weasd, longitude, latitude, and projection
# variable definitions (creates slot for variable but does not create the variable)
ncout <- nc_create(
  out_path,
  list(weasd_def, lon_def, lat_def, proj_def),
  force_v4 = FALSE
  )

# put the variables into their "slots" as defined in `nc_create()`
ncvar_put(ncout, weasd_def, weasd_array)
ncvar_put(ncout, lon_def, lon)
ncvar_put(ncout, lat_def, lat)

# put additional dimension attributes
ncatt_put(ncout, "x", "axis", "X")
ncatt_put(ncout, "x", "standard_name", "projection_x_coordinate")
ncatt_put(ncout, "x", "_CoordinateAxisType", "GeoX")
ncatt_put(ncout, "y", "axis", "Y")
ncatt_put(ncout, "y", "standard_name", "projection_y_coordinate")
ncatt_put(ncout, "y", "_CoordinateAxisType", "GeoY")
ncatt_put(ncout, "weasd", "grid_mapping", "Lambert_Conformal")
ncatt_put(ncout, "weasd", "coordinates", "lat lon")

# put the coordinate reference system attributes
projname <- "lambert_conformal_conic"
false_easting <- 5632642.22547
false_northing <- 4612545.65137
ncatt_put(
  ncout, 
  "Lambert_Conformal",
  "name", 
  projname)
ncatt_put(
  ncout,
  "Lambert_Conformal",
  "long_name",
  projname)
ncatt_put(
  ncout, 
  "Lambert_Conformal",
  "grid_mapping_name",
  projname)
ncatt_put(
  ncout, 
  "Lambert_Conformal",
  "longitude_of_central_meridian",
  as.double(longitude_of_central_meridian$value))
ncatt_put(
  ncout,
  "Lambert_Conformal",
  "latitude_of_projection_origin",
  as.double(latitude_of_projection_origin$value))
ncatt_put(
  ncout, 
  "Lambert_Conformal", 
  "standard_parallel", 
  c(50.0, 50.0))
ncatt_put(
  ncout, 
  "Lambert_Conformal", 
  "false_easting",
  false_easting)
ncatt_put(
  ncout, 
  "Lambert_Conformal", 
  "false_northing",
  false_northing)
ncatt_put(
  ncout, 
  "Lambert_Conformal", 
  "_CoordinateTransformType",
  "Projection")
ncatt_put(
  ncout, 
  "Lambert_Conformal",
  "_CoordinateAxisTypes", 
  "GeoX GeoY")

# put global descriptor attributes
ncatt_put(ncout, 0, "title", "test output of projected data")
ncatt_put(ncout, 0, "institution", "NOAA ESRL PSD")
ncatt_put(ncout, 0, "source", "weasd")
history <- paste("P.J. Bartlein", date(), sep = ", ")
ncatt_put(ncout, 0, "history", history)
ncatt_put(ncout, 0, "Conventions", "CF=1.6")

#####
# inspect the created netCDF
ncout
# close connections
nc_close(ncout)
nc_close(ncin)

#####
# read new netCDF with `terra`
terra::rast(out_path)
