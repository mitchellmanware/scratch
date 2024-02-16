# Mitchell Manware
# February 16, 2023
# Creating NARR omega test data set
# https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-projected-netcdf-file

# load packages
library(ncdf4)
library(terra)
devtools::load_all("~/amadeus")

# path and file name, set dname
ncpath <- "omega/"
ncname <- "omega.201801"
ncfname <- paste(ncpath, ncname, ".nc", sep = "")
ncfname3 <- paste(ncpath, ncname, "_V2", ".nc", sep = "")
dname <- "omega"
ncfname
ncfname3

ncin <- nc_open(ncfname)
print(ncin)

# get x's and y's
x <- ncvar_get(ncin,"x")
xlname <- ncatt_get(ncin, "x", "long_name")
xunits <- ncatt_get(ncin, "x", "units")
nx <- dim(x)
head(x)

y <- ncvar_get(ncin,"y")
ylname <- ncatt_get(ncin, "y", "long_name")
yunits <- ncatt_get(ncin, "y", "units")
ny <- dim(y)
head(y)

# get time
time <- ncvar_get(ncin, "time")
time

# get level (needed for omega pressure levels)
level <- ncvar_get(ncin, "level")
level

# get unit
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt

# get variable
omegam_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(omegam_array)

lon <- ncvar_get(ncin, "lon")
dim(lon)
lat <- ncvar_get(ncin, "lat")
dim(lat)


#### REWRITE AFTER SUBSET
o <- terra::rast("omega/omega.201801.nc")
o_crop <- terra::crop(
  o,
  co,
  snap = "out"
)
e <- ext(o_crop)
x <- x[which(x > e[1] & x < e[2])]
nx <- dim(x)
y <- y[which(y > e[3] & y < e[4])]
ny <- dim(y)
omegam_array <- omegam_array[which(x > e[1] & x < e[2]), which(y > e[3] & y < e[4]),,]
lon <- lon[which(x > e[1] & x < e[2]), which(y > e[3] & y < e[4])]
dim(lon)
lat <- lat[which(x > e[1] & x < e[2]), which(y > e[3] & y < e[4])]
dim(lat)
####


grid_mapping_name <- ncatt_get(ncin, "Lambert_Conformal", "grid_mappping_name")
standard_parallel <- ncatt_get(ncin, "Lambert_Conformal", "standard_parallel")
longitude_of_central_meridian <- ncatt_get(ncin, "Lambert_Conformal", "longitude_of_central_meridian")
latitude_of_projection_origin <- ncatt_get(ncin, "Lambert_Conformal", "latitude_of_projection_origin")
false_easting <- ncatt_get(ncin, "Lambert_Conformal", "false_easting")
false_northing <- ncatt_get(ncin, "Lambert_Conformal", "false_northing")

# create and write the netCDF file -- ncdf4 version
# define dimensions
xdim <- ncdim_def(
  "x",
  units = "m",
  longname = "eastward distance from southwest corner of domain in projection coordinates",
  as.double(x)
)
ydim <- ncdim_def(
  "y",
  units = "m",
  longname = "northward distance from southwest corner of domain in projection coordinates",
  as.double(y)
)
timedim <- ncdim_def(
  "time",
  tunits$value,
  as.double(time)
)
leveldim <- ncdim_def(
  "level",
  "millibar",
  as.double(level)
)

# define variables also include longitude and latitude and the CRS variable
fillvalue <- 1e32
dlname <- "Daily Omega on Pressure Levels"
omega_def <- ncvar_def("omega",dunits$value,list(xdim,ydim,leveldim,timedim),fillvalue,dlname,prec="single")
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon", "degrees_east", list(xdim, ydim), NULL, dlname, prec = "double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat", "degrees_north", list(xdim, ydim), NULL, dlname, prec = "double")
dlname <- "Lambert_Conformal"
proj_def <- ncvar_def("Lambert_Conformal", "1", NULL, NULL, longname = dlname, prec = "char")

# create netCDF file and put arrays
ncout <- nc_create(ncfname3,list(omega_def,lon_def,lat_def,proj_def),force_v4=FALSE)

# put variables
ncvar_put(ncout,omega_def,omegam_array)
ncvar_put(ncout, lon_def, lon)
ncvar_put(ncout, lat_def, lat)

# put additional attributes into dimension and data variables
ncatt_put(ncout, "x", "axis", "X")
ncatt_put(ncout, "x", "standard_name", "projection_x_coordinate")
ncatt_put(ncout, "x", "_CoordinateAxisType", "GeoX")
ncatt_put(ncout, "y", "axis", "Y")
ncatt_put(ncout, "y", "standard_name", "projection_y_coordinate")
ncatt_put(ncout, "y", "_CoordinateAxisType", "GeoY")
ncatt_put(ncout, "omega", "grid_mapping", "Lambert_Conformal")
ncatt_put(ncout, "omega", "coordinates", "lat lon")

# put the CRS attributes
projname <- "lambert_conformal_conic"
false_easting <- 5632642.22547
false_northing <- 4612545.65137
ncatt_put(ncout, "Lambert_Conformal", "name", projname)
ncatt_put(ncout, "Lambert_Conformal", "long_name", projname)
ncatt_put(ncout, "Lambert_Conformal", "grid_mapping_name", projname)
ncatt_put(ncout, "Lambert_Conformal", "longitude_of_central_meridian", as.double(longitude_of_central_meridian$value))
ncatt_put(ncout, "Lambert_Conformal", "latitude_of_projection_origin", as.double(latitude_of_projection_origin$value))
ncatt_put(ncout, "Lambert_Conformal", "standard_parallel", c(50.0, 50.0))
ncatt_put(ncout, "Lambert_Conformal", "false_easting", false_easting)
ncatt_put(ncout, "Lambert_Conformal", "false_northing", false_northing)
ncatt_put(ncout, "Lambert_Conformal", "_CoordinateTransformType", "Projection")
ncatt_put(ncout, "Lambert_Conformal", "_CoordinateAxisTypes", "GeoX GeoY")

# add global attributes
ncatt_put(ncout, 0, "title", "test output of projected data")
ncatt_put(ncout, 0, "institution", "NOAA ESRL PSD")
ncatt_put(ncout, 0, "source", "omega")
history <- paste("P.J. Bartlein", date(), sep = ", ")
ncatt_put(ncout, 0, "history", history)
ncatt_put(ncout, 0, "Conventions", "CF=1.6")

# Get a summary of the created file:
ncout
nc_close(ncout)
nc_close(ncin)