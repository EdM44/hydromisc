# Steps to produce CHIRPS sample file
# CHIRPS climatological pecipitation data
#
# Precipitation data averaged from 1981-2020
#
# format S4 class 'SpatRaster' with one layer
# source <https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_annual/tifs/>

prcp <- terra::rast("data-raw/chirps-v2.0.1981-2020.40yrs.tif")
e <- terra::ext( -125, -119, 34, 40)
prcp_cropped <- terra::crop(prcp, e)
prcp_cropped <- terra::ifel(prcp_cropped < 0, NA, prcp_cropped)
#terra::plot(prcp_cropped, range=c(0, 3000))

terra::writeRaster(prcp_cropped, "./inst/extdata/prcp_cropped.tif", overwrite=TRUE)
#usethis::use_data(prcp_cropped, overwrite = TRUE)

