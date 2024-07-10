# Downloaded JPL data from https://www.earthdata.nasa.gov/
# which are described at https://grace.jpl.nasa.gov/data/get-data/jpl_global_mascons/

library(terra)
library(geodata)

grace_file <- "GRCTellus.JPL.200204_202404.GLO.RL06.1M.MSCNv03CRI.nc"
# File contains many valriables. Only extract lwe
r <- terra::rast(file.path(grace_file), subds="lwe_thickness")
us_states <- geodata::gadm(country="USA", level=1, path=tempdir())

#Area of 17 western US states
wus_states <- c("Nebraska","Washington","New Mexico","South Dakota","Texas",
                "California","Oregon","Kansas","Idaho","Nevada","Montana",
                "North Dakota","Arizona","Colorado","Oklahoma","Utah","Wyoming")
wus = us_states[match(toupper(wus_states),toupper(us_states$NAME_1)),]

#change boundary projection to be consistent with GRACE data
wus_t = terra::project(wus, terra::crs(r))

# Rotate a SpatRaster that has longitude coordinates from 0 to 360, to standard 
# coordinates between -180 and 180 degrees. Crop a smaller domain
r_wus <- terra::crop(terra::rotate(r), wus_t, snap="out")

# remove big raster from environment
remove(r)

terra::writeCDF(r_wus, "GRCTellus.JPL.200204_202404_wus.nc", varname = "lwe_thickness", 
                longname = "Liquid_Water_Equivalent_Thickness", 
                unit = "cm")
terra::writeVector(wus_t, "wus_states.shp")
