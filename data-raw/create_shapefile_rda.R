library(tibble)

# Define coordinate reference system (CRS)
# Geographic (lat-long)
Geoproj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
GEO <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Collect folder information and move into the raw data folder
WD <- getwd()
devtools::wd(".", "data-raw/Tanzania/reference")

# village shapefile
village_2012 <- rgdal::readOGR(".","TZ_Village_2012")
# transform to geo projection (lat long)
village_2012 <-sp::spTransform(village_2012, Geoproj)
# export it and save it as a .rda object in the "data" folder
devtools::use_data(village_2012, overwrite = TRUE)

# wards shapefile
ward_2012 <- rgdal::readOGR(".","TZ_Ward_2012")
# transform to geo projection (lat long)
ward_2012 <-sp::spTransform(ward_2012, Geoproj)
# export it and save it as a .rda object in the "data" folder
devtools::use_data(ward_2012, overwrite = TRUE)

# districts shapefile
district_2012 <- rgdal::readOGR(".","TZ_District_2012")
# transform to geo projection (lat long)
district_2012 <-sp::spTransform(district_2012, Geoproj)
# export it and save it as a .rda object in the "data" folder
devtools::use_data(district_2012, overwrite = TRUE)

# regions shapefile
region_2012 <- rgdal::readOGR(".","TZ_Region_2012")
# transform to geo projection (lat long)
region_2012 <-sp::spTransform(region_2012, Geoproj)
# export it and save it as a .rda object in the "data" folder
devtools::use_data(region_2012, overwrite = TRUE)

# Tanzania outline shapefile
TZ_outline <- rgdal::readOGR(".","Outline")
# transform to geo projection (lat long)
sp::proj4string(TZ_outline) <- Geoproj
# export it and save it as a .rda object in the "data" folder
devtools::use_data(TZ_outline, overwrite = TRUE)

# Tanzania regions outline shapefile
TZ_region_outline <- rgdal::readOGR(".","TZ_Region_Outline")
# transform to geo projection (lat long)
sp::proj4string(TZ_region_outline) <- Geoproj
# export it and save it as a .rda object in the "data" folder
devtools::use_data(TZ_region_outline, overwrite = TRUE)

# Tanzania districts outline shapefile
TZ_district_outline <- rgdal::readOGR(".","TZ_District_Outline")
# transform to geo projection (lat long)
sp::proj4string(TZ_district_outline) <- Geoproj
# export it and save it as a .rda object in the "data" folder
devtools::use_data(TZ_district_outline, overwrite = TRUE)

# Tanzania wards outline shapefile
TZ_ward_outline <- rgdal::readOGR(".","TZ_Ward_Outline")
# transform to geo projection (lat long)
sp::proj4string(TZ_ward_outline) <- Geoproj
# export it and save it as a .rda object in the "data" folder
devtools::use_data(TZ_ward_outline, overwrite = TRUE)

# Tanzania village outline shapefile
TZ_village_outline <- rgdal::readOGR(".","TZ_Village_Outline")
# transform to geo projection (lat long)
sp::proj4string(TZ_village_outline) <- Geoproj
# export it and save it as a .rda object in the "data" folder
devtools::use_data(TZ_village_outline, overwrite = TRUE)

# Now move to the root of the package
devtools::wd(".")

# And rerun the documentation
devtools::document()
devtools::install(local = FALSE)

# Go back to where you were
setwd(WD)
