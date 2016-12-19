library(tibble)

# Define coordinate reference system (CRS)
# Geographic (lat-long)
Geoproj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
GEO <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Collect folder information and move into the raw data folder
WD <- getwd()
devtools::wd(".", "data-raw/Tanzania/reference")

# village shapefile
village_2012 <- rgdal::readOGR(".","Tanzania-village-2012")
# transform to geo projection (lat long)
village_2012 <-sp::spTransform(village_2012, Geoproj)
# export it and save it as a .rda object in the "data" folder
devtools::use_data(village_2012, overwrite = TRUE)

# wards shapefile
ward_2012 <- rgdal::readOGR(".","Tanzania-ward-2012")
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

# Now move to the root of the package
devtools::wd(".")

# And rerun the documentation
devtools::document()
devtools::install(local = FALSE)

# Go back to where you were
setwd(WD)
