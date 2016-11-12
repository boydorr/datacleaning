#-----------------------------------------------------------------------------#
# ** Test Function **
#-----------------------------------------------------------------------------#

# Load Function
source("Change_projection Function.R")

# Set intended projection
Geoproj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Read in a shapefile to plot subsequent shapefile on to check projection
tanz <- readShapePoly("Functions/Regions.shp",  proj4string=Geoproj)

# Load in Shapefile
# You don't always need "delete_null_obj=TRUE" - test without first.
shapefile <- readShapeLines("Serengeti GIS/Airstrips/Airstrips_V1.shp",
                           proj4string = CRS("+proj=utm +zone=36 +south +datum=WGS84"),
                           delete_null_obj=TRUE)

# Plot Original shapefile over tanzania shape
plot(tanz, col="blanchedalmond", axes=TRUE)
plot(shapefile, col="tomato", lwd=4, add=TRUE)

# Run Function
proj.shapefile <- change_projection(shapefile=shapefile)

# Plot results
plot(tanz, col="blanchedalmond", axes=TRUE)
plot(proj.shapefile, col="tomato", lwd=4, add=TRUE)

# Save newly projected shapefile
rgdal::writeOGR(ITEM TO BE SAVED, dsn = "FOLDER NAME",
                "FILE NAME", driver="ESRI Shapefile")
