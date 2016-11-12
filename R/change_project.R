#-----------------------------------------------------------------------------#
# ** Load in Packages **
#-----------------------------------------------------------------------------#

library(sp)
library(rgdal)
library(maptools)

#-----------------------------------------------------------------------------#
# ** Change_projection Function **
#
# This function will alter the projection of a shapefile to that of the entire
# project (+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0).
#
# Argument:
# - shapefile: Loaded in the original projection
#-----------------------------------------------------------------------------#

change_projection <- function(shapefile)
{
  # Set intended projection
  Geoproj <-
    CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Transform shapefile to intended projection
  proj.shapefile <- spTransform(shapefile, Geoproj)

  return(proj.shapefile)
}
