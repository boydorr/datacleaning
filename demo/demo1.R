#-------------------------------------------------------------------------------#
# This demo will show how to use the range of functions within the datacleaning #
# package in order to process location names at the village, ward, district and #
# region level.                                                                 #
#-------------------------------------------------------------------------------#

#'## Setup ##

# Load in example dataset
data("example_locations")

# Define coordinate reference system (CRS) as Geographic WGS84
Geoproj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
GEO <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


