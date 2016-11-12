#-------------------------------------------------------------------------------
#'** FUNCTION to extract the location information from a set of coordinates**
#-------------------------------------------------------------------------------

#' Arguments:
## dataframe = coordinates in two seperate columns (unfortunately labelled
## "X.UTM" and "Y.UTM")
## shapefile = "TANZANIA_VILLAGES.shp" in Geoproj format
## ("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
## data.projection = the projection that the data is currently in
## (e.g. "+proj=utm +zone=36 +south +datum=WGS84")

#' Return:
## A plot showing the location of the data points over all Villages in Tanzania
## A dataframe object listing location information that can be easily exported
## as .csv

extract_location <- function(dataframe, shapefile, data.projection)
{
  # Set shapefile projetion
  Geoproj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  #' Transform from UTM to latlong
  data <- SpatialPoints(dataframe, proj4string=CRS(data.projection))
  proj.data <- spTransform(data, Geoproj)

  # Plot points on map of Wards in Tanzania
  plot(shapefile)
  points(proj.data$X.UTM, proj.data$Y.UTM, pch=16, col="tomato")

  #' Extract information on which wards, district and region data is from
  location <- over(proj.data, shapefile)

  # Save location information in new data frame
  location_df <- data.frame(location$Vil_Mtaa_N, location$Ward_Name,
                            location$District_N, location$Region_Nam)

  # Rename columns to match GIS_Ward_function requirements
  names(location_df)[names(location_df)=="location.Region_Nam"] <- "Region"
  names(location_df)[names(location_df)=="location.District_N"] <- "District"
  names(location_df)[names(location_df)=="location.Ward_Name"] <- "Ward"
  names(location_df)[names(location_df)=="location.Vil_Mtaa_N"] <- "Village"

  #' Remove duplicated rows
  location_df <- unique(location_df)

  return(location_df)
}
