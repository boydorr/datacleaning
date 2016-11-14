#' extract_location / Extract the location information from a set of coordinates
#'
#' Extracts village, ward, district and region information for each data point
#' as a data frame if coordinates are correct. Coordinates do not need to be in
#' UTM format, but the columns containing coordinates currently need to be
#' labelled as "X.UTM" and "Y.UTM".
#'
#' @param dataframe - coordinates in two seperate columns (unfortunately labelled
#' X.UTM" and "Y.UTM")
#' @param shapefile - shapefile object of "TANZANIA_VILLAGES.shp" in the project
#' projction ("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#' @param data.projection - an object containing the projection that the data
#' is currently in.
#'
#' @return a plot overlaying the location of all data points over Village
#' polygons in Tanzania, and a dataframe object listing location information that
#' can be easily exported as .csv.
#'
#' @export
#'
#' @examples
#' #

extract_location <- function(dataframe, shapefile, data.projection)
{
  # Set shapefile projetion
  Geoproj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  #' Transform from UTM to latlong
  data <- sp::SpatialPoints(dataframe, proj4string=CRS(data.projection))
  proj.data <- sp::spTransform(data, Geoproj)

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
