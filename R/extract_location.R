#' extract_location / Extract the location information from a set of coordinates
#'
#' Extracts village, ward, district and region information for each data point
#' as a data frame if coordinates are correct. Coordinates do not need to be in
#' UTM format, but the columns containing coordinates need to be labelled as
#' "X.UTM" and "Y.UTM".
#'
#' @param dataframe - coordinates in two seperate columns labelled "X.UTM" and
#' "Y.UTM".
#' @param shapefile - shapefile object of "TANZANIA_VILLAGES.shp" in the
#' LongLat WGS84 projection.
#' ("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#' @param data.projection - an object containing the projection that the data
#' is currently in.
#'
#' @return A plot overlaying the location of all data points over Village
#' polygons in Tanzania, and a dataframe object listing location information that
#' can be easily exported as a .csv.
#'
#' @export
#'
#' @details This function creates a spatial object, and will transform this to 
#' that of 'Geoproj' - if already in this projection, it passess to the next 
#' stage without change. A message will advise of which step was taken upon 
#' running the function. 
#' 
#' Each pair of coordinates will be plotted over a shapefile image of Tanzania 
#' to check that they are plotting in the expected area.
#' 
#' Finally, a data frame is produced with location information seperated into 
#' four columns (Village, Ward, District and Region). This information is not 
#' a complete list, but a set of unique locations. A warning message will be 
#' produced if NAs are present in this resulting dataframe.
#'
#' @examples
#' #

extract_location <- function(dataframe, shapefile, data.projection)
{
  # Set shapefile projetion
  Geoproj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Create spatial object
  data <- sp::SpatialPoints(dataframe, proj4string=CRS(data.projection))
  
  # If projection is different from Geoproj, transform. If not, then pass to 
  # object of the same name.
  if(!data.projection=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  {
    print("Tranformed to 'Geogproj'")
    proj.data <- sp::spTransform(data, Geoproj)
  } else {
    print("Not transformed: In Geoproj already")
    proj.data <-  data
  }

  # Plot points on map of Wards in Tanzania
  plot(shapefile)
  points(proj.data$X.UTM, proj.data$Y.UTM, pch=16, col="tomato")

  # Extract information on which wards, district and region data is from
  location <- sp::over(proj.data, shapefile)

  # Save location information in new data frame
  location_df <- data.frame(location$Vil_Mtaa_N, location$Ward_Name,
                            location$District_N, location$Region_Nam)

  # Rename columns to match GIS_Ward_function requirements
  names(location_df)[names(location_df)=="location.Region_Nam"] <- "Region"
  names(location_df)[names(location_df)=="location.District_N"] <- "District"
  names(location_df)[names(location_df)=="location.Ward_Name"] <- "Ward"
  names(location_df)[names(location_df)=="location.Vil_Mtaa_N"] <- "Village"

  # Trigger warning message if NAs are present
  if(any(is.na(location_df))) warning('NAs present in dataframe')

  # Remove duplicated rows
  location_df <- unique(location_df)

  return(location_df)
}
