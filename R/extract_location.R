#' @title Extract the location information from a set of coordinates
#'
#' @description Extracts village, ward, district and region information for each
#' data point as a data frame if coordinates are correct.
#'
#' @param dataframe a dataframe which contains coordinates in two seperate
#' columns.
#' @param data.projection the projection that the data is currently in.
#' @param shapefile (Optional) this will use the village_2012 shapefile as
#' default, but will accept an alternative if provided.
#' @param x_label (Optional) this is automatically recognised as "X.UTM", but an
#' alternative column name will be accepted if provided.
#' @param y_label (Optional) this is automatically recognised as "Y.UTM", but an
#' alternative column name will be accepted if provided.
#'
#' @details \code{extract_location} creates a spatial object, and will transform
#' to the 'Geoproj' projecion - if already in this projection, it passess to the
#' next stage without change. A message will advise of which step was taken upon
#' running the function.
#'
#' A data frame is produced with location information seperated into four columns
#' (Village, Ward, District and Region). This information is not a complete list,
#' but a set of unique locations. A warning message will be produced if NAs are
#' present in this resulting dataframe.
#'
#' @return a dataframe object listing location information that can be easily
#' exported as a .csv.
#'
#' @export
#'
#' @examples
#' # Load in example dataset
#' data("example_utm_coordinates")
#'
#' # Run function
#' extract_location(dataframe=example_utm_coordinates, data.projection="+proj=utm +zone=36 +south +datum=WGS84")
#' # RUN AS update=TRUE and update=FALSE

# Add locations to original dataframe: update.df=FALSE -> If TRUE, cbind to original dataframe
extract_location <- function(dataframe, data.projection, shapefile=village_2012,
                             x_label="X.UTM", y_label="Y.UTM")
{
  # Seperate out the two columns containing coordinates
  Longitude <- dataframe[[x_label]]
  Latitude <- dataframe[[y_label]]

  # Bind the two coordinate objects into a data frame
  coord_data <- cbind(Longitude, Latitude)

  # Set shapefile projetion
  Geoproj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Create spatial object
  data <- sp::SpatialPoints(coord_data, proj4string=CRS(data.projection))

  # If projection is different from Geoproj then transform, else pass to
  # object of the same name.
  if(!data.projection=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  {
    print("Tranformed to 'Geogproj'")
    proj.data <- sp::spTransform(data, Geoproj)
    new.coord <- as.data.frame(proj.data)
  } else {
    print("Not transformed: In Geoproj already")
    proj.data <-  data
  }

  # Add new coordinates to original dataframe if transformed to Geographic WGS84
  if(!data.projection=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  {
    print("Geograhic WGS84 coordinates added'")
    dataframe <- cbind(dataframe, new.coord)
  } else {
    print("Dataframe coordinates already in Geographic WGS84")
  }

  # Extract information on which Village, ward, district and region data is from
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

  # Remove duplicated rows to produce unique list
  unique_location_df <- unique(location_df)

  return(unique_location_df)
  }
