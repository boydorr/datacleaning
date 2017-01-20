#' @title Extract the location information from a set of coordinates
#'
#' @description Extracts village, ward, district and region information for each
#' data point as a data frame if coordinates are correct.
#'
#' @param dataframe a dataframe which contains coordinates in two seperate
#' columns - this dataframe can include other columns which are not used by this
#' function.
#' @param data.projection the projection that the data is currently in.
#' @param shapefile (Optional) the village_2012 shapefile is used as default, but
#' an alternative can be provided.
#' @param x_label (Optional) this is automatically recognised as "X.UTM", but an
#' alternative column name will be accepted if provided.
#' @param y_label (Optional) this is automatically recognised as "Y.UTM", but an
#' alternative column name will be accepted if provided.
#' @param update.df (Optional) this is FALSE by default; if "TRUE", the original
#' dataframe will be updated with new columns providing the Village, District,
#' District and Region names for each row.
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
#' @return a dataframe object listing unique location information that can be
#' easily exported as a .csv.
#'
#' @export
#'
#' @examples
#' # Load in example UTM coordinate dataset
#' data("example_utm_coordinates")
#'
#' # Run function without updating dataframe with locations
#' extract_location(dataframe=example_utm_coordinates, data.projection="+proj=utm +zone=36 +south +datum=WGS84")
#'
#' # Run function with update of dataframe with locations
#' extract_location(dataframe=example_utm_coordinates, data.projection="+proj=utm +zone=36 +south +datum=WGS84", update.df=TRUE)
#'
#' # Load in example Geographic WGS84 coordinate dataset
#' data("example_geog_coordinates")
#'
#' # Run function without updating dataframe with locations
#' extract_location(dataframe=example_geog_coordinates, data.projection="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", x_label="Longitude", y_label="Latitude")


extract_location <- function(dataframe, data.projection, shapefile=village_2012,
                             x_label="X.UTM", y_label="Y.UTM", update.df=FALSE)
{
  # Remove rows with NA values
  data <- dataframe[!is.na(dataframe[[x_label]]),]
  data <- data[!is.na(data[[y_label]]),]

  # Re-class as numeric
  data[[x_label]] <- as.numeric(data[[x_label]])
  data[[y_label]] <- as.numeric(data[[y_label]])

  # Bind the two coordinate objects into a data frame
  coord_data <- data.frame(data[[x_label]], data[[y_label]])

  # Set shapefile projetion
  Geoproj <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Create spatial object
  data <- sp::SpatialPoints(coord_data, proj4string=CRS(data.projection))

  # Create if() loop to recognise if data needs to be transformed
  if(!data.projection=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  {
    # If projection is different from Geoproj -> transform
    print("Tranformed to 'Geogproj'")
    proj.data <- sp::spTransform(data, Geoproj)
    new.coord <- as.data.frame(proj.data)

    # Add transformed coordinates to original dataframe
    print("Geograhic WGS84 coordinates added'")
    dataframe <- cbind(dataframe, new.coord)
  } else {
    # Else pass to object of the same name.
    print("Not transformed: In Geoproj already")
    proj.data <-  data
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
  if(any(is.na(location_df))) print('NAs present in dataframe')

  # Remove duplicated rows to produce unique list
  unique_location_df <- unique(location_df)

  # Remove NAs
  complete_location_df <- unique_location_df[complete.cases(unique_location_df),]

  # Create if() loop to recognise if data needs to be updated
  if(update.df==TRUE)
  {
    print("Village, Ward, District and Region columns added to dataframe")
    # Bind columns detailing Village, Ward, District and Region to original dataframe
    dataframe <- cbind(dataframe, location_df)
  } else {
    print("Dataframe not updated")
  }

  return(complete_location_df)
  }
