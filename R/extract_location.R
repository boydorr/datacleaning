#' @title Extract the location information from a set of coordinates
#'
#' @description Extracts village, ward, district and region information for each
#' data point as a data frame if coordinates are correct; then produces multiple
#' dataframes which can be used in future processing.
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
#' @param add.coords (Optional) this is FALSE by default; if "TRUE", the original
#' dataframe will be updated with two new columns giving the new coordinates for
#' Latitude and Longitude in the Geographic WGS84 projection.
#'
#' @details \code{extract_location} creates a spatial object, and will transform
#' to the Geographic WGS84 projecion - if already in this projection, it passess
#' to the next stage without change. A message will advise of which step was
#' taken upon running the function.
#'
#' A list is returned containing:
#'
#' [1] A data frame with location information seperated into four columns
#' (Village, Ward, District and Region). This information is a set of unique
#' locations. A warning message will be produced if NAs are present in this
#' dataframe.
#' [2] A dataframe with location information in a setup using unique codes to
#' identify each location, using three columns (Location, Location_Type and
#' Loc_ID); This dataframe is used within the Shiny App.
#'
#' If \code{update.df} or \code{add.coords} is used in the function call, a
#' third item is returned:
#'
#' [3] The original dataframe with the updates in new columns.
#'
#' Each of these can be accessed by calling \code{data_location[[i]]}, where
#' \code{i} is the number mentioned above.
#'
#' @return a list of dataframe objects providing location information that can be
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
                             x_label="X.UTM", y_label="Y.UTM", update.df=FALSE,
                             add.coords=FALSE)
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
    print("Tranformed to Geograhic WGS84")
    proj.data <- sp::spTransform(data, Geoproj)
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

  # Create Final dataframe of locations
  shiny_location_df <- datacleaning::create_final_df(complete_location_df,
                                         corr_df_vill ="Village",
                                         corr_df_ward="Ward",
                                         corr_df_distr="District",
                                         corr_df_reg="Region",
                                         ID_df=Location_ID_df)
  shiny_location_df <- unique(shiny_location_df)

  #-----------------------------------------#
  # Add transformed coordinate to dataframe #
  #-----------------------------------------#
  if(add.coords==TRUE)
  {
    # View as dataframe
    new.coord <- as.data.frame(proj.data)

    # Find which rows contain non-NAs in original dataframe coordinate columns
    rows <- which(!is.na(dataframe[[x_label]]))

    # Create empty data frame
    new.coord.df <- data.frame()

    # Create an object to tick through each row in new.coord
    j=1

    for(i in 1:nrow(dataframe)){
      # If any of the row numbers of non-NA cells is equal to i
      if(any(rows==i)){
        new.row <- subset(new.coord, rownames(new.coord)==j)

        names(new.row)[[1]] <- "Latitude"
        names(new.row)[[2]] <- "Longitude"

        new.coord.df <- rbind(new.coord.df, new.row)

        # Add 1 to j to scroll to next row in "rows" dataframe
        j = j + 1
      } else {
        new.row <- data.frame(NA, NA)
        names(new.row)[[1]] <- "Latitude"
        names(new.row)[[2]] <- "Longitude"

        new.coord.df <- rbind(new.coord.df, new.row)
      }
    }

    # Add transformed coordinates to original dataframe
    print("Geograhic WGS84 coordinates added")
    dataframe <- cbind(dataframe, new.coord.df)
  }

  #----------------------------------#
  # Add location update to dataframe #
  #----------------------------------#
  if(update.df==TRUE)
  {
    # Find which rows contain non-NAs in original dataframe coordinate columns
    rows <- which(!is.na(dataframe[[x_label]]))

    # Create empty data frame
    loc.df <- data.frame()

    # Create an object to tick through each row in new.coord
    k=1

    for(m in 1:nrow(dataframe)){
      # If any of the row numbers of non-NA cells is equal to i
      if(any(rows==m)){
        n.row <- subset(location_df, rownames(location_df)==k)

        names(n.row)[[1]] <- "Village"
        names(n.row)[[2]] <- "Ward"
        names(n.row)[[3]] <- "District"
        names(n.row)[[4]] <- "Region"

        loc.df <- rbind(loc.df, n.row)

        # Add 1 to k to scroll to next row in "rows" dataframe
        k = k + 1
      } else {
        n.row <- data.frame(NA, NA, NA, NA)
        names(n.row)[[1]] <- "Village"
        names(n.row)[[2]] <- "Ward"
        names(n.row)[[3]] <- "District"
        names(n.row)[[4]] <- "Region"

        loc.df <- rbind(loc.df, n.row)
      }
    }

    print("Village, Ward, District and Region columns added to dataframe")

    # Bind columns detailing Village, Ward, District and Region to original dataframe
    dataframe <- cbind(dataframe, loc.df)
  }

  # Create list of returnable items
  if(add.coords==TRUE | update.df==TRUE){
    final_list <- list(complete_location_df, shiny_location_df, dataframe)
  } else {
    final_list <- list(complete_location_df, shiny_location_df)
  }

  return(final_list)
  }
