#' @title Create a SpatialPolygonsDataFrame object of the regions/districts/wards/villages of interest
#'
#' @description It creates a SpatialPolygonsDataFrame object of the regions/districts/wards/villages of interest
#'
#' @param dataframe the final dataframe returned by the function create_final_df
#' @param  loc_type the location type of interest, stored in the Location_Type column of the dataframe. Must be one of "Region", "District", "Ward", "Village"
#' @param  label_ID the column name of the column in the final dataframe storing the locations ID
#' @param  shapefile (default = TZ_all_bound) the shapefile object with Tanzania all boundaries (villages/wards/districts/regions)
#'
#' @details \code{identify_location} uses the regions/districts/wards/villages information
#' in the dataset returned by the function create_final_df to produce a SpatialPolygonsDataFrame 
#' object of the regions/districts/wards/villages of interest. If a Village/Ward/District 
#' was NA in the original dataset and plot_additional=T, the uncertainty associated 
#' to it is represented by including its respective higher level element. For instance, 
#' if a village is NA, the function will also return the WARD associated 
#' to the NA village; if also the ward is NA, it will return the District, and if 
#' also the district is NA it will return the respective Region.
#'
#' @return a SpatialPolygonsDataframe object (that can be easily exported as shapefile)
#' of the locations of interest. 
#'
#' @export
#'
#' @examples
#' #
#'

identify_location_from_all_boundaries <- function(dataframe, loc_type, label_ID, shapefile)
{
  
  ## filter the loc_df by location type
  loc_df <- dplyr::filter(dataframe, Location_Type==loc_type)
  
  loc_code <- as.character(loc_df[[label_ID]])
  
  ## identify the rows in the shapefile in which the identified Loc_IDs are 
  final_loc_rows <- which(shapefile$Loc_ID %in% loc_code)
  
  loc_subset <- shapefile[final_loc_rows,]
  
  return(loc_subset)
 }
