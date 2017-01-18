#' @title Create a SpatialPolygonsDataFrame object of the regions/districts/wards/villages of interest
#'
#' @description It creates (and plots, if plot=TRUE) a SpatialPolygonsDataFrame object of the regions/districts/wards/villages of interest
#'
#' @param dataframe the final dataframe returned by the function create_final_df
#' @param  loc_type the location type of interest, stored in the Location_Type column of the dataframe. Must be one of "Region", "District", "Ward", "Village"
#' @param  label_ID the column name of the column in the final dataframe storing the locations ID
#' @param  shapefile the shapefile object with Tanzania villages/wards/districts/regions, depending on the location type of interest 
#' @param  plot optional, default = TRUE. Specifies whether the identified regions/districts/wards must be plotted.
#'
#' @details \code{identify_location} uses the regions/districts/wards/villages information
#' in the dataset returned by the function create_final_df to produce a SpatialPolygonsDataFrame 
#' object of the regions/districts/wards/villages of interest. 
#'
#' @return a SpatialPolygonsDataframe object (that can be easily exported as shapefile)
#' (and a plot, if the argument plot=T) of the locations of interest. 
#'
#' @export
#'
#' @examples
#' #
#'

identify_location <- function(dataframe, loc_type, label_ID, shapefile, plot=T)
{
  
  loc_df <- dplyr::filter(dataframe, Location_Type==loc_type)
  
  loc_code <- as.character(loc_df[[label_ID]])
  loc_code <- unique(loc_code)
  
  final_loc_rows <- which(shapefile$Loc_ID %in% loc_code)
  
  loc_subset <- shapefile[final_loc_rows,]
  
  if(plot==T)
  {
    sp::plot(loc_subset)
    return(loc_subset)
  } else {
    return(loc_subset)
  }
}
