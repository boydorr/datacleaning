#' @title Create and plot a SpatialPolygonsDataFrame object of the regions of interest
#'
#' @description If each region in the project dataset is correct, it creates and
#' plot a SpatialPolygonsDataFrame object of the regions in the project dataset.
#'
#' @param dataframe the project dataframe with the district/region names
#' @param  label_reg the name of the column in the project dataframe storing the region names. NOTE: The label must be specified as "label" (i.e. "region", for instance)
#' @param  shapefile the shapefile object with Tanzania wards, districts and regions
#'
#' @details \code{plot_regions} uses the region information
#' in the project dataset to create a a SpatialPolygonsDataFrame object of the
#' regions. The function only uses the CORRECT region names. The boundaries
#' between the polygons are the boundaries of the WARDS.
#'
#' @return a SpatialPolygonsDataframe object (that can be easily exported as shapefile)
#' and a plot of the regions of interest. The "@data" slot in the SpatialPolygonsDataframe
#' only stores the information about the ward/district/region.
#'
#' @export
#'
#' @examples
#' #
#'

plot_regions <- function(dataframe, label_reg, shapefile)
{
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)

  rg <- dataframe[[label_reg]]
  rg <- as.character(rg)

  ## number of regions
  reg_length <- length(rg)
  ## generate empty list for results
  mylist_regions <- vector('list', reg_length)

  ### list with the index of the region
  mylist_nums <- vector('list', ntot)

  ## list with the index of the rows
  mylist_rows <- vector('list', ntot)

  ## write a loop to identify the rows of the shapefile to plot
  for(i in 1:ntot)
  {
    mylist_regions[[i]] <- which(shapefile$Region_Nam==rg[i])

  }

  mylist_regions <- unique(mylist_regions)
  ## The districts identified with certainty
  final_reg_rows <- unlist(mylist_regions)
  final_reg_rows <- final_reg_rows[!is.na(final_reg_rows)]

  ## the shapefile with the villages of interest
  vill_subset <- shapefile[final_reg_rows,]
  vill_subset@data <- dplyr::select(vill_subset@data, Region_Nam)

  vill_subset$Region_Nam <- as.character(vill_subset$Region_Nam)
  vill_subset$Region_Nam <- as.factor(vill_subset$Region_Nam)

  sp::plot(vill_subset)
  return(vill_subset)
}
