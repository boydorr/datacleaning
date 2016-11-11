#' plot_districts / Create and plot a SpatialPolygonsDataFrame object of the districts of interest
#' 
#' If each district/region combination in the project dataset is correct 
#' and not ambiguous, it creates and plot a SpatialPolygonsDataFrame object of 
#' the districts in the project dataset.
#'
#' @param dataframe - the project dataframe with the district/region names
#' @param  label_dis - the name of the column in the project dataframe storing the district names. NOTE: The label must be specified as "label" (i.e. "district", for instance)
#' @param  label_reg - the name of the column in the project dataframe storing the region names. NOTE: The label must be specified as "label" (i.e. "region", for instance)
#' @param  shapefile - the shapefile object with Tanzania wards, districts and regions 
#' 
#' @details \code{plot_districts} uses the district/region information 
#' in the project dataset to create a a SpatialPolygonsDataFrame object of the 
#' districts. The function only includes the district/region 
#' combinations that are both CORRECT (no mismatches, i.e. the district is
#' in that region) and UNIQUE (no ambiguity, i.e. there is only
#' one district with that name within that region). The boundaries
#' between the polygons are the boundaries of the WARDS
#' 
#' @return a SpatialPolygonsDataframe object (that can be easily exported as shapefile) 
#' and a plot of the districts of interest. The "@data" slot in the SpatialPolygonsDataframe
#' only stores the information about the ward/district/region.
#' @export
#' 
#' @examples 
#' #
#' 

plot_districts <- function(dataframe, label_dis, label_reg, 
                       shapefile)
{
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)
  
  ds <- dataframe[[label_dis]]
  ds <- as.character(ds)
  
  rg <- dataframe[[label_reg]]
  rg <- as.character(rg)
  
  ## number of districts
  dis_length <- length(ds)
  ## generate empty list for results
  mylist_districts <- vector('list', dis_length)
  
  ## number of regions
  reg_length <- length(rg)
  ## generate empty list for results
  mylist_regions <- vector('list', reg_length)
  
  ### list with the index of the districts
  mylist_nums <- vector('list', ntot)
  
  ## list with the index of the rows 
  mylist_rows <- vector('list', ntot)
  
  ## write a loop to identify the rows of the shapefile to plot
  for(i in 1:ntot)
  {
    # districts
    if(is.na(ds[i])==F)
    {
      mylist_districts[[i]] <- which(shapefile$District_N==ds[i])
    } else {
      mylist_districts[[i]] <- c(1:length(shapefile$District_N))
    }
    
    # regions
    if(is.na(rg[i])==F)
    {
      mylist_regions[[i]] <- which(shapefile$Region_Nam==rg[i])
    } else {
      mylist_regions[[i]] <- c(1:length(shapefile$Region_Nam))
    }
    
    mylist_nums[[i]] <- Reduce(intersect, list(mylist_districts[[i]],
                                               mylist_regions[[i]]))
    
  }
  
  mylist_nums <- unique(mylist_nums)
  ## The districts identified with certainty
  final_dist_rows <- unlist(mylist_nums)
  final_dist_rows <- final_dist_rows[!is.na(final_dist_rows)]
  
  ## the shapefile with the villages of interest
  vill_subset <- shapefile[final_dist_rows,]
  vill_subset@data <- select(vill_subset@data, OBJECTID, Ward_Name,
                             District_N, Region_Nam)
  
  vill_subset$Ward_Name <- as.character(vill_subset$Ward_Name)
  vill_subset$Ward_Name <- as.factor(vill_subset$Ward_Name)
  
  vill_subset$District_N <- as.character(vill_subset$District_N)
  vill_subset$District_N <- as.factor(vill_subset$District_N)
  
  vill_subset$Region_Nam <- as.character(vill_subset$Region_Nam)
  vill_subset$Region_Nam <- as.factor(vill_subset$Region_Nam)
  
  return(vill_subset)
}
  
################################################################################
################################################################################
################################################################################
## in order to export polygon shapefile

#rgdal::writeOGR(shp_object, dsn = "shapefile_folder/", "file_name", driver="ESRI Shapefile")

## the file will be in the "shapefile_folder" in your working directory :)

