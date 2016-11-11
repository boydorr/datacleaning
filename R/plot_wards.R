#' plot_wards / Create and plot a SpatialPolygonsDataFrame object of the wards of interest
#' 
#' If each ward/district/region combination in the project dataset is correct 
#' and not ambiguous, it creates and plot a SpatialPolygonsDataFrame object of 
#' the wards in the project dataset.
#'
#' @param dataframe - the project dataframe with the ward/district/region names
#' @param  label_ward - the name of the column in the project dataframe storing the ward names. NOTE: The label must be specified as "label" (i.e. "ward", for instance)
#' @param  label_dis - the name of the column in the project dataframe storing the district names. NOTE: The label must be specified as "label" (i.e. "district", for instance)
#' @param  label_reg - the name of the column in the project dataframe storing the region names. NOTE: The label must be specified as "label" (i.e. "region", for instance)
#' @param  shapefile - the shapefile object with Tanzania wards, districts and regions 
#' 
#' @details \code{plot_wards} uses the ward/district/region information 
#' in the project dataset to create a a SpatialPolygonsDataFrame object of the 
#' wards. The function only includes the ward/district/region 
#' combinations that are both CORRECT (no mismatches, i.e. the ward is
#' in that district and region) and UNIQUE (no ambiguity, i.e. there is only
#' one ward with that name within that district and region). The boundaries
#' between the polygons are the boundaries of the wards.
#' 
#' @return a SpatialPolygonsDataframe object (that can be easily exported as shapefile) 
#' and a plot of the wards of interest. The "@data" slot in the SpatialPolygonsDataframe
#' only stores the information about the ward/district/region.
#' @export
#' 
#' @examples 
#' #
#' 

plot_wards <- function(dataframe, label_ward, label_dis, label_reg, 
                      shapefile)
{
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)
  
  wrd <- dataframe[[label_ward]]
  wrd <- as.character(wrd)
  
  ds <- dataframe[[label_dis]]
  ds <- as.character(ds)
  
  rg <- dataframe[[label_reg]]
  rg <- as.character(rg)
  
  ## number of wards
  wrd_length <- length(wrd)
  ## generate empty list for results
  mylist_wards <- vector('list', wrd_length)
  
  ## number of districts
  dis_length <- length(ds)
  ## generate empty list for results
  mylist_districts <- vector('list', dis_length)
  
  ## number of regions
  reg_length <- length(rg)
  ## generate empty list for results
  mylist_regions <- vector('list', reg_length)
  
  ### list with the index of the wards
  mylist_nums <- vector('list', ntot)
  
  ## list with the index of the rows 
  mylist_rows <- vector('list', ntot)
  
  ## write a loop to identify the rows of the shapefile to plot
  for(i in 1:ntot)
  {
    # ward
    if(is.na(wrd[i])==F)
    {
      mylist_wards[[i]] <- which(shapefile$Ward_Name==wrd[i])
    } else {
      mylist_wards[[i]] <- c(1:length(shapefile$Ward_Name))
    }
    
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
    
    mylist_nums[[i]] <- Reduce(intersect, list(mylist_wards[[i]],
                                               mylist_districts[[i]],
                                               mylist_regions[[i]]))
    
    #### Find the duplicated names and produce a warning message
    if(length(mylist_nums[[i]])==1)
    {
      next
    } 
    else { 
      if(length(mylist_nums[[i]]) > 1)
      {
        print(paste("More than one match for:", wrd[i]))
        mylist_nums[[i]] <- NA
      } else { 
        print(paste("No matches for:", wrd[i])) 
        mylist_nums[[i]] <- NA
      }
    }
  }
  
  ## remove the duplicates
  mylist_uniquerows <- unique(mylist_nums)
  
  ## The wards identified with certainty
  final_ward_rows <- unlist(mylist_uniquerows)
  final_ward_rows <- final_ward_rows[!is.na(final_ward_rows)]
  
  ## the shapefile with the villages of interest
  vill_subset <- shapefile[final_ward_rows,]
  
  vill_subset@data <- select(vill_subset@data, OBJECTID, Ward_Name,
                             District_N, Region_Nam)
  
  vill_subset$Ward_Name <- as.character(vill_subset$Ward_Name)
  vill_subset$Ward_Name <- as.factor(vill_subset$Ward_Name)
  
  vill_subset$District_N <- as.character(vill_subset$District_N)
  vill_subset$District_N <- as.factor(vill_subset$District_N)
  
  vill_subset$Region_Nam <- as.character(vill_subset$Region_Nam)
  vill_subset$Region_Nam <- as.factor(vill_subset$Region_Nam)
  
  plot(vill_subset)
  
  return(vill_subset)
}

