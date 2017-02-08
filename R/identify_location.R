#' @title Create a SpatialPolygonsDataFrame object of the regions/districts/wards/villages of interest
#'
#' @description It creates (and plots, if plot=TRUE) a SpatialPolygonsDataFrame object of the regions/districts/wards/villages of interest
#'
#' @param dataframe the final dataframe returned by the function create_final_df
#' @param  loc_type the location type of interest, stored in the Location_Type column of the dataframe. Must be one of "Region", "District", "Ward", "Village"
#' @param  label_ID the column name of the column in the final dataframe storing the locations ID
#' @param  shapefile the shapefile object with Tanzania villages/wards/districts/regions, depending on the location type of interest 
#' @param  plot_loc optional, default = TRUE. Specifies whether the identified regions/districts/wards/villages must be plotted.
#' @param  plot_additional optional, default = TRUE. Specifies whether the identified additional regions/districts/wards must be plotted.
#'
#' @details \code{identify_location} uses the regions/districts/wards/villages information
#' in the dataset returned by the function create_final_df to produce a SpatialPolygonsDataFrame 
#' object of the regions/districts/wards/villages of interest. If a Village/Ward/District 
#' was NA in the original dataset and plot_additional=T, the uncertainty associated 
#' to it is represented by plotting its respective higher level element. For instance, 
#' if a village is NA and plot_additional=T, the function will plot the WARD associated 
#' to the NA village; if also the ward is NA, it will plot the District, and if 
#' also the district is NA it will plot the respective Region.
#'
#' @return a SpatialPolygonsDataframe object (that can be easily exported as shapefile)
#' (and a plot, if the argument plot_loc=T) of the locations of interest. 
#'
#' @export
#'
#' @examples
#' #
#'

identify_location <- function(dataframe, loc_type, label_ID, shapefile, 
                              plot_loc=T, plot_additional=T)
{
  
  ## filter the loc_df by location type
  loc_df <- dplyr::filter(dataframe, Location_Type==loc_type)
  
  ## the rows where Location_Type == Real_Loc_Type
  diff_real_loc_index <- which(loc_df$Location_Type!=loc_df$Real_Loc_Type)
  ## store them in an additional df
  additional_loc <- loc_df[diff_real_loc_index,]
  
  if(nrow(additional_loc)==0)
  {
    additional_loc <- additional_loc # do nothing
    
  } else { # if there are additional locations
    
    if("Ward" %in% unique(additional_loc$Real_Loc_Type))
    {
      ## extract the unique Loc_ID
      indx <- which(additional_loc$Real_Loc_Type=="Ward")
      loc_code_w <- as.character(additional_loc[[label_ID]][indx])
      loc_code_w <- unique(loc_code_w)
      
      ## identify the rows in the shapefile in which the identified Loc_IDs are 
      W_shp <- datacleaning::ward_2012
      final_loc_rows_w <- which(W_shp$Loc_ID %in% loc_code_w)
      
      ## create the final shapefile with only the identified locations
      loc_subset_w <- W_shp[final_loc_rows_w,]
      
    } else {
      
      additional_loc <- additional_loc # do nothing
    }
    
    if ("District" %in% unique(additional_loc$Real_Loc_Type))
    {
      ## extract the unique Loc_ID
      indx <- which(additional_loc$Real_Loc_Type=="District")
      loc_code_d <- as.character(additional_loc[[label_ID]][indx])
      loc_code_d <- unique(loc_code_d)
      
      ## identify the rows in the shapefile in which the identified Loc_IDs are 
      D_shp <- datacleaning::district_2012
      final_loc_rows_d <- which(D_shp$Loc_ID %in% loc_code_d)
      
      ## create the final shapefile with only the identified locations
      loc_subset_d <- D_shp[final_loc_rows_d,]
    } else {
      
      additional_loc <- additional_loc # do nothing
    }
    
    if("Region" %in% unique(additional_loc$Real_Loc_Type))
    {
      ## extract the unique Loc_ID
      indx <- which(additional_loc$Real_Loc_Type=="Region")
      loc_code_r <- as.character(additional_loc[[label_ID]][indx])
      loc_code_r <- unique(loc_code_r)
      
      ## identify the rows in the shapefile in which the identified Loc_IDs are 
      R_shp <- datacleaning::region_2012
      final_loc_rows_r <- which(R_shp$Loc_ID %in% loc_code_r)
      
      ## create the final shapefile with only the identified locations
      loc_subset_r <- R_shp[final_loc_rows_r,]
      
    } else {
      # do nothing
      additional_loc <- additional_loc # do nothing
      
    }
  }
  
  
  
  ## extract the unique Loc_ID
  loc_code <- as.character(loc_df[[label_ID]])
  loc_code <- unique(loc_code)
  
  ## identify the rows in the shapefile in which the identified Loc_IDs are 
  final_loc_rows <- which(shapefile$Loc_ID %in% loc_code)
  
  ## create the final shapefile with only the identified locations
  loc_subset <- shapefile[final_loc_rows,]
  
  add_bounds <- T
    
  if(plot_additional==T)
  {
    plot_loc <- T
    
    if("Region" %in% unique(additional_loc$Real_Loc_Type))
    {
      sp::plot(loc_subset_r, col="darkolivegreen")
      
    } else {
      ## do nothing
      loc_subset <- loc_subset
      add_bounds <- F
    }
    
    if("District" %in% unique(additional_loc$Real_Loc_Type))
    {
      sp::plot(loc_subset_d, col="darkolivegreen4", add=add_bounds)
      add_bounds <- T
      
    } else {
      ## do nothing
      loc_subset <- loc_subset
      add_bounds <- F
    }
    
    if("Ward" %in% unique(additional_loc$Real_Loc_Type))
    {
      sp::plot(loc_subset_w, col="darkolivegreen3", add=add_bounds)
      add_bounds <- T
    } else {
      ## do nothing
      loc_subset <- loc_subset
      add_bounds <- F
    }
    
  } else {
    add_bounds <- F 
  } 
  
  
  if(plot_loc==T)
  {  
    sp::plot(loc_subset, col="darkolivegreen1", add=add_bounds)
    return(loc_subset)
    
  } else {
    plot_additional = F
    return(loc_subset)
  }
}
