#' find_similar_district / Find similar district names 
#'  
#' Identifies the misspelled district names in the project dataset and proposes a 
#' set of similar correct district names.
#'
#' @param dataframe - the project dataframe with misspelled district names
#' @param  label - the name of the column in the project dataframe storing the district names. NOTE: The label must be specified as "label" (i.e. "district", for instance)
#' @param  label_higher_level - the name of the column in the project dataframe storing the CORRECT region names. NOTE: The label must be specified as "label" (i.e. "region", for instance)
#' @param  legal_names - a vector containing the legal district names specified by the shapefile 
#' @param  shapefile - the shapefile object used to retrieve the legal names 
#' @param  distnc - Numeric value indicating the maximum distance allowed for a match (see ?agrep)
#' @param  additional_places - Optional. A dataframe containing the additional names of the new districts (post 2012 census). Default = NA. NOTE: The district/region column of this dataframe must be called like the @data slot in the wards shapefile 
#' 
#' @details \code{find_similar_district} finds the misspellings in the district column 
#' in the project dataset, looks for the respective most similar legal names and 
#' proposes them as potential corrections to the typos. The argument "label_higher_level"
#' allows for narrowing down the search to the similar districts contained in the 
#' region specified in the dataset. NOTE: it must be provided even if there is no 
#' region information (in that case, the function will look for similar matches in 
#' the whole list of legal district names). The typos dataframe produced by this 
#' function can be used as argument of fix_typos. 
#' 
#' @return a new dataframe with: i) the district misspellings, stored in the first 
#' column; ii) all the similar matches, each one stored under a separate column;
#' iii) the Region where the misspelled district and the respective similar districts 
#' are (if the information is not available, the column will contain NAs).
#' @export
#' 
#' @examples 
#' #
#' 

find_similar_district <- function(dataframe, label, label_higher_level, 
                                  legal_names, shapefile, distnc,
                                  additional_places=NA)
{
  
  if(!is.na(additional_places))
  {
    shp_df <- select(shapefile@data, District_N, Region_Nam)
    shp_df$District_N <- as.character(shp_df$District_N)
    shp_df$Region_Nam <- as.character(shp_df$Region_Nam)
    legal_names <- c(shp_df$District_N, additional_places$District_N)
    fullset <- rbind(shp_df, additional_places)
    
  } else {
    legal_names <- shapefile@data$District_N
    fullset <- select(shapefile@data, District_N, Region_Nam)
  }
  
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)
  
  #wrd <- dataframe$Ward
  #wrd <- as.character(wrd)
  
  ds <- unique(dataframe[[label]])
  ds <- as.character(ds)
  
  ds_correct <- legal_names
  ds_correct <- as.character(legal_names)
  c_d <- gsub("[^[:alnum:]]", "", tolower(ds_correct))
  
  rg <- unique(dataframe[[label_higher_level]])
  rg <- as.character(rg)
  
  ## number of districts
  dis_length <- length(ds)
  ## generate empty list for results
  mylist_districts <- vector('list', dis_length)
  
  ## number of regions
  reg_length <- length(rg)
  ## generate empty list for results
  mylist_regions <- vector('list', dis_length)
  
  ## initiate vector with the wrong names in the csv file
  misspelled_districts <- c() 
  
  ## loop through the rows of the dataframe
  for(i in 1:dis_length)
  { 
    ## if in the dataframe there is a correct district, do nothing
    if((ds[i] %in% ds_correct)==T) 
    { 
      misspelled_districts[i] <- NA
      print(paste("CORRECT name for the district:", ds[i]))
    }
    else { # otherwise, take note of the misspelling
      
      print(paste("Looking for similar results because there are no matches for the district:", ds[i]))
      
      ## the misspellings
      misspelled_districts[i] <- ds[i]
    }
  }  
  
  ## remove the NAs (that are the correct district names)
  misspelled_districts <- misspelled_districts[!is.na(misspelled_districts)]
  
  ## the vector with unique misspelled districts
  misspelled_districts <- unique(misspelled_districts)
  misspelled_districts <- as.character(misspelled_districts)
  
  ## the misspelled districts position in the dataframe
  miss_dis_index <- which((dataframe[[label]]) %in% misspelled_districts)
  
  if(length(miss_dis_index)==0)
  {
    stop("There are no misspellings") 
    
  } else {
      
      ## the misspelled district names
      respective_district <- ((dataframe[[label]])[miss_dis_index])
      respective_district <- as.character(respective_district)
      ## the respective region names
      respective_region <- ((dataframe[[label_higher_level]])[miss_dis_index])
      respective_region <- as.character(respective_region)
      
      check_df <- cbind(respective_district, respective_region)
      check_df <- as.data.frame(check_df)
      check_df <- unique(check_df)
      
      check_df$respective_district <- as.character(check_df$respective_district)
      check_df$respective_region <- as.character(check_df$respective_region)
      
      ## generate empty list for similar results
      Tot_misspell <- nrow(check_df)
      mylist_similar_districts <- vector('list', Tot_misspell)
      
      ## only consider alphanumeric characters of the misspelled/correct district names.
      ## same names' indices
      m_d <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_district)) 
      
      for(q in 1:Tot_misspell)
      {
        if((is.na(check_df$respective_region[q]))==F)
        {
          study_reg <- check_df$respective_region[q] 
          study_reg <- as.character(study_reg)
          
          ##find the similar district names within the subset
          correct_subset <- dplyr::filter(fullset, Region_Nam==study_reg)
          correct_subset_dis <- correct_subset$District_N
          correct_subset_dis <- as.character(correct_subset_dis)
          correct_subset_dis <- unique(correct_subset_dis)
          
          c_sub_d <- gsub("[^[:alnum:]]", "", tolower(correct_subset_dis))
          
          
          ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
          if((m_d[q] %in% c_sub_d)==T) 
          {
            number <- which(c_sub_d %in% m_d[q])
            mylist_similar_districts[[q]] <- number
            mylist_similar_districts[[q]] <- unique(correct_subset_dis[mylist_similar_districts[[q]]])  
          } else {
            
            ##find the similar district names within the subset
            mylist_similar_districts[[q]] <- agrep(m_d[q], c_sub_d, max.distance = distnc)
            #print(correct_subset_dis[mylist_similar_districts[[q]]])
            mylist_similar_districts[[q]] <- unique(correct_subset_dis[mylist_similar_districts[[q]]])  
          }
        } else {
          
          ## if there is no region information
          ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
          if((m_d[q] %in% c_d)==T) 
          {
            number <- which(c_d %in% m_d[q])
            mylist_similar_districts[[q]] <- number
            mylist_similar_districts[[q]] <- unique(ds_correct[mylist_similar_districts[[q]]])  
          } else {
            # if there are no case insensitive matches 
            ##look for similar district names in the whole correct names list
            mylist_similar_districts[[q]] <- agrep(m_d[q], c_d, max.distance = distnc)
            mylist_similar_districts[[q]] <- unique(ds_correct[mylist_similar_districts[[q]]])
          }
        }
      }
    }
  
  districts_identified <- plyr::ldply(mylist_similar_districts, rbind)
  districts_correct_name <- districts_identified
  
  misspelled_districts <- check_df$respective_district
  misspelled_districts <- as.character(misspelled_districts)
  
  corrections <- cbind(misspelled_districts, districts_correct_name)
  corrections <- as.data.frame(corrections)
  
  ## rename the column of corrected districts
  totcol <- length(corrections)
  
  for(q in 2:totcol)
  {
    newname <- paste("Correct_District_", q-1, sep="")
    colnames(corrections)[q] <- newname
  }
  
  ## cbind the region
  Region <- check_df$respective_region
  Region <- as.character(Region)
  
  corrections <- cbind(corrections, Region)
  #corrections <- as.data.frame(corrections)
  
  return(corrections)
}

