#' find_similar_region / Find similar region names 
#'  
#' Identifies the misspelled region names in the project dataset and proposes a 
#' set of similar correct region names.
#'
#' @param dataframe - the project dataframe with misspelled region names
#' @param label - the name of the column in the project dataframe storing the region names. NOTE: The label must be specified as "label" (i.e. "region", for instance)
#' @param distnc - Numeric value indicating the maximum distance allowed for a match (see ?agrep)
#' @param shapefile - the shapefile object used to retrieve the legal names 
#' @param additional_places - Optional. A dataframe containing the additional names of the new districts (post 2012 census). Default = NA. NOTE: The district/region column of this dataframe must be called like the @data slot in the wards shapefile 
#' 
#' @details \code{find_similar_region} finds the misspellings in the region column 
#' in the project dataset, looks for the respective most similar legal names and 
#' proposes them as potential corrections to the typos. The typos dataframe produced 
#' by this function can be used as argument of fix_typos. 
#' 
#' @return a new dataframe with: i) the region misspellings, stored in the first 
#' column; ii) all the similar matches, each one stored under a separate column.
#' @export
#' 
#' @examples 
#' #
#' 

find_similar_region <- function(dataframe, label, distnc,
                                shapefile, additional_places=NA)
{
  if(!is.na(additional_places))
  {
    shp_df <- select(shapefile@data, Region_Nam)
    shp_df$Region_Nam <- as.character(shp_df$Region_Nam)
    legal_names <- c(shp_df$Region_Nam, additional_places$Region_Nam)
    fullset <- rbind(shp_df, additional_places)
    
  } else {
    legal_names <- shapefile@data$Region_Nam
    fullset <- select(shapefile@data, Region_Nam)
  }
  
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)
  
  rg <- unique(dataframe[[label]])
  rg <- as.character(rg)
  
  rg_correct <- legal_names
  rg_correct <- as.character(rg_correct)
  
  ## number of regions
  reg_length <- length(rg)
  ## generate empty list for results
  mylist_regions <- vector('list', reg_length)
  
  ## list with the index of the regions
  mylist_nums <- vector('list', ntot)
  
  ## list with the index of the rows 
  mylist_rows <- vector('list', ntot)
  
  ## initiate vector with the wrong names in the csv file
  misspelled_regions <- c() 
  
  ## loop through the rows of the dataframe
  for(i in 1:reg_length)
  { 
    ## if in the dataframe there is a correct region, do nothing
    if((rg[i] %in% rg_correct)==T) 
    { 
      #print(paste("CORRECT name for the region:", rg[i]))
      misspelled_regions[i] <- NA
    }
    else { # otherwise, take note of the misspelling
      
      print(paste("Looking for similar results because there are no matches for the region:", rg[i]))
      
      ## the misspellings
      misspelled_regions[i] <- rg[i]
      
    }
  }  
  
  ## remove the NAs (that are the correct region names)
  misspelled_regions <- misspelled_regions[!is.na(misspelled_regions)]
  
  ## the vector with unique misspelled regions
  misspelled_regions <- unique(misspelled_regions)
  
  ## loop through the unique misspelled regions 
  Tot_misspell <- length(misspelled_regions)
  
  ## generate empty list for similar results
  mylist_similar_regions <- vector('list', Tot_misspell)
  
  ## initialise the real_typo vector
  real_typo <- c(rep(NA, Tot_misspell))
  real_typo <- as.character(real_typo)
  
  ## only consider alphanumeric characters of the misspelled/correct region names.
  ## same names' indices
  m_r <- gsub("[^[:alnum:]]", "", tolower(misspelled_regions)) 
  c_r <- gsub("[^[:alnum:]]", "", tolower(rg_correct))
  
  for(j in 1:Tot_misspell)
  {
    
    ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
    if((m_r[j] %in% c_r)==T) 
    {
      real_typo[j] <- FALSE
      number <- which(c_r %in% m_r[j])
      mylist_similar_regions[[j]] <- number
      
    } else {
      real_typo[j] <- TRUE
      ## find the similar regions' names
      mylist_similar_regions[[j]] <- agrep(m_r[j], c_r, max.distance = distnc)
      #print(rg_correct[mylist_similar_regions[[j]]])
    }
  }
  
  for(a in 1:length(mylist_similar_regions))
  {
    mylist_similar_regions[[a]] <- unique(rg_correct[mylist_similar_regions[[a]]])
  }
  
  tot_sim <- length(mylist_similar_regions)
  
  regions_identified <- rep(NA, tot_sim)
  
  for(k in 1:tot_sim)
  {
    if(((length(mylist_similar_regions[[k]]))==1)==T)
    {
      regions_identified[k] <- mylist_similar_regions[[k]] } else {
        regions_identified[k] <- NA
      }
  }
  
  regions_correct_name <- regions_identified
  
  corrections <- cbind(misspelled_regions, regions_correct_name)
  corrections <- as.data.frame(corrections)
  
  ## cbind the real_typo column
  real_typo <- as.character(real_typo)
  corrections <- cbind(corrections, real_typo)
  
  tot_obs <- nrow(corrections)
  typo_level <- c()
  typo_level <- as.character(typo_level)
  typo_level <- rep("Region", tot_obs)
  corrections <- cbind(corrections, typo_level)
  
  return(corrections)
}
