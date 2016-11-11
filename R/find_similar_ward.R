#' find_similar_ward / Find similar ward names 
#'  
#' Identifies the misspelled ward names in the project dataset and proposes a 
#' set of similar correct ward names.
#'
#' @param dataframe - the project dataframe with misspelled ward names
#' @param  label - the name of the column in the project dataframe storing the ward names. NOTE: The label must be specified as "label" (i.e. "ward", for instance)
#' @param  higher_level - MUST be either "District" or "Region".
#' @param  label_higher_level - the name of the column in the project dataframe storing the CORRECT district/region names. NOTE: The label must be specified as "label" (i.e. "district", for instance)
#' @param  shapefile - the shapefile object used to retrieve the legal names 
#' @param  distnc - Numeric value indicating the maximum distance allowed for a match (see ?agrep)
#' @param  only_in_distr - Optional. A character vector containing the names of the districts in which the study took place. Default = NA
#' @param  additional_places - Optional. A dataframe containing the additional names of the new wards (post 2012 census). Default = NA. NOTE: The ward/district/region column of this dataframe must be called like the @data slot in the wards shapefile 
#' 
#' @details \code{find_similar_ward} finds the misspellings in the ward column 
#' in the project dataset, looks for the respective most similar legal names and 
#' proposes them as potential corrections to the typos. The arguments "higher_level" 
#' and label_higher_level" allow for narrowing down the search to the similar 
#' wards contained in the district or region specified in the dataset. 
#' NOTE: these two arguments must be provided even if there is no district/region  
#' information (in that case, the function will look for similar matches in 
#' the whole list of legal ward names). The typos dataframe produced by this 
#' function can be used as argument of fix_typos. 
#' 
#' @return a new dataframe with: i) the ward misspellings, stored in the first 
#' column; ii) all the similar matches, each one stored under a separate column;
#' iii) the District/Region where the misspelled ward and the respective similar 
#' wards are (if the District/Region information is not available, the column will 
#' contain NAs).
#' @export
#' 
#' @examples 
#' #
#' 

find_similar_ward <- function(dataframe, label, 
                              higher_level, label_higher_level, 
                              shapefile, distnc,
                              only_in_distr=NA,
                              additional_places=NA)
{
  
  if(!is.na(additional_places))
  {
    shp_df <- select(shapefile@data, Ward_Name, District_N, Region_Nam)
    shp_df$Ward_Name <- as.character(shp_df$Ward_Name)
    shp_df$District_N <- as.character(shp_df$District_N)
    shp_df$Region_Nam <- as.character(shp_df$Region_Nam)
    legal_names <- c(shp_df$Ward_Name, additional_places$Ward_Name)
    fullset <- rbind(shp_df, additional_places)
    
  } else {
    legal_names <- shapefile@data$Ward_Name
    fullset <- select(shapefile@data, Ward_Name, District_N, Region_Nam)
  }
  
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)
  
  wrd <- unique(dataframe[[label]])
  wrd <- as.character(wrd)
  
  ## number of wards
  ward_length <- length(wrd)
  ## generate empty list for results
  mylist_wards <- vector('list', ward_length)
  
  wrd_correct <- legal_names
  wrd_correct <- as.character(legal_names)
  c_v <- gsub("[^[:alnum:]]", "", tolower(wrd_correct))
  
  ## initiate vector with the wrong names in the csv file
  misspelled_wards <- c() 
  
  ## loop through the unique wards
  for(i in 1:ward_length)
  { 
    ## if in the dataframe there is a correct ward, do nothing
    if((wrd[i] %in% wrd_correct)==T) 
    { 
      misspelled_wards[i] <- NA
      print(paste("CORRECT name for the ward:", wrd[i]))
      
    } else { # otherwise, take note of the misspelling
      
      print(paste("Looking for similar results because there are no matches for the ward:", wrd[i]))
      
      ## the misspellings
      misspelled_wards[i] <- wrd[i]
    }
  }  
  
  ## remove the NAs (that are the correct ward names)
  misspelled_wards <- misspelled_wards[!is.na(misspelled_wards)]
  
  ## the vector with unique misspelled wards
  misspelled_wards <- unique(misspelled_wards)
  misspelled_wards <- as.character(misspelled_wards)
  
  ## the misspelled wards position in the dataframe
  miss_wards_index <- which((dataframe[[label]]) %in% misspelled_wards)
  
  ## the misspelled wards names
  respective_ward <- ((dataframe[[label]])[miss_wards_index])
  respective_ward <- as.character(respective_ward)
  
  if(higher_level=="District")
  {
    if (!is.na(only_in_distr))
    {
      
      only_in_distr <- as.character(only_in_distr)
      
      ## respective_ward: the misspelled village names
      
      ## the respective district name
      respective_district <- c()
      respective_district <- as.character(respective_district)
      
      ## generate empty list for similar results
      Tot_misspell <- length(respective_ward)
      mylist_similar_wards <- vector('list', Tot_misspell)
      
      mylist_respective_districts <- vector('list', Tot_misspell)
      
      ##find the similar ward names within the subset
      correct_subset <- dplyr::filter(fullset, District_N%in%only_in_distr)
      #correct_subset <- correct_subset[unique(correct_subset$Vil_Mtaa_N),] 
      
      correct_subset_ward <- correct_subset$Ward_Name
      correct_subset_ward <- as.character(correct_subset_ward)
      
      correct_subset_dis <- correct_subset$District_N
      correct_subset_dis <- as.character(correct_subset_dis)
      
      ## only consider alphanumeric characters of the misspelled/correct village names.
      m_v <- gsub("[^[:alnum:]]", "", tolower(respective_ward)) 
      c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_ward))
      
      for(m in 1:Tot_misspell)
      {
        ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
        if((m_v[m] %in% c_sub_v)==T) 
        {
          number <- which(c_sub_v %in% m_v[m])
          mylist_similar_wards[[m]] <- number
          mylist_similar_wards[[m]] <- correct_subset_ward[mylist_similar_wards[[m]]]
          mylist_respective_districts[[m]] <- number
          mylist_respective_districts[[m]] <- correct_subset_dis[mylist_respective_districts[[m]]]
          
          unique_wards <- !duplicated(mylist_similar_wards[[m]])
          unique_ind <- seq_along(mylist_similar_wards[[m]])[unique_wards]
          
          mylist_similar_wards[[m]] <- mylist_similar_wards[[m]][unique_ind]
          mylist_respective_districts[[m]] <- mylist_respective_districts[[m]][unique_ind]
      
        } else {
          
          ##find the similar ward names within the subset
          number <- agrep(m_v[m], c_sub_v, max.distance = distnc)
          mylist_similar_wards[[m]] <- number
          mylist_similar_wards[[m]] <- correct_subset_ward[mylist_similar_wards[[m]]]
          mylist_respective_districts[[m]] <- number
          mylist_respective_districts[[m]] <- correct_subset_dis[mylist_respective_districts[[m]]]
          
          unique_wards <- !duplicated(mylist_similar_wards[[m]])
          unique_ind <- seq_along(mylist_similar_wards[[m]])[unique_wards]
          
          mylist_similar_wards[[m]] <- mylist_similar_wards[[m]][unique_ind]
          mylist_respective_districts[[m]] <- mylist_respective_districts[[m]][unique_ind]
        }
      }
      
      wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
      wards_correct_name <- wards_identified
      wards_correct_name <- as.data.frame(wards_correct_name)
      totcol_wrdcorrname <- length(wards_correct_name)
      
      for(l in 1:totcol_wrdcorrname)
      {
        newname <- paste("Correct_Ward_", l, sep="")
        colnames(wards_correct_name)[l] <- newname
      }
      
      
      misspelled_wards <- respective_ward
      misspelled_wards <- as.character(misspelled_wards)
      
      corrections <- cbind(misspelled_wards, wards_correct_name)
      corrections <- as.data.frame(corrections)
      
      districts_identified <- plyr::ldply(mylist_respective_districts, rbind)
      districts_correct_name <- districts_identified
      totcol_discorrname <- length(districts_correct_name)
      
      for(Q in 1:totcol_discorrname)
      {
        newname <- paste("Respective_District_", Q, sep="")
        colnames(districts_correct_name)[Q] <- newname
      }
      
      corrections <- cbind(corrections, districts_correct_name)
      
    } else {
      ## i.e. if the argument only_in_distr is NA
      
      ds <- unique(dataframe[[label_higher_level]])
      ds <- as.character(ds)
      
      ## number of districts
      dis_length <- length(ds)
      ## generate empty list for results
      mylist_districts <- vector('list', dis_length)
      
      ## the respective district names
      respective_district <- ((dataframe[[label_higher_level]])[miss_wards_index])
      respective_district <- as.character(respective_district)
      
      check_df <- cbind(respective_ward, respective_district)
      check_df <- as.data.frame(check_df)
      check_df <- unique(check_df)
      
      check_df$respective_ward <- as.character(check_df$respective_ward)
      check_df$respective_district <- as.character(check_df$respective_district)
      
      ## generate empty list for similar results
      Tot_misspell <- nrow(check_df)
      mylist_similar_wards <- vector('list', Tot_misspell)
      
      ## only consider alphanumeric characters of the misspelled/correct village names.
      ## same names' indices
      m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_ward)) 
      
      for(q in 1:Tot_misspell)
      {
        if((is.na(check_df$respective_district[q]))==F)
        {
          study_district <- check_df$respective_district[q] 
          study_district <- as.character(study_district)
          
          ##find the similar village names within the subset
          correct_subset <- dplyr::filter(fullset, District_N==study_district)
          correct_subset_wards <- correct_subset$Ward_Name
          correct_subset_wards <- as.character(correct_subset_wards)
          correct_subset_wards <- unique(correct_subset_wards)
          
          ##find the similar ward names within the subset
          mylist_similar_wards[[q]] <- agrep(check_df$respective_ward[q], correct_subset_wards, max.distance = distnc)
          mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])
          
          c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_wards))
          
          
          ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
          if((m_v[q] %in% c_sub_v)==T) 
          {
            number <- which(c_sub_v %in% m_v[q])
            mylist_similar_wards[[q]] <- number
            mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])  
            
          } else {
            
            ##find the similar village names within the subset
            mylist_similar_wards[[q]] <- agrep(m_v[q], c_sub_v, max.distance = distnc)
            mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])  
          }
          
        } else {
          ## if there is no district information
          ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
          if((m_v[q] %in% c_v)==T) 
          {
            number <- which(c_v %in% m_v[q])
            mylist_similar_wards[[q]] <- number
            mylist_similar_wards[[q]] <- unique(wrd_correct[mylist_similar_wards[[q]]])  
          } else {
            # if there are no case insensitive matches 
            ##look for similar village names in the whole correct village list
            mylist_similar_wards[[q]] <- agrep(m_v[q], c_v, max.distance = distnc)
            mylist_similar_wards[[q]] <- unique(wrd_correct[mylist_similar_wards[[q]]])
          }
        }
      }
      
      wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
      wards_correct_name <- wards_identified
      
      misspelled_wards <- check_df$respective_ward
      misspelled_wards <- as.character(misspelled_wards)
      
      corrections <- cbind(misspelled_wards, wards_correct_name)
      corrections <- as.data.frame(corrections)
      
      ## rename the column of corrected wards
      totcol <- length(corrections)
      
      for(q in 2:totcol)
      {
        newname <- paste("Correct_Ward_", q-1, sep="")
        colnames(corrections)[q] <- newname
      }
      
      ## cbind the District
      District <- check_df$respective_district
      corrections <- cbind(corrections, District)
    }
  } else {
    ## higher_level = "Region"
    rg <- unique(dataframe[[label_higher_level]])
    rg <- as.character(rg)
    
    ## number of regions
    reg_length <- length(rg)
    ## generate empty list for results
    mylist_regions <- vector('list', reg_length)
    
    ## the respective region names
    respective_region <- ((dataframe[[label_higher_level]])[miss_wards_index])
    respective_region <- as.character(respective_region)
    
    check_df <- cbind(respective_ward, respective_region)
    check_df <- as.data.frame(check_df)
    check_df <- unique(check_df)
    
    check_df$respective_ward <- as.character(check_df$respective_ward)
    check_df$respective_region <- as.character(check_df$respective_region)
    
    ## generate empty list for similar results
    Tot_misspell <- nrow(check_df)
    mylist_similar_wards <- vector('list', Tot_misspell)
    
    ## only consider alphanumeric characters of the misspelled/correct village names.
    ## same names' indices
    m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_ward)) 
    
    for(q in 1:Tot_misspell)
    {
      if((is.na(check_df$respective_region[q]))==F)
      {
        study_region <- check_df$respective_region[q] 
        study_region <- as.character(study_region)
        
        ##find the similar village names within the subset
        correct_subset <- dplyr::filter(fullset, Region_Nam==study_region)
        correct_subset_wards <- correct_subset$Ward_Name
        correct_subset_wards <- as.character(correct_subset_wards)
        correct_subset_wards <- unique(correct_subset_wards)
        
        ##find the similar village names within the subset
        mylist_similar_wards[[q]] <- agrep(check_df$respective_ward[q], correct_subset_wards, max.distance = distnc)
        mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])
        
        c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_wards))
        
        ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
        if((m_v[q] %in% c_sub_v)==T) 
        {
          number <- which(c_sub_v %in% m_v[q])
          mylist_similar_wards[[q]] <- number
          mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])  
          
        } else {
          
          ##find the similar names within the subset
          mylist_similar_wards[[q]] <- agrep(m_v[q], c_sub_v, max.distance = distnc)
          mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])  
        }
        
      } else {
        ## if there is no region information
        ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
        if((m_v[q] %in% c_v)==T) 
        {
          number <- which(c_v %in% m_v[q])
          mylist_similar_wards[[q]] <- number
          mylist_similar_wards[[q]] <- unique(wrd_correct[mylist_similar_wards[[q]]])  
        } else {
          # if there are no case insensitive matches 
          ##look for similar village names in the whole correct village list
          mylist_similar_wards[[q]] <- agrep(m_v[q], c_v, max.distance = distnc)
          mylist_similar_wards[[q]] <- unique(wrd_correct[mylist_similar_wards[[q]]])
        }
      }
    }
    
    wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
    wards_correct_name <- wards_identified
    
    misspelled_wards <- check_df$respective_ward
    misspelled_wards <- as.character(misspelled_wards)
    
    corrections <- cbind(misspelled_wards, wards_correct_name)
    corrections <- as.data.frame(corrections)
    
    ## rename the column of corrected wards
    totcol <- length(corrections)
    
    for(q in 2:totcol)
    {
      newname <- paste("Correct_Ward_", q-1, sep="")
      colnames(corrections)[q] <- newname
    }
    
    ## cbind the District
    Region <- check_df$respective_region
    corrections <- cbind(corrections, Region)
    
  } 
  return(corrections)
}

