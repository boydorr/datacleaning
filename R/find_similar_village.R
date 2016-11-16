#' find_similar_village / Find similar village names 
#'  
#' Identifies the misspelled village names in the project dataset and proposes a 
#' set of similar correct village names.
#'
#' @param dataframe - the project dataframe with misspelled village names
#' @param  label - the name of the column in the project dataframe storing the village names. NOTE: The label must be specified as "label" (i.e. "village", for instance)
#' @param  higher_level - MUST be one of the following: "Ward", District" or "Region".
#' @param  label_higher_level - the name of the column in the project dataframe storing the CORRECT ward/district/region names. NOTE: The label must be specified as "label" (i.e. "ward", for instance)
#' @param  shapefile - the shapefile object used to retrieve the legal names 
#' @param  distnc - Numeric value indicating the maximum distance allowed for a match (see ?agrep)
#' @param  only_in_distr - Optional. A character vector containing the names of the districts in which the study took place. Default = NA
#' @param  additional_places - Optional. A dataframe containing the additional names of the new villages (post 2012 census). Default = NA. NOTE: The village/ward/district/region column of this dataframe must be called like the @data slot in the village shapefile 
#' 
#' @details \code{find_similar_village} finds the misspellings in the village column 
#' in the project dataset, looks for the respective most similar legal names and 
#' proposes them as potential corrections to the typos. The arguments "higher_level" 
#' and label_higher_level" allow for narrowing down the search to the similar 
#' villages contained in the ward, district or region specified in the dataset. 
#' NOTE: these two arguments must be provided even if there is no ward/district/region  
#' information (in that case, the function will look for similar matches in 
#' the whole list of legal village names). The typos dataframe produced by this 
#' function can be used as argument of fix_typos. 
#' 
#' @return a new dataframe with: i) the village misspellings, stored in the first 
#' column; ii) all the similar matches, each one stored under a separate column;
#' iii) the Ward/District/Region where the misspelled village and the respective similar 
#' villages are (if the Ward/District/Region information is not available, the column will 
#' contain NAs).
#' @export
#' 
#' @examples 
#' #
#' 

find_similar_village <- function(dataframe, label, label_higher_level, 
                                 higher_level, shapefile, only_in_distr=NA, 
                                 distnc, additional_places=NA)
{
  
  if(!is.na(additional_places))
  {
    shp_df <- select(shapefile@data, Vil_Mtaa_N, Ward_Name, District_N, Region_Nam)
    shp_df$Vil_Mtaa_N <- as.character(shp_df$Vil_Mtaa_N)
    shp_df$Ward_Name <- as.character(shp_df$Ward_Name)
    shp_df$District_N <- as.character(shp_df$District_N)
    shp_df$Region_Nam <- as.character(shp_df$Region_Nam)
    legal_names <- c(shp_df$Vil_Mtaa_N, additional_places$Vil_Mtaa_N)
    fullset <- rbind(shp_df, additional_places)
    
  } else {
    legal_names <- shapefile@data$Vil_Mtaa_N
    fullset <- select(shapefile@data, Vil_Mtaa_N, Ward_Name, District_N, Region_Nam)
  }
  
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)
  
  vll <- unique(dataframe[[label]])
  vll <- as.character(vll)
  
  ## number of villages
  vill_length <- length(vll)
  ## generate empty list for results
  mylist_similar_villages <- vector('list', vill_length)
  
  vll_correct <- legal_names
  vll_correct <- as.character(vll_correct)
  c_v <- gsub("[^[:alnum:]]", "", tolower(vll_correct))
  
  ## initiate vector with the wrong names in the csv file
  misspelled_villages <- c() 
  
  ## loop through the unique villages
  for(i in 1:vill_length)
  { 
    ## if in the dataframe there is a correct village, do nothing
    if((vll[i] %in% vll_correct)==T) 
    { 
      misspelled_villages[i] <- NA
      print(paste("CORRECT name for the village:", vll[i]))
    }
    else { # otherwise, take note of the misspelling
      
      print(paste("Looking for similar results because there are no matches for the village:", vll[i]))
      
      ## the misspellings
      misspelled_villages[i] <- vll[i]
    }
  }  
  
  ## remove the NAs (that are the correct village names)
  misspelled_villages <- misspelled_villages[!is.na(misspelled_villages)]
  
  ## the vector with unique misspelled villages
  misspelled_villages <- unique(misspelled_villages)
  misspelled_villages <- as.character(misspelled_villages)
  
  ## the misspelled villages position in the dataframe
  miss_vill_index <- which((dataframe[[label]]) %in% misspelled_villages)
  
  ## the misspelled village names
  respective_vill <- ((dataframe[[label]])[miss_vill_index])
  respective_vill <- as.character(respective_vill)
  
  
  if(higher_level=="Ward")
  {
    wrd <- unique(dataframe[[label_higher_level]])
    wrd <- as.character(wrd)
    
    ## number of wards
    ward_length <- length(wrd)
    ## generate empty list for results
    mylist_wards <- vector('list', ward_length)
    
    ## the respective ward names
    respective_ward <- ((dataframe[[label_higher_level]])[miss_vill_index])
    respective_ward <- as.character(respective_ward)
    
    check_df <- cbind(respective_vill, respective_ward)
    check_df <- as.data.frame(check_df)
    check_df <- unique(check_df)
    
    check_df$respective_vill <- as.character(check_df$respective_vill)
    check_df$respective_ward <- as.character(check_df$respective_ward)
    
    ## generate empty list for similar results
    Tot_misspell <- nrow(check_df)
    mylist_similar_villages <- vector('list', Tot_misspell)
    
    ## initialise the real_typo vector
    real_typo <- c(rep(NA, Tot_misspell))
    real_typo <- as.character(real_typo)
    
    ## only consider alphanumeric characters of the misspelled/correct village names.
    ## same names' indices
    m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_vill)) 
    
    
    for(q in 1:Tot_misspell)
    {
      if((is.na(check_df$respective_ward[q]))==F)
      {
        study_ward <- check_df$respective_ward[q] 
        study_ward <- as.character(study_ward)
        
        ##find the similar village names within the subset
        correct_subset <- dplyr::filter(fullset, Ward_Name==study_ward)
        correct_subset_vill <- correct_subset$Vil_Mtaa_N
        correct_subset_vill <- as.character(correct_subset_vill)
        correct_subset_vill <- unique(correct_subset_vill)
        
        ##find the similar ward names within the subset
        mylist_similar_villages[[q]] <- agrep(check_df$respective_vill[q], correct_subset_vill, max.distance = distnc)
        mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
        
        c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
        
        
        ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
        if((m_v[q] %in% c_sub_v)==T) 
        {
          number <- which(c_sub_v %in% m_v[q])
          mylist_similar_villages[[q]] <- number
          mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])  
          real_typo[q] <- FALSE
          
        } else {
          
          ##find the similar ward names within the subset
          mylist_similar_villages[[q]] <- agrep(m_v[q], c_sub_v, max.distance = distnc)
          mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
          real_typo[q] <- TRUE
        }
        
      } else {
        ## if there is no ward information
        ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
        if((m_v[q] %in% c_v)==T) 
        {
          number <- which(c_v %in% m_v[q])
          mylist_similar_villages[[q]] <- number
          mylist_similar_villages[[q]] <- unique(vll_correct[mylist_similar_villages[[q]]]) 
          real_typo[q] <- FALSE
        } else {
          # if there are no case insensitive matches 
          ##look for similar village names in the whole correct village list
          mylist_similar_villages[[q]] <- agrep(m_v[q], c_v, max.distance = distnc)
          mylist_similar_villages[[q]] <- unique(vll_correct[mylist_similar_villages[[q]]])
          real_typo[q] <- TRUE
        }
      }
    }
    
    villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
    villages_correct_name <- villages_identified
    
    misspelled_villages <- check_df$respective_vill
    misspelled_villages <- as.character(misspelled_villages)
    
    corrections <- cbind(misspelled_villages, villages_correct_name)
    corrections <- as.data.frame(corrections)
    
    ## rename the column of corrected wards
    totcol <- length(corrections)
    
    for(q in 2:totcol)
    {
      newname <- paste("Correct_Village_", q-1, sep="")
      colnames(corrections)[q] <- newname
    }
    
    ## cbind the Ward
    Ward <- check_df$respective_ward
    corrections <- cbind(corrections, Ward)
    
    ## cbind the real_typo column
    real_typo <- as.character(real_typo)
    corrections <- cbind(corrections, real_typo)
    
    
  } else {
    
    if(higher_level=="District")
    {
      if (!is.na(only_in_distr))
      {
        only_in_distr <- as.character(only_in_distr)
        
        ## respective_vill: the misspelled village names
        
        ## the respective district name
        respective_district <- c()
        respective_district <- as.character(respective_district)
        
        ## generate empty list for similar results
        Tot_misspell <- length(respective_vill)
        mylist_similar_villages <- vector('list', Tot_misspell)
        mylist_respective_districts <- vector('list', Tot_misspell)
        
        ## initialise the real_typo vector
        real_typo <- c(rep(NA, Tot_misspell))
        real_typo <- as.character(real_typo)
        
        ##find the similar village names within the subset
        correct_subset <- dplyr::filter(fullset, District_N%in%only_in_distr)
        #correct_subset <- correct_subset[unique(correct_subset$Vil_Mtaa_N),] 
        
        correct_subset_vill <- correct_subset$Vil_Mtaa_N
        correct_subset_vill <- as.character(correct_subset_vill)
        
        correct_subset_dis <- correct_subset$District_N
        correct_subset_dis <- as.character(correct_subset_dis)
        
        ## only consider alphanumeric characters of the misspelled/correct village names.
        m_v <- gsub("[^[:alnum:]]", "", tolower(respective_vill)) 
        c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
        
        for(m in 1:Tot_misspell)
        {
          ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
          if((m_v[m] %in% c_sub_v)==T) 
          {
            real_typo[m] <- FALSE
            
            number <- which(c_sub_v %in% m_v[m])
            mylist_similar_villages[[m]] <- number
            mylist_similar_villages[[m]] <- correct_subset_vill[mylist_similar_villages[[m]]]  
            mylist_respective_districts[[m]] <- number
            mylist_respective_districts[[m]] <- correct_subset_dis[mylist_respective_districts[[m]]]
            
            unique_vill <- !duplicated(mylist_similar_villages[[m]])
            unique_ind <- seq_along(mylist_similar_villages[[m]])[unique_vill]
            
            mylist_similar_villages[[m]] <- mylist_similar_villages[[m]][unique_ind]
            mylist_respective_districts[[m]] <- mylist_respective_districts[[m]][unique_ind]
            
            
          } else {
            
            real_typo[m] <- TRUE
            
            ##find the similar village names within the subset
            number <- agrep(m_v[m], c_sub_v, max.distance = distnc)
            mylist_similar_villages[[m]] <- number
            mylist_similar_villages[[m]] <- correct_subset_vill[mylist_similar_villages[[m]]]  
            mylist_respective_districts[[m]] <- number
            mylist_respective_districts[[m]] <- correct_subset_dis[mylist_respective_districts[[m]]]
            
            unique_vill <- !duplicated(mylist_similar_villages[[m]])
            unique_ind <- seq_along(mylist_similar_villages[[m]])[unique_vill]
            
            mylist_similar_villages[[m]] <- mylist_similar_villages[[m]][unique_ind]
            mylist_respective_districts[[m]] <- mylist_respective_districts[[m]][unique_ind]
            
          }
        }
        
        villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
        villages_correct_name <- villages_identified
        villages_correct_name <- as.data.frame(villages_correct_name)
        totcol_villcorrname <- length(villages_correct_name)
        for(l in 1:totcol_villcorrname)
        {
          newname <- paste("Correct_Village_", l, sep="")
          colnames(villages_correct_name)[l] <- newname
        }
        
        misspelled_villages <- respective_vill
        misspelled_villages <- as.character(misspelled_villages)
        
        corrections <- cbind(misspelled_villages, villages_correct_name)
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
        
        ## cbind the real_typo column
        real_typo <- as.character(real_typo)
        corrections <- cbind(corrections, real_typo)
        
      } else {
        
        ds <- unique(dataframe[[label_higher_level]])
        ds <- as.character(ds)
        
        ## number of districts
        dis_length <- length(ds)
        ## generate empty list for results
        mylist_districts <- vector('list', dis_length)
        
        ## the respective district names
        respective_district <- ((dataframe[[label_higher_level]])[miss_vill_index])
        respective_district <- as.character(respective_district)
        
        check_df <- cbind(respective_vill, respective_district)
        check_df <- as.data.frame(check_df)
        check_df <- unique(check_df)
        
        check_df$respective_vill <- as.character(check_df$respective_vill)
        check_df$respective_district <- as.character(check_df$respective_district)
        
        ## generate empty list for similar results
        Tot_misspell <- nrow(check_df)
        mylist_similar_villages <- vector('list', Tot_misspell)
        
        ## initialise the real_typo vector
        real_typo <- c(rep(NA, Tot_misspell))
        real_typo <- as.character(real_typo)
        
        ## only consider alphanumeric characters of the misspelled/correct village names.
        ## same names' indices
        m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_vill)) 
        
        for(q in 1:Tot_misspell)
        {
          if((is.na(check_df$respective_district[q]))==F)
          {
            study_district <- check_df$respective_district[q] 
            study_district <- as.character(study_district)
            
            ##find the similar village names within the subset
            correct_subset <- dplyr::filter(fullset, District_N==study_district)
            correct_subset_vill <- correct_subset$Vil_Mtaa_N
            correct_subset_vill <- as.character(correct_subset_vill)
            correct_subset_vill <- unique(correct_subset_vill)
            
            ##find the similar village names within the subset
            mylist_similar_villages[[q]] <- agrep(check_df$respective_vill[q], correct_subset_vill, max.distance = distnc)
            mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
            
            c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
            
            
            ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
            if((m_v[q] %in% c_sub_v)==T) 
            {
              real_typo[q] <- FALSE
              number <- which(c_sub_v %in% m_v[q])
              mylist_similar_villages[[q]] <- number
              mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])  
              
            } else {
              real_typo[q] <- TRUE
              ##find the similar village names within the subset
              mylist_similar_villages[[q]] <- agrep(m_v[q], c_sub_v, max.distance = distnc)
              mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])  
            }
          } else {
            ## if there is no district information
            ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
            if((m_v[q] %in% c_v)==T) 
            {
              real_typo[q] <- FALSE
              number <- which(c_v %in% m_v[q])
              mylist_similar_villages[[q]] <- number
              mylist_similar_villages[[q]] <- unique(vll_correct[mylist_similar_villages[[q]]])  
            } else {
              real_typo[q] <- TRUE
              # if there are no case insensitive matches 
              ##look for similar village names in the whole correct village list
              mylist_similar_villages[[q]] <- agrep(m_v[q], c_v, max.distance = distnc)
              mylist_similar_villages[[q]] <- unique(vll_correct[mylist_similar_villages[[q]]])
            }
          }
        } 
        villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
        villages_correct_name <- villages_identified
        
        misspelled_villages <- check_df$respective_vill
        misspelled_villages <- as.character(misspelled_villages)
        
        corrections <- cbind(misspelled_villages, villages_correct_name)
        corrections <- as.data.frame(corrections)
        
        ## rename the column of corrected wards
        totcol <- length(corrections)
        
        for(s in 2:totcol)
        {
          newname <- paste("Correct_Village_", s-1, sep="")
          colnames(corrections)[s] <- newname
        }
        
        ## cbind the District
        District <- check_df$respective_district
        corrections <- cbind(corrections, District)
        
        ## cbind the real_typo column
        real_typo <- as.character(real_typo)
        corrections <- cbind(corrections, real_typo)
        
      }
      
    } else {
      
      ## higher_level = Region
      rg <- unique(dataframe[[label_higher_level]])
      rg <- as.character(rg)
      
      ## number of wards
      reg_length <- length(rg)
      ## generate empty list for results
      mylist_regions <- vector('list', reg_length)
      
      ## the respective region names
      respective_region <- ((dataframe[[label_higher_level]])[miss_vill_index])
      respective_region <- as.character(respective_region)
      
      check_df <- cbind(respective_vill, respective_region)
      check_df <- as.data.frame(check_df)
      check_df <- unique(check_df)
      
      check_df$respective_vill <- as.character(check_df$respective_vill)
      check_df$respective_region <- as.character(check_df$respective_region)
      
      ## generate empty list for similar results
      Tot_misspell <- nrow(check_df)
      mylist_similar_villages <- vector('list', Tot_misspell)
      
      ## initialise the real_typo vector
      real_typo <- c(rep(NA, Tot_misspell))
      real_typo <- as.character(real_typo)
      
      ## only consider alphanumeric characters of the misspelled/correct village names.
      ## same names' indices
      m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_vill)) 
      
      for(q in 1:Tot_misspell)
      {
        if((is.na(check_df$respective_region[q]))==F)
        {
          study_region <- check_df$respective_region[q] 
          study_region <- as.character(study_region)
          
          ##find the similar village names within the subset
          correct_subset <- dplyr::filter(fullset, Region_Nam==study_region)
          correct_subset_vill <- correct_subset$Vil_Mtaa_N
          correct_subset_vill <- as.character(correct_subset_vill)
          correct_subset_vill <- unique(correct_subset_vill)
          
          ##find the similar village names within the subset
          mylist_similar_villages[[q]] <- agrep(check_df$respective_vill[q], correct_subset_vill, max.distance = distnc)
          mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
          
          c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
          
          ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
          if((m_v[q] %in% c_sub_v)==T) 
          {
            real_typo[q] <- FALSE
            number <- which(c_sub_v %in% m_v[q])
            mylist_similar_villages[[q]] <- number
            mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])  
            
          } else {
            real_typo[q] <- TRUE
            ##find the similar village names within the subset
            mylist_similar_villages[[q]] <- agrep(m_v[q], c_sub_v, max.distance = distnc)
            mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])  
          }
          
        } else {
          ## if there is no region information
          ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
          if((m_v[q] %in% c_v)==T) 
          {
            real_typo[q] <- FALSE
            number <- which(c_v %in% m_v[q])
            mylist_similar_villages[[q]] <- number
            mylist_similar_villages[[q]] <- unique(vll_correct[mylist_similar_villages[[q]]])  
          } else {
            real_typo[q] <- TRUE
            # if there are no case insensitive matches 
            ##look for similar village names in the whole correct village list
            mylist_similar_villages[[q]] <- agrep(m_v[q], c_v, max.distance = distnc)
            mylist_similar_villages[[q]] <- unique(vll_correct[mylist_similar_villages[[q]]])
          }
        }
      }
      
      villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
      villages_correct_name <- villages_identified
      
      misspelled_villages <- check_df$respective_vill
      misspelled_villages <- as.character(misspelled_villages)
      
      corrections <- cbind(misspelled_villages, villages_correct_name)
      corrections <- as.data.frame(corrections)
      
      ## rename the column of corrected wards
      totcol <- length(corrections)
      
      for(q in 2:totcol)
      {
        newname <- paste("Correct_Village_", q-1, sep="")
        colnames(corrections)[q] <- newname
      }
      
      ## cbind the Region
      Region <- check_df$respective_region
      corrections <- cbind(corrections, Region)
      
      ## cbind the real_typo column
      real_typo <- as.character(real_typo)
      corrections <- cbind(corrections, real_typo)
      
    }
  }
  
  tot_obs <- nrow(corrections)
  typo_level <- c()
  typo_level <- as.character(typo_level)
  typo_level <- rep("Village", tot_obs)
  corrections <- cbind(corrections, typo_level)
  
  return(corrections)
}

