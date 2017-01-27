#' @title Find similar village names
#'
#' @description Identifies the misspelled village names in the project dataset
#' and proposes a set of similar correct village names.
#'
#' @param dataframe the project dataframe with misspelled village names
#' @param  label the name of the column in the project dataframe storing the village names. NOTE: The label must be specified as "label" (i.e. "village", for instance)
#' @param  higher_level MUST be one of the following: "Ward", District" or "Region". If "Ward", the argument look_ward must be FALSE.
#' @param  label_higher_level the name of the column in the project dataframe storing the CORRECT ward/district/region names. NOTE: The label must be specified as "label" (i.e. "ward", for instance)
#' @param  shapefile the shapefile object used to retrieve the legal names
#' @param  distnc numeric value indicating the maximum distance allowed for a match (see ?agrep)
#' @param  only_in_distr (optional) a character vector containing the names of the districts in which the study took place. Default = NA
#' @param  additional_places (optional) a dataframe containing the additional names of the new villages (post 2012 census). Default = NA. NOTE: The village/ward/district/region column of this dataframe must be called like the @data slot in the village shapefile
#' @param  look_ward (default = FALSE) logical, "TRUE" or "FALSE". If a misspelled village is found and look_ward = TRUE, the function will propose corrections by looking for similar names within both the legal village and ward names. If higher_level = "Ward", look_ward must be set to FALSE  
#' @param  ward_shapefile (optional) The ward shapefile, to use only when look_ward = TRUE . Default = NA. 
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
#' @return a new dataframe with:  
#'   
#' i) the village misspellings, stored in the first column ("misspelled_villages");
#'    
#' ii) a column specifying the level of the proposed correction ("correction_level");
#'    
#' iii) all the similar matches, each one stored under a separate column
#' ("Correct_Name_1", "Correct_Name_2", etc.);
#'
#' iv) a column storing the region/district/ward information in the project dataframe associated
#' to the misspelled village ("Region_in_df", "District_in_df" or "Ward_in_df"),
#' depending on the higher_level argument specified in the function call;    
#'      
#' v) a series of columns (one for each similar match found) specifying the
#' region/district/ward in the shapefile where each proposed correction
#' is located ("Name_1_in_Region"/"Name_1_in_District"/"Name_1_in_Ward", etc.);   
#'    
#' vi) a column specifying whether the misspelling is a real alphanumeric typo
#' or not ("real_typo);    
#'    
#' vii) all the location IDs, for each proposed correction ("Loc_ID_1", Loc_ID_2, etc.).
#'
#' @export
#'
#' @examples
#' #
#'

find_similar_village <- function(dataframe, label, higher_level, label_higher_level,
                                 shapefile, only_in_distr=NA, distnc, 
                                 additional_places=NA, look_ward=F,
                                 ward_shapefile=NA)
{
  
  if(!is.na(additional_places))
  {
    shp_df <- dplyr::select(shapefile@data, Vil_Mtaa_N, Ward_Name, District_N, Region_Nam)
    shp_df$Vil_Mtaa_N <- as.character(shp_df$Vil_Mtaa_N)
    shp_df$Ward_Name <- as.character(shp_df$Ward_Name)
    shp_df$District_N <- as.character(shp_df$District_N)
    shp_df$Region_Nam <- as.character(shp_df$Region_Nam)
    
    if(look_ward==T)
    {
      legal_names <- c(shp_df$Vil_Mtaa_N, additional_places$Vil_Mtaa_N)
      legal_names_ward <- c(shp_df$Ward_Name, additional_places$Ward_Name)
      fullset <- rbind(shp_df, additional_places) 
    } else {
      legal_names <- c(shp_df$Vil_Mtaa_N, additional_places$Vil_Mtaa_N)
      fullset <- rbind(shp_df, additional_places) 
    }
    
  } else {
    
    if(look_ward==T)
    {
      legal_names <- shapefile@data$Vil_Mtaa_N
      legal_names_ward <- shapefile@data$Ward_Name
      fullset <- dplyr::select(shapefile@data, Vil_Mtaa_N, Ward_Name, District_N, Region_Nam)
    } else {
      legal_names <- shapefile@data$Vil_Mtaa_N
      fullset <- dplyr::select(shapefile@data, Vil_Mtaa_N, Ward_Name, District_N, Region_Nam)
    }
  }
  
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)
  
  vll <- unique(dataframe[[label]])
  vll <- as.character(vll)
  
  ## number of villages
  vill_length <- length(vll)
  ## generate empty list for results
  mylist_similar_villages <- vector('list', vill_length)
  
  if(look_ward==T)
  {
    vll_correct <- legal_names
    vll_correct <- as.character(vll_correct)
    c_v <- gsub("[^[:alnum:]]", "", tolower(vll_correct))
    
    wrd_correct <- legal_names_ward
    wrd_correct <- as.character(wrd_correct)
    c_w <- gsub("[^[:alnum:]]", "", tolower(wrd_correct))
    
  } else { ## if look_ward==F
    vll_correct <- legal_names
    vll_correct <- as.character(vll_correct)
    c_v <- gsub("[^[:alnum:]]", "", tolower(vll_correct))
  }
  
  ## initiate vector with the wrong names in the csv file
  misspelled_villages <- c()
  
  ## loop through the unique villages
  for(i in 1:vill_length)
  {
    ## if in the dataframe there is a correct village, do nothing
    if((vll[i] %in% vll_correct)==T)
    {
      misspelled_villages[i] <- NA

    } else { # otherwise, take note of the misspelling
      
      no_matches <- as.character("Looking for similar results because there are no matches for the village: ")
      missp_vill <- as.character(vll[i])
      
      message(no_matches, missp_vill)
      
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
    look_ward=F
    ward_shapefile=NA
    
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
    mylist_respective_wards_v <- vector('list', Tot_misspell)
    
    ## initialise the real_typo vector
    real_typo <- c(rep(NA, Tot_misspell))
    real_typo <- as.character(real_typo)
    
    ## only consider alphanumeric characters of the misspelled/correct village names.
    ## same names' indices
    m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_vill))
    
    for(q in 1:Tot_misspell)
    {
      if((is.na(check_df$respective_ward[q]))==F) ## if there is ward information
      {
        study_ward <- check_df$respective_ward[q]
        study_ward <- as.character(study_ward)
        
        ##find the similar village names within the subset
        correct_subset <- dplyr::filter(fullset, Ward_Name==study_ward)
        correct_subset_vill <- correct_subset$Vil_Mtaa_N
        correct_subset_vill <- as.character(correct_subset_vill)
        correct_subset_vill <- unique(correct_subset_vill)
        correct_subset_ward <- correct_subset$Ward_Name
        correct_subset_ward <- as.character(correct_subset_ward)
        
        c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
        
        ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
        if((m_v[q] %in% c_sub_v)==T)
        {
          real_typo[q] <- FALSE
          number <- which(c_sub_v %in% m_v[q])
          mylist_similar_villages[[q]] <- number
          mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
          mylist_respective_wards_v[[q]] <- number
          mylist_respective_wards_v[[q]] <- correct_subset_ward[mylist_respective_wards_v[[q]]]
          
          unique_vill <- !duplicated(mylist_similar_villages[[q]])
          unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
          
          mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
          mylist_respective_wards_v[[q]] <- mylist_respective_wards_v[[q]][unique_ind]
          
        } else {
          real_typo[q] <- TRUE
          
          ##find the similar village names within the subset
          number <- agrep(m_v[q], c_sub_v, max.distance = distnc)
          
          mylist_similar_villages[[q]] <- number
          mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
          
          mylist_respective_wards_v[[q]] <- number
          mylist_respective_wards_v[[q]] <- correct_subset_ward[mylist_respective_wards_v[[q]]]
          
          unique_vill <- !duplicated(mylist_similar_villages[[q]])
          unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
          mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
          mylist_respective_wards_v[[q]] <- mylist_respective_wards_v[[q]][unique_ind]
        }
        
      } else {
        ## if there is no ward information
        ##find the similar village names within the fullset
        correct_subset <- fullset
        correct_subset_vill <- correct_subset$Vil_Mtaa_N
        correct_subset_vill <- as.character(correct_subset_vill)
        c_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
        
        correct_subset_ward <- correct_subset$Ward_Name
        correct_subset_ward <- as.character(correct_subset_ward)
        
        ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
        if((m_v[q] %in% c_v)==T)
        {
          real_typo[q] <- FALSE
          number <- which(c_v %in% m_v[q])
          mylist_similar_villages[[q]] <- number
          mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
          mylist_respective_wards_v[[q]] <- number
          mylist_respective_wards_v[[q]] <- correct_subset_ward[mylist_respective_wards_v[[q]]]
          
          unique_vill <- !duplicated(mylist_similar_villages[[q]])
          unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
          
          mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
          mylist_respective_wards_v[[q]] <- mylist_respective_wards_v[[q]][unique_ind]
        
        } else {
          # if there are no case insensitive matches
          real_typo[q] <- TRUE
          
          ##find the similar village names within the subset
          number <- agrep(m_v[q], c_v, max.distance = distnc)
          
          mylist_similar_villages[[q]] <- number
          mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
          
          mylist_respective_wards_v[[q]] <- number
          mylist_respective_wards_v[[q]] <- correct_subset_ward[mylist_respective_wards_v[[q]]]
          
          unique_vill <- !duplicated(mylist_similar_villages[[q]])
          unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
          mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
          mylist_respective_wards_v[[q]] <- mylist_respective_wards_v[[q]][unique_ind]
        }
      }
    }
    
    villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
    villages_correct_name <- villages_identified
    villages_correct_name <- as.data.frame(villages_correct_name)
    totcol_villcorrname <- length(villages_correct_name)
    
    correction_level <- rep("Village", nrow(villages_correct_name))
    
    for(l in 1:totcol_villcorrname)
    {
      newname <- paste("Correct_Name_", l, sep="")
      colnames(villages_correct_name)[l] <- newname
    }
    
    misspelled_villages <- check_df$respective_vill
    misspelled_villages <- as.character(misspelled_villages)
    
    corrections <- cbind(misspelled_villages, correction_level, villages_correct_name)
    
    ## cbind the Ward
    Ward_in_df <- check_df$respective_ward
    corrections <- cbind(corrections, Ward_in_df)
    
    wards_identified <- plyr::ldply(mylist_respective_wards_v, rbind)
    wards_correct_name <- wards_identified
    totcol_wardcorrname <- length(wards_correct_name)
    
    for(Q in 1:totcol_wardcorrname)
    {
      newname <- paste("Name_", Q, "_in_Ward", sep="")
      colnames(wards_correct_name)[Q] <- newname
    }
    
    corrections <- cbind(corrections, wards_correct_name)
    corrections <- as.data.frame(corrections)
    
    ## cbind the real_typo column
    real_typo <- as.character(real_typo)
    corrections <- cbind(corrections, real_typo)
    
    
  } else { 
    
    if(higher_level=="District") ## higher_level = "District"
    { 
      if(look_ward==F)
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
              # it is not a real typo
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
           
               } else { # if there are no case insensitive alphanumeric matches
              # it is a real typo
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
            newname <- paste("Correct_Name_", l, sep="")
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
            newname <- paste("Name_", Q, "_in_District", sep="")
            colnames(districts_correct_name)[Q] <- newname
          }
          
          corrections <- cbind(corrections, districts_correct_name)
          
          ## cbind the real_typo column
          real_typo <- as.character(real_typo)
          corrections <- cbind(corrections, real_typo)
          
        } else { ## if the argument only_in_distr is NA
          
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
          mylist_respective_districts_v <- vector('list', Tot_misspell)
          
          ## initialise the real_typo vector
          real_typo <- c(rep(NA, Tot_misspell))
          real_typo <- as.character(real_typo)
          
          ## only consider alphanumeric characters of the misspelled/correct village names.
          ## same names' indices
          m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_vill))
          
          for(q in 1:Tot_misspell)
          {
            if((is.na(check_df$respective_district[q]))==F) ## if there is district information
            {
              study_district <- check_df$respective_district[q]
              study_district <- as.character(study_district)
              
              ##find the similar village names within the subset
              correct_subset <- dplyr::filter(fullset, District_N==study_district)
              correct_subset_vill <- correct_subset$Vil_Mtaa_N
              correct_subset_vill <- as.character(correct_subset_vill)
              correct_subset_vill <- unique(correct_subset_vill)
              correct_subset_dis <- correct_subset$District_N
              correct_subset_dis <- as.character(correct_subset_dis)
            
              c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
              
              ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
              if((m_v[q] %in% c_sub_v)==T)
              {
                real_typo[q] <- FALSE
                number <- which(c_sub_v %in% m_v[q])
                mylist_similar_villages[[q]] <- number
                mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
                mylist_respective_districts_v[[q]] <- number
                mylist_respective_districts_v[[q]] <- correct_subset_dis[mylist_respective_districts_v[[q]]]
                
                unique_vill <- !duplicated(mylist_similar_villages[[q]])
                unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
                
                mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
                mylist_respective_districts_v[[q]] <- mylist_respective_districts_v[[q]][unique_ind]
                
              } else {
                real_typo[q] <- TRUE
                
                ##find the similar village names within the subset
                number <- agrep(m_v[q], c_sub_v, max.distance = distnc)
                
                mylist_similar_villages[[q]] <- number
                mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
                
                mylist_respective_districts_v[[q]] <- number
                mylist_respective_districts_v[[q]] <- correct_subset_dis[mylist_respective_districts_v[[q]]]
                
                unique_vill <- !duplicated(mylist_similar_villages[[q]])
                unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
                mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
                mylist_respective_districts_v[[q]] <- mylist_respective_districts_v[[q]][unique_ind]
              }
              
            } else {
              ## if there is no district information
              ##find the similar village names within the subset
              correct_subset <- fullset
              correct_subset_vill <- correct_subset$Vil_Mtaa_N
              correct_subset_vill <- as.character(correct_subset_vill)
              c_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
              
              correct_subset_dis <- correct_subset$District_N
              correct_subset_dis <- as.character(correct_subset_dis)
              
              ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
              if((m_v[q] %in% c_v)==T)
              {
                real_typo[q] <- FALSE
                number <- which(c_v %in% m_v[q])
                mylist_similar_villages[[q]] <- number
                mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
                mylist_respective_districts_v[[q]] <- number
                mylist_respective_districts_v[[q]] <- correct_subset_dis[mylist_respective_districts_v[[q]]]
                
                unique_vill <- !duplicated(mylist_similar_villages[[q]])
                unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
                
                mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
                mylist_respective_districts_v[[q]] <- mylist_respective_districts_v[[q]][unique_ind]
                
              } else {
                real_typo[q] <- TRUE
                
                ##find the similar village names within the subset
                number <- agrep(m_v[q], c_v, max.distance = distnc)
                
                mylist_similar_villages[[q]] <- number
                mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
                
                mylist_respective_districts_v[[q]] <- number
                mylist_respective_districts_v[[q]] <- correct_subset_dis[mylist_respective_districts_v[[q]]]
                
                unique_vill <- !duplicated(mylist_similar_villages[[q]])
                unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
                mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
                mylist_respective_districts_v[[q]] <- mylist_respective_districts_v[[q]][unique_ind]
              }
            }
          }
          
          villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
          villages_correct_name <- villages_identified
          villages_correct_name <- as.data.frame(villages_correct_name)
          totcol_villcorrname <- length(villages_correct_name)
          
          correction_level <- rep("Village", nrow(villages_correct_name))
          
          for(l in 1:totcol_villcorrname)
          {
            newname <- paste("Correct_Name_", l, sep="")
            colnames(villages_correct_name)[l] <- newname
          }
          
          misspelled_villages <- check_df$respective_vill
          misspelled_villages <- as.character(misspelled_villages)
          
          corrections <- cbind(misspelled_villages, correction_level, villages_correct_name)
          
          ## cbind the Region
          District_in_df <- check_df$respective_district
          corrections <- cbind(corrections, District_in_df)
          
          districts_identified <- plyr::ldply(mylist_respective_districts_v, rbind)
          districts_correct_name <- districts_identified
          totcol_discorrname <- length(districts_correct_name)
          
          for(Q in 1:totcol_discorrname)
          {
            newname <- paste("Name_", Q, "_in_District", sep="")
            colnames(districts_correct_name)[Q] <- newname
          }
          
          corrections <- cbind(corrections, districts_correct_name)
          corrections <- as.data.frame(corrections)
          
          ## cbind the real_typo column
          real_typo <- as.character(real_typo)
          corrections <- cbind(corrections, real_typo)
          
        }
        
      } else { ## if look_ward==T
        
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
          mylist_similar_wards <- vector('list', Tot_misspell)
          mylist_respective_districts_v <- vector('list', Tot_misspell)
          mylist_respective_districts_w <- vector('list', Tot_misspell)
          
          ## initialise the real_typo vector
          real_typo <- c(rep(NA, Tot_misspell))
          real_typo <- as.character(real_typo)
          
          ## initialise the correction level vector
          correction_level_v <- c(rep(NA, Tot_misspell))
          correction_level_v <- as.character(correction_level_v)
          
          correction_level_w <- c(rep("Ward", Tot_misspell))
          correction_level_w <- as.character(correction_level_w)
          
          ##find the similar village names within the subset
          correct_subset <- dplyr::filter(fullset, District_N%in%only_in_distr)
          
          correct_subset_vill <- correct_subset$Vil_Mtaa_N
          correct_subset_vill <- as.character(correct_subset_vill)
          
          correct_subset_ward <- correct_subset$Ward_Name
          correct_subset_ward <- as.character(correct_subset_ward)
          
          correct_subset_dis <- correct_subset$District_N
          correct_subset_dis <- as.character(correct_subset_dis)
          
          ## only consider alphanumeric characters of the misspelled/correct village and ward names.
          m_v <- gsub("[^[:alnum:]]", "", tolower(respective_vill))
          c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
          c_sub_w <- gsub("[^[:alnum:]]", "", tolower(correct_subset_ward))
          
          for(m in 1:Tot_misspell)
          {
            ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
            if((m_v[m] %in% c_sub_v)==T)
            {
              # it is not a real typo
              real_typo[m] <- FALSE
              correction_level_v[m] <- "Village"
              correction_level_w[m] <- "Ward"
              number <- which(c_sub_v %in% m_v[m])
              mylist_similar_villages[[m]] <- number
              mylist_similar_villages[[m]] <- correct_subset_vill[mylist_similar_villages[[m]]]
              mylist_respective_districts_v[[m]] <- number
              mylist_respective_districts_v[[m]] <- correct_subset_dis[mylist_respective_districts_v[[m]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[m]])
              unique_ind <- seq_along(mylist_similar_villages[[m]])[unique_vill]
              
              mylist_similar_villages[[m]] <- mylist_similar_villages[[m]][unique_ind]
              mylist_respective_districts_v[[m]] <- mylist_respective_districts_v[[m]][unique_ind]
              
              mylist_similar_wards[[m]] <- NA
              mylist_respective_districts_w[[m]] <- NA
              
            } else { # if there are no case insensitive alphanumeric matches
              
              # it is a real typo
              real_typo[m] <- TRUE
              correction_level_v[m] <- "Village"
              
              ##find the similar village names within the subset
              number_v <- agrep(m_v[m], c_sub_v, max.distance = distnc)
              
              mylist_similar_villages[[m]] <- number_v
              mylist_similar_villages[[m]] <- correct_subset_vill[mylist_similar_villages[[m]]]
              
              mylist_respective_districts_v[[m]] <- number_v
              mylist_respective_districts_v[[m]] <- correct_subset_dis[mylist_respective_districts_v[[m]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[m]])
              unique_ind_v <- seq_along(mylist_similar_villages[[m]])[unique_vill]
              mylist_similar_villages[[m]] <- mylist_similar_villages[[m]][unique_ind_v]
              mylist_respective_districts_v[[m]] <- mylist_respective_districts_v[[m]][unique_ind_v]
              
              number_w <- agrep(m_v[m], c_sub_w, max.distance = distnc)
              correction_level_w[m] <- "Ward"
              
              if(length(number_w)==0)
              {
                mylist_similar_wards[[m]] <- NA
                mylist_respective_districts_w[[m]] <- NA
                
              } else {
                mylist_similar_wards[[m]] <- number_w
                mylist_similar_wards[[m]] <- correct_subset_ward[mylist_similar_wards[[m]]]
                
                mylist_respective_districts_w[[m]] <- number_w
                mylist_respective_districts_w[[m]] <- correct_subset_dis[mylist_respective_districts_w[[m]]]
                
                
                unique_ward <- !duplicated(mylist_similar_wards[[m]])
                unique_ind_w <- seq_along(mylist_similar_wards[[m]])[unique_ward]
                mylist_similar_wards[[m]] <- mylist_similar_wards[[m]][unique_ind_w]
                
                mylist_respective_districts_w[[m]] <- mylist_respective_districts_w[[m]][unique_ind_w]
              
              }
            }
          }
          
          #############################          
          villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
          villages_correct_name <- villages_identified
          villages_correct_name <- as.data.frame(villages_correct_name)
          totcol_villcorrname <- length(villages_correct_name)
          
          for(l in 1:totcol_villcorrname)
          {
            newname <- paste("Correct_Name_", l, sep="")
            colnames(villages_correct_name)[l] <- newname
          }
          
          misspelled_villages <- respective_vill
          misspelled_villages <- as.character(misspelled_villages)
          
          corrections_v <- cbind(misspelled_villages, correction_level_v, villages_correct_name)
          corrections_v <- as.data.frame(corrections_v)
          
          districts_identified_v <- plyr::ldply(mylist_respective_districts_v, rbind)
          districts_correct_name_v <- districts_identified_v
          totcol_discorrname_v <- length(districts_correct_name_v)
          
          for(Q in 1:totcol_discorrname_v)
          {
            newname <- paste("Name_", Q, "_in_District", sep="")
            colnames(districts_correct_name_v)[Q] <- newname
          }
          
          corrections_v <- cbind(corrections_v, districts_correct_name_v)
          
          ## cbind the real_typo column
          real_typo <- as.character(real_typo)
          corrections_v <- cbind(corrections_v, real_typo)
          
          #############################          
          wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
          wards_correct_name <- wards_identified
          wards_correct_name <- as.data.frame(wards_correct_name)
          totcol_wardscorrname <- length(wards_correct_name)
          
          real_typo_w <- rep("TRUE", nrow(wards_correct_name))
          
          for(L in 1:totcol_wardscorrname)
          {
            newname <- paste("Correct_Name_", L, sep="")
            colnames(wards_correct_name)[L] <- newname
          }
          
          
          corrections_w <- cbind(misspelled_villages, correction_level_w, wards_correct_name)
          corrections_w <- as.data.frame(corrections_w)
          
          districts_identified_w <- plyr::ldply(mylist_respective_districts_w, rbind)
          districts_correct_name_w <- districts_identified_w
          totcol_discorrname_w <- length(districts_correct_name_w)
          
          for(A in 1:totcol_discorrname_w)
          {
            newname <- paste("Name_", A, "_in_District", sep="")
            colnames(districts_correct_name_w)[A] <- newname
          }
          
          corrections_w <- cbind(corrections_w, districts_correct_name_w, real_typo_w)
          
          colnames(corrections_v)[which(names(corrections_v) == "correction_level_v")] <- "correction_level"
          colnames(corrections_w)[which(names(corrections_w) == "correction_level_w")] <- "correction_level"
          colnames(corrections_w)[which(names(corrections_w) == "real_typo_w")] <- "real_typo"
          
          ## The final dataframe with the NAs in the ward names
          corrections <- plyr::rbind.fill(corrections_v, corrections_w)
         
          ## The final dataframe without the uninformative rows
          nocol <- c("correction_level", "real_typo")
          nonums <- which(names(corrections) %in% nocol)
          corrections <- corrections[rowSums(!is.na(corrections[,-nonums])) > 0, ]
          
          
          #############################          
          
        } else { ## if the argument only_in_distr is NA
          
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
          mylist_similar_wards <- vector('list', Tot_misspell)
          mylist_respective_districts_v <- vector('list', Tot_misspell)
          mylist_respective_districts_w <- vector('list', Tot_misspell)
          
          ## initialise the real_typo vector
          real_typo <- c(rep(NA, Tot_misspell))
          real_typo <- as.character(real_typo)
          
          ## initialise the correction level vector
          correction_level_v <- c(rep(NA, Tot_misspell))
          correction_level_v <- as.character(correction_level_v)
          correction_level_w <- c(rep(NA, Tot_misspell))
          correction_level_w <- as.character(correction_level_w)
          
          ## only consider alphanumeric characters of the misspelled/correct village names.
          ## same names' indices
          m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_vill))
          
          for(q in 1:Tot_misspell)
          {
            if((is.na(check_df$respective_district[q]))==F) ## if there is district information
            {
              study_district <- check_df$respective_district[q]
              study_district <- as.character(study_district)
              
              ##find the similar village names within the subset
              correct_subset <- dplyr::filter(fullset, District_N==study_district)
              correct_subset_vill <- correct_subset$Vil_Mtaa_N
              correct_subset_vill <- as.character(correct_subset_vill)
              correct_subset_vill <- unique(correct_subset_vill)
              
              correct_subset_ward <- correct_subset$Ward_Name
              correct_subset_ward <- as.character(correct_subset_ward)
              correct_subset_ward <- unique(correct_subset_ward)
              
              correct_subset_dis <- correct_subset$District_N
              correct_subset_dis <- as.character(correct_subset_dis)
              
              c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
              c_sub_w <- gsub("[^[:alnum:]]", "", tolower(correct_subset_ward))
              
              ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
              if((m_v[q] %in% c_sub_v)==T)
              {
                real_typo[q] <- FALSE
                correction_level_v[q] <- "Village"
                correction_level_w[q] <- "Ward"
                number <- which(c_sub_v %in% m_v[q])
                mylist_similar_villages[[q]] <- number
                mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
                mylist_respective_districts_v[[q]] <- number
                mylist_respective_districts_v[[q]] <- correct_subset_dis[mylist_respective_districts_v[[q]]]
                
                unique_vill <- !duplicated(mylist_similar_villages[[q]])
                unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
                
                mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
                mylist_respective_districts_v[[q]] <- mylist_respective_districts_v[[q]][unique_ind]
                
                mylist_similar_wards[[q]] <- NA
                mylist_respective_districts_w[[q]] <- NA
                
              } else {
                real_typo[q] <- TRUE
                correction_level_v[q] <- "Village"
                
                ##find the similar village names within the subset
                number_v <- agrep(m_v[q], c_sub_v, max.distance = distnc)
                
                mylist_similar_villages[[q]] <- number_v
                mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
                
                mylist_respective_districts_v[[q]] <- number_v
                mylist_respective_districts_v[[q]] <- correct_subset_dis[mylist_respective_districts_v[[q]]]
                
                unique_vill <- !duplicated(mylist_similar_villages[[q]])
                unique_ind_v <- seq_along(mylist_similar_villages[[q]])[unique_vill]
                mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind_v]
                mylist_respective_districts_v[[q]] <- mylist_respective_districts_v[[q]][unique_ind_v]
                
                
                ##find the similar ward names within the subset
                number_w <- agrep(m_v[q], c_sub_w, max.distance = distnc)
                correction_level_w[q] <- "Ward"
                
                if(length(number_w)==0)
                {
                  mylist_similar_wards[[q]] <- NA
                  mylist_respective_districts_w[[q]] <- NA
                  
                } else {
                  mylist_similar_wards[[q]] <- number_w
                  mylist_similar_wards[[q]] <- correct_subset_ward[mylist_similar_wards[[q]]]
                  mylist_respective_districts_w[[q]] <- number_w
                  mylist_respective_districts_w[[q]] <- correct_subset_dis[mylist_respective_districts_w[[q]]]
                  
                  unique_ward <- !duplicated(mylist_similar_wards[[q]])
                  unique_ind_w <- seq_along(mylist_similar_wards[[q]])[unique_ward]
                  mylist_similar_wards[[q]] <- mylist_similar_wards[[q]][unique_ind_w]
                  
                  mylist_respective_districts_w[[q]] <- mylist_respective_districts_w[[q]][unique_ind_w]
                }
              }
              
            } else { ## if there is no district information
              
              ##find the similar village names within the subset
              correct_subset <- fullset
              correct_subset_vill <- correct_subset$Vil_Mtaa_N
              correct_subset_vill <- as.character(correct_subset_vill)
              c_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
              
              correct_subset_ward <- correct_subset$Ward_Name
              correct_subset_ward <- as.character(correct_subset_ward)
              c_w <- gsub("[^[:alnum:]]", "", tolower(correct_subset_ward))
              
              correct_subset_dis <- correct_subset$District_N
              correct_subset_dis <- as.character(correct_subset_dis)
              
              ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
              if((m_v[q] %in% c_v)==T)
              {
                real_typo[q] <- FALSE
                correction_level_v[q] <- "Village"
                
                correction_level_w[q] <- "Ward"
                mylist_similar_wards[[q]] <- NA
                
                number <- which(c_v %in% m_v[q])
                
                mylist_similar_villages[[q]] <- number
                mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
                mylist_respective_districts_v[[q]] <- number
                mylist_respective_districts_v[[q]] <- correct_subset_dis[mylist_respective_districts_v[[q]]]
                
                unique_vill <- !duplicated(mylist_similar_villages[[q]])
                unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
                
                mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
                mylist_respective_districts_v[[q]] <- mylist_respective_districts_v[[q]][unique_ind]
                
                mylist_similar_wards[[q]] <- NA
                mylist_respective_districts_w[[q]] <- NA
                
              } else {
                real_typo[q] <- TRUE
                correction_level_v[q] <- "Village"
                
                ##find the similar village names within the subset
                number_v <- agrep(m_v[q], c_v, max.distance = distnc)
                
                mylist_similar_villages[[q]] <- number_v
                mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
                
                mylist_respective_districts_v[[q]] <- number_v
                mylist_respective_districts_v[[q]] <- correct_subset_dis[mylist_respective_districts_v[[q]]]
                
                unique_vill <- !duplicated(mylist_similar_villages[[q]])
                unique_ind_v <- seq_along(mylist_similar_villages[[q]])[unique_vill]
                mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind_v]
                mylist_respective_districts_v[[q]] <- mylist_respective_districts_v[[q]][unique_ind_v]
                
                
                ##find the similar ward names within the subset
                number_w <- agrep(m_v[q], c_w, max.distance = distnc)
                correction_level_w[q] <- "Ward"
                
                if(length(number_w)==0)
                {
                  mylist_similar_wards[[q]] <- NA
                  mylist_respective_districts_w[[q]] <- NA
                  
                } else {
                  mylist_similar_wards[[q]] <- number_w
                  mylist_similar_wards[[q]] <- correct_subset_ward[mylist_similar_wards[[q]]]
                  mylist_respective_districts_w[[q]] <- number_w
                  mylist_respective_districts_w[[q]] <- correct_subset_dis[mylist_respective_districts_w[[q]]]
                  
                  unique_ward <- !duplicated(mylist_similar_wards[[q]])
                  unique_ind_w <- seq_along(mylist_similar_wards[[q]])[unique_ward]
                  mylist_similar_wards[[q]] <- mylist_similar_wards[[q]][unique_ind_w]
                  
                  mylist_respective_districts_w[[q]] <- mylist_respective_districts_w[[q]][unique_ind_w]
                }
              }
            }
          }
          
          
          #############################          
          villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
          villages_correct_name <- villages_identified
          villages_correct_name <- as.data.frame(villages_correct_name)
          totcol_villcorrname <- length(villages_correct_name)
          
          for(l in 1:totcol_villcorrname)
          {
            newname <- paste("Correct_Name_", l, sep="")
            colnames(villages_correct_name)[l] <- newname
          }
          
          misspelled_villages <- check_df$respective_vill
          misspelled_villages <- as.character(misspelled_villages)
          
          corrections_v <- cbind(misspelled_villages, correction_level_v, villages_correct_name)
          
          ## cbind the District
          District_in_df <- check_df$respective_district
          corrections_v <- cbind(corrections_v, District_in_df)
          
          districts_identified_v <- plyr::ldply(mylist_respective_districts_v, rbind)
          districts_correct_name_v <- districts_identified_v
          totcol_discorrname_v <- length(districts_correct_name_v)
          
          for(Q in 1:totcol_discorrname_v)
          {
            newname <- paste("Name_", Q, "_in_District", sep="")
            colnames(districts_correct_name_v)[Q] <- newname
          }
          
          corrections_v <- cbind(corrections_v, districts_correct_name_v)
          corrections_v <- as.data.frame(corrections_v)
          
          ## cbind the real_typo column
          real_typo <- as.character(real_typo)
          corrections_v <- cbind(corrections_v, real_typo)
          
          #############################          
          wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
          wards_correct_name <- wards_identified
          wards_correct_name <- as.data.frame(wards_correct_name)
          totcol_wardscorrname <- length(wards_correct_name)
          
          real_typo_w <- rep("TRUE", nrow(wards_correct_name))
          
          for(L in 1:totcol_wardscorrname)
          {
            newname <- paste("Correct_Name_", L, sep="")
            colnames(wards_correct_name)[L] <- newname
          }
          
          corrections_w <- cbind(misspelled_villages, correction_level_w, wards_correct_name)
          corrections_w <- as.data.frame(corrections_w)
          
          districts_identified_w <- plyr::ldply(mylist_respective_districts_w, rbind)
          districts_correct_name_w <- districts_identified_w
          totcol_discorrname_w <- length(districts_correct_name_w)
          
          for(A in 1:totcol_discorrname_w)
          {
            newname <- paste("Name_", A, "_in_District", sep="")
            colnames(districts_correct_name_w)[A] <- newname
          }
          
          corrections_w <- cbind(corrections_w, districts_correct_name_w, real_typo_w)
          
          #District_in_df <- check_df$respective_district
          corrections_w <- cbind(corrections_w, District_in_df)
          
          colnames(corrections_v)[which(names(corrections_v) == "correction_level_v")] <- "correction_level"
          colnames(corrections_w)[which(names(corrections_w) == "correction_level_w")] <- "correction_level"
          colnames(corrections_w)[which(names(corrections_w) == "real_typo_w")] <- "real_typo"
          
          ## The final dataframe with the NAs in the ward names
          corrections <- plyr::rbind.fill(corrections_v, corrections_w)
          
          ## The final dataframe without the uninformative rows
          nocol <- c("correction_level", "real_typo", "District_in_df")
          nonums <- which(names(corrections) %in% nocol)
          corrections <- corrections[rowSums(!is.na(corrections[,-nonums])) > 0, ]
          
          #############################          
          
        }
      }

    } else { ## higher_level = Region
      
      if(look_ward==F)
      {
        rg <- unique(dataframe[[label_higher_level]])
        rg <- as.character(rg)
        
        ## number of reg
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
        mylist_respective_regions_v <- vector('list', Tot_misspell)
        
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
            
            correct_subset_reg <- correct_subset$Region_Nam
            correct_subset_reg <- as.character(correct_subset_reg)
            
            c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))

            ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
            if((m_v[q] %in% c_sub_v)==T)
            {
              real_typo[q] <- FALSE
              number <- which(c_sub_v %in% m_v[q])
              mylist_similar_villages[[q]] <- number
              mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
              mylist_respective_regions_v[[q]] <- number
              mylist_respective_regions_v[[q]] <- correct_subset_reg[mylist_respective_regions_v[[q]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[q]])
              unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
              
              mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
              mylist_respective_regions_v[[q]] <- mylist_respective_regions_v[[q]][unique_ind]
              
            } else {
              real_typo[q] <- TRUE
              
              ##find the similar village names within the subset
              number <- agrep(m_v[q], c_sub_v, max.distance = distnc)
              
              mylist_similar_villages[[q]] <- number
              mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
              
              mylist_respective_regions_v[[q]] <- number
              mylist_respective_regions_v[[q]] <- correct_subset_reg[mylist_respective_regions_v[[q]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[q]])
              unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
              mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
              mylist_respective_regions_v[[q]] <- mylist_respective_regions_v[[q]][unique_ind]
            }
            
          } else {
            ## if there is no region information
            ##find the similar village names within the subset
            correct_subset <- fullset
            correct_subset_vill <- correct_subset$Vil_Mtaa_N
            correct_subset_vill <- as.character(correct_subset_vill)
            c_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
            
            correct_subset_reg <- correct_subset$Region_Nam
            correct_subset_reg <- as.character(correct_subset_reg)
            
            
            ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
            if((m_v[q] %in% c_v)==T)
            {
              real_typo[q] <- FALSE
              number <- which(c_v %in% m_v[q])
              mylist_similar_villages[[q]] <- number
              mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
              mylist_respective_regions_v[[q]] <- number
              mylist_respective_regions_v[[q]] <- correct_subset_reg[mylist_respective_regions_v[[q]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[q]])
              unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
              
              mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
              mylist_respective_regions_v[[q]] <- mylist_respective_regions_v[[q]][unique_ind]
            
            } else {
              real_typo[q] <- TRUE
              
              ##find the similar village names within the subset
              number <- agrep(m_v[q], c_v, max.distance = distnc)
              
              mylist_similar_villages[[q]] <- number
              mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
              
              mylist_respective_regions_v[[q]] <- number
              mylist_respective_regions_v[[q]] <- correct_subset_reg[mylist_respective_regions_v[[q]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[q]])
              unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
              mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
              mylist_respective_regions_v[[q]] <- mylist_respective_regions_v[[q]][unique_ind]
              
            }
          }
        }
        
        
        villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
        villages_correct_name <- villages_identified
        villages_correct_name <- as.data.frame(villages_correct_name)
        totcol_villcorrname <- length(villages_correct_name)
        
        correction_level <- rep("Village", nrow(villages_correct_name))
        
        for(l in 1:totcol_villcorrname)
        {
          newname <- paste("Correct_Name_", l, sep="")
          colnames(villages_correct_name)[l] <- newname
        }
        
        misspelled_villages <- check_df$respective_vill
        misspelled_villages <- as.character(misspelled_villages)
        
        corrections <- cbind(misspelled_villages, correction_level, villages_correct_name)
        
        ## cbind the Region
        Region_in_df <- check_df$respective_region
        corrections <- cbind(corrections, Region_in_df)
        
        regions_identified <- plyr::ldply(mylist_respective_regions_v, rbind)
        regions_correct_name <- regions_identified
        totcol_regcorrname <- length(regions_correct_name)
        
        for(Q in 1:totcol_regcorrname)
        {
          newname <- paste("Name_", Q, "_in_Region", sep="")
          colnames(regions_correct_name)[Q] <- newname
        }
        
        corrections <- cbind(corrections, regions_correct_name)
        corrections <- as.data.frame(corrections)
        
        ## cbind the real_typo column
        real_typo <- as.character(real_typo)
        corrections <- cbind(corrections, real_typo)
        
      } else { ## if look ward==T
        
        rg <- unique(dataframe[[label_higher_level]])
        rg <- as.character(rg)
        
        ## number of reg
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
        mylist_similar_wards <- vector('list', Tot_misspell)
        mylist_respective_regions_v <- vector('list', Tot_misspell)
        mylist_respective_regions_w <- vector('list', Tot_misspell)
        
        ## initialise the real_typo vector
        real_typo <- c(rep(NA, Tot_misspell))
        real_typo <- as.character(real_typo)
        
        ## initialise the correction level vector
        correction_level_v <- c(rep(NA, Tot_misspell))
        correction_level_v <- as.character(correction_level_v)
        correction_level_w <- c(rep(NA, Tot_misspell))
        correction_level_w <- as.character(correction_level_w)
        
        ## only consider alphanumeric characters of the misspelled/correct village names.
        ## same names' indices
        m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_vill))
        
        for(q in 1:Tot_misspell)
        {
          if((is.na(check_df$respective_region[q]))==F) ## if there is region information
          {
            study_region <- check_df$respective_region[q]
            study_region <- as.character(study_region)
            
            ##find the similar village names within the subset
            correct_subset <- dplyr::filter(fullset, Region_Nam==study_region)
            correct_subset_vill <- correct_subset$Vil_Mtaa_N
            correct_subset_vill <- as.character(correct_subset_vill)
            correct_subset_vill <- unique(correct_subset_vill)
            
            correct_subset_ward <- correct_subset$Ward_Name
            correct_subset_ward <- as.character(correct_subset_ward)
            correct_subset_ward <- unique(correct_subset_ward)
            
            correct_subset_reg <- correct_subset$Region_Nam
            correct_subset_reg <- as.character(correct_subset_dis)
            
            c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
            c_sub_w <- gsub("[^[:alnum:]]", "", tolower(correct_subset_ward))
            
            ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
            if((m_v[q] %in% c_sub_v)==T)
            {
              real_typo[q] <- FALSE
              correction_level_v[q] <- "Village"
              
              correction_level_w[q] <- "Ward"
              mylist_similar_wards[[q]] <- NA
              
              number <- which(c_sub_v %in% m_v[q])
              mylist_similar_villages[[q]] <- number
              mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
              mylist_respective_regions_v[[q]] <- number
              mylist_respective_regions_v[[q]] <- correct_subset_reg[mylist_respective_regions_v[[q]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[q]])
              unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
              
              mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
              mylist_respective_regions_v[[q]] <- mylist_respective_regions_v[[q]][unique_ind]
              
              mylist_similar_wards[[q]] <- NA
              mylist_respective_regions_w[[q]] <- NA
              
            } else {
              real_typo[q] <- TRUE
              correction_level_v[q] <- "Village"
              
              ##find the similar village names within the subset
              number_v <- agrep(m_v[q], c_sub_v, max.distance = distnc)
              
              mylist_similar_villages[[q]] <- number_v
              mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
              
              mylist_respective_regions_v[[q]] <- number_v
              mylist_respective_regions_v[[q]] <- correct_subset_reg[mylist_respective_regions_v[[q]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[q]])
              unique_ind_v <- seq_along(mylist_similar_villages[[q]])[unique_vill]
              mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind_v]
              mylist_respective_regions_v[[q]] <- mylist_respective_regions_v[[q]][unique_ind_v]
              
              correction_level_w[q] <- "Ward"
              ##find the similar ward names within the subset
              number_w <- agrep(m_v[q], c_sub_w, max.distance = distnc)
              if(length(number_w)==0)
              {
                mylist_similar_wards[[q]] <- NA
                mylist_respective_regions_w[[q]] <- NA
                
              } else {
                mylist_similar_wards[[q]] <- number_w
                mylist_similar_wards[[q]] <- correct_subset_ward[mylist_similar_wards[[q]]]
                mylist_respective_regions_w[[q]] <- number_w
                mylist_respective_regions_w[[q]] <- correct_subset_reg[mylist_respective_regions_w[[q]]]
                
                unique_ward <- !duplicated(mylist_similar_wards[[q]])
                unique_ind_w <- seq_along(mylist_similar_wards[[q]])[unique_ward]
                mylist_similar_wards[[q]] <- mylist_similar_wards[[q]][unique_ind_w]
                
                mylist_respective_regions_w[[q]] <- mylist_respective_regions_w[[q]][unique_ind_w]
              }
            }
            
          } else { ## if there is no region information
            
            ##find the similar village names within the subset
            correct_subset <- fullset
            correct_subset_vill <- correct_subset$Vil_Mtaa_N
            correct_subset_vill <- as.character(correct_subset_vill)
            c_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_vill))
            
            correct_subset_ward <- correct_subset$Ward_Name
            correct_subset_ward <- as.character(correct_subset_ward)
            c_w <- gsub("[^[:alnum:]]", "", tolower(correct_subset_ward))
            
            correct_subset_reg <- correct_subset$Region_Nam
            correct_subset_reg <- as.character(correct_subset_reg)
            
            ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
            if((m_v[q] %in% c_v)==T)
            {
              real_typo[q] <- FALSE
              correction_level_v[q] <- "Village"
              correction_level_w[q] <- "Ward"
              number <- which(c_v %in% m_v[q])
              mylist_similar_villages[[q]] <- number
              mylist_similar_villages[[q]] <- unique(correct_subset_vill[mylist_similar_villages[[q]]])
              mylist_respective_regions_v[[q]] <- number
              mylist_respective_regions_v[[q]] <- correct_subset_reg[mylist_respective_regions_v[[q]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[q]])
              unique_ind <- seq_along(mylist_similar_villages[[q]])[unique_vill]
              
              mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind]
              mylist_respective_regions_v[[q]] <- mylist_respective_regions_v[[q]][unique_ind]
              
              mylist_similar_wards[[q]] <- NA
              mylist_respective_regions_w[[q]] <- NA
              
            } else {
              
              real_typo[q] <- TRUE
              correction_level_v[q] <- "Village"
              ##find the similar village names within the subset
              number_v <- agrep(m_v[q], c_v, max.distance = distnc)
              
              mylist_similar_villages[[q]] <- number_v
              mylist_similar_villages[[q]] <- correct_subset_vill[mylist_similar_villages[[q]]]
              
              mylist_respective_regions_v[[q]] <- number_v
              mylist_respective_regions_v[[q]] <- correct_subset_reg[mylist_respective_regions_v[[q]]]
              
              unique_vill <- !duplicated(mylist_similar_villages[[q]])
              unique_ind_v <- seq_along(mylist_similar_villages[[q]])[unique_vill]
              mylist_similar_villages[[q]] <- mylist_similar_villages[[q]][unique_ind_v]
              mylist_respective_regions_v[[q]] <- mylist_respective_regions_v[[q]][unique_ind_v]
              
              correction_level_w[q] <- "Ward"
              ##find the similar ward names within the subset
              number_w <- agrep(m_v[q], c_w, max.distance = distnc)
              if(length(number_w)==0)
              {
                mylist_similar_wards[[q]] <- NA
                mylist_respective_regions_w[[q]] <- NA
                
              } else {
                mylist_similar_wards[[q]] <- number_w
                mylist_similar_wards[[q]] <- correct_subset_ward[mylist_similar_wards[[q]]]
                mylist_respective_regions_w[[q]] <- number_w
                mylist_respective_regions_w[[q]] <- correct_subset_reg[mylist_respective_regions_w[[q]]]
                
                unique_ward <- !duplicated(mylist_similar_wards[[q]])
                unique_ind_w <- seq_along(mylist_similar_wards[[q]])[unique_ward]
                mylist_similar_wards[[q]] <- mylist_similar_wards[[q]][unique_ind_w]
                
                mylist_respective_regions_w[[q]] <- mylist_respective_regions_w[[q]][unique_ind_w]
              }
            }
          }
        }
        
        #############################          
        villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
        villages_correct_name <- villages_identified
        villages_correct_name <- as.data.frame(villages_correct_name)
        totcol_villcorrname <- length(villages_correct_name)
        
        for(l in 1:totcol_villcorrname)
        {
          newname <- paste("Correct_Name_", l, sep="")
          colnames(villages_correct_name)[l] <- newname
        }
        
        misspelled_villages <- check_df$respective_vill
        misspelled_villages <- as.character(misspelled_villages)
        
        corrections_v <- cbind(misspelled_villages, correction_level_v, villages_correct_name)
        
        ## cbind the Region
        Region_in_df <- check_df$respective_region
        corrections_v <- cbind(corrections_v, Region_in_df)
        
        
        regions_identified_v <- plyr::ldply(mylist_respective_regions_v, rbind)
        regions_correct_name_v <- regions_identified_v
        totcol_regcorrname_v <- length(regions_correct_name_v)
        
        for(Q in 1:totcol_regcorrname_v)
        {
          newname <- paste("Name_", Q, "_in_Region", sep="")
          colnames(regions_correct_name_v)[Q] <- newname
        }
        
        corrections_v <- cbind(corrections_v, regions_correct_name_v)
        corrections_v <- as.data.frame(corrections_v)
        
        ## cbind the real_typo column
        real_typo <- as.character(real_typo)
        corrections_v <- cbind(corrections_v, real_typo)
        
        #############################          
        wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
        wards_correct_name <- wards_identified
        wards_correct_name <- as.data.frame(wards_correct_name)
        totcol_wardscorrname <- length(wards_correct_name)
        
        real_typo_w <- rep("TRUE", nrow(wards_correct_name))
        
        for(L in 1:totcol_wardscorrname)
        {
          newname <- paste("Correct_Name_", L, sep="")
          colnames(wards_correct_name)[L] <- newname
        }
        
        corrections_w <- cbind(misspelled_villages, correction_level_w, wards_correct_name)
        corrections_w <- as.data.frame(corrections_w)
        
        regions_identified_w <- plyr::ldply(mylist_respective_regions_w, rbind)
        regions_correct_name_w <- regions_identified_w
        totcol_regcorrname_w <- length(regions_correct_name_w)
        
        for(A in 1:totcol_regcorrname_w)
        {
          newname <- paste("Name_", A, "_in_Region", sep="")
          colnames(regions_correct_name_w)[A] <- newname
        }
        
        corrections_w <- cbind(corrections_w, regions_correct_name_w, real_typo_w)
        
        corrections_w <- cbind(corrections_w, Region_in_df)
        
        colnames(corrections_v)[which(names(corrections_v) == "correction_level_v")] <- "correction_level"
        colnames(corrections_w)[which(names(corrections_w) == "correction_level_w")] <- "correction_level"
        colnames(corrections_w)[which(names(corrections_w) == "real_typo_w")] <- "real_typo"
        
        ## The final dataframe with the NAs in the ward names
        corrections <- plyr::rbind.fill(corrections_v, corrections_w)
        
        ## The final dataframe without the uninformative rows
        nocol <- c("correction_level", "real_typo", "District_in_df")
        nonums <- which(names(corrections) %in% nocol)
        corrections <- corrections[rowSums(!is.na(corrections[,-nonums])) > 0, ]
        
      }
    }
  }
  
  if(look_ward==F)
  {
    corrections_II <- as.data.frame(villages_correct_name)
    totcol <- length(corrections_II)
    ## initialise the list to store the code
    mylist_resp_code <- vector('list', totcol)
    
    for(w in 1:totcol)
    {
      corr_vill <- corrections_II[,w]
      corr_vill <- as.character(corr_vill)
      
      Loc_ID <- rep(NA, length(corr_vill))
      Loc_ID <- as.character(Loc_ID)
      
      for(b in 1:length(corr_vill))
      {
        sub_set <- dplyr::filter(shapefile@data, Vil_Mtaa_N == corr_vill[b])
        
        if(nrow(sub_set)>0)
        {
          Loc_ID[b] <- as.character(sub_set$Loc_ID)
          
        } else {
          Loc_ID[b] <- as.character(NA)
        }
      }
      
      mylist_resp_code[[w]] <- Loc_ID
      
    }
    
    code_identified <- as.data.frame(mylist_resp_code)
    
    ## rename the columns 
    totcol <- length(code_identified)
    for(O in 1:totcol)
    {
      newname <- paste("Loc_ID_", O, sep="")
      colnames(code_identified)[O] <- newname
    }
    
    corrections_final <- cbind(corrections, code_identified)
    corrections_final <- as.data.frame(corrections_final)
    
  } else {
    
    correct_villages_proc_df <- dplyr::filter(corrections, correction_level=="Village")
    correct_wards_proc_df <- dplyr::filter(corrections, correction_level=="Ward")
    
    corr_villages <- as.data.frame(correct_villages_proc_df[, grepl("Correct_Name", names(correct_villages_proc_df))])
    corr_wards <- as.data.frame(correct_wards_proc_df[, grepl("Correct_Name", names(correct_wards_proc_df))])
    
    totcol_v <- length(corr_villages)
    ## initialise the list to store the code
    mylist_resp_code_v <- vector('list', totcol_v)
    
    for(w in 1:totcol_v)
    {
      corr_vill <- corr_villages[,w]
      corr_vill <- as.character(corr_vill)
      
      Loc_ID <- rep(NA, length(corr_vill))
      Loc_ID <- as.character(Loc_ID)
      
      for(b in 1:length(corr_vill))
      {
        sub_set <- dplyr::filter(shapefile@data, Vil_Mtaa_N == corr_vill[b])
        
        if(nrow(sub_set)>0)
        {
          Loc_ID[b] <- as.character(sub_set$Loc_ID)
          
        } else {
          Loc_ID[b] <- as.character(NA)
        }
      }
      
      mylist_resp_code_v[[w]] <- Loc_ID
    }
     
    totcol_w <- length(corr_wards)
    
    ## initialise the list to store the code
    mylist_resp_code_w <- vector('list', totcol_w)
    
    for(W in 1:totcol_w)
    {
      corr_wrd <- corr_wards[,W]
      corr_wrd <- as.character(corr_wrd)
      
      Loc_ID <- rep(NA, length(corr_wrd))
      Loc_ID <- as.character(Loc_ID)
      
      for(B in 1:length(corr_wrd))
      {
        sub_set <- dplyr::filter(ward_shapefile@data, Ward_Name == corr_wrd[B])
        
        if(nrow(sub_set)>0)
        {
          Loc_ID[B] <- as.character(sub_set$Loc_ID)
          
        } else {
          Loc_ID[B] <- as.character(NA)
        }
      }
      
      mylist_resp_code_w[[W]] <- Loc_ID
    }
    
    code_identified_v <- as.data.frame(mylist_resp_code_v)
    
    ## rename the columns 
    totcol <- length(code_identified_v)
    for(O in 1:totcol)
    {
      newname <- paste("Loc_ID_", O, sep="")
      colnames(code_identified_v)[O] <- newname
    }
    
    
    code_identified_w <- as.data.frame(mylist_resp_code_w)
    ## rename the columns 
    totcol <- length(code_identified_w)
    for(P in 1:totcol)
    {
      newname <- paste("Loc_ID_", P, sep="")
      colnames(code_identified_w)[P] <- newname
    }
 
    corrections_final_v <- cbind(correct_villages_proc_df, code_identified_v)  
    corrections_final_w <- cbind(correct_wards_proc_df, code_identified_w)  
    
    corrections_final <- plyr::rbind.fill(corrections_final_v, corrections_final_w)
    corrections_final <- as.data.frame(corrections_final)
  
    }
  
  corrections_final <- corrections_final[order(corrections_final$misspelled_villages),]
  return(corrections_final)
}


