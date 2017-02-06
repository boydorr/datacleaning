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
#' i) the village misspellings, stored in the first column ("misspelling");
#'  
#' ii) the similar matches, each one stored in a different row of the same column
#' ("correct_name"). The matches are ordered in the different rows according to their 
#' level of similarity with the misspelled village.     
#'    
#' iii) a column specifying whether the misspelling is a real alphanumeric typo
#' or not ("real_typo);    
#'    
#' iv) a column storing the region/district/ward information in the project dataframe associated
#' to the misspelled village ("region_in_df", "district_in_df" or "ward_in_df"),
#' depending on the higher_level argument specified in the function call. This 
#' column is only returned when only_in_distr=FALSE;       
#'       
#' v) a column specifying the level of the proposed correction ("correction_level"). 
#' When look_ward=TRUE, the level could be either "Village" or "Ward".  
#'    
#' vi) a column storing the level of similarity of the proposed corrections ("lev_sim");  
#'   
#' vii) a column specifying, for each misspelling, the hierarchical rank of each 
#' proposed correction ("choice_number"), with choice_number=1 indicating the most 
#' strongely suggested correction.  
#'   
#' viii) a column specifying the region/district/ward in the shapefile where each 
#' proposed correction is located ("correct_name_in_region", "correct_name_in_district", 
#' "correct_name_in_ward");     
#'      
#' ix) a column storing the location ID of each proposed correction ("Loc_ID").  
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
    only_in_distr=NA
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
    
    n_obs <- rep(0, nrow(villages_correct_name))
    for(y in 1:nrow(villages_correct_name))
    {
      n_obs[y] <- sum(!is.na(villages_correct_name[y,]))
    }
    
    misspelled_villages <- check_df$respective_vill
    misspelled_villages <- as.character(misspelled_villages)
    
    missp <- as.character()
    misspell <- as.character()
    
    r_typo <- as.character()
    typo_real <- as.character()
    
    ## cbind the ward
    Ward_in_df <- check_df$respective_ward
    Ward_in_df <- as.character(Ward_in_df)
    ward_df <- as.character()
    df_ward <- as.character()
    
    first.time=T
    
    for(t in 1:length(n_obs))
    {
      if(sum(n_obs[t])==0)
      {
        misspell <- misspelled_villages[t]
        typo_real <- real_typo[t]
        ward_df <- Ward_in_df[t]
        
      } else {
        misspell <- rep(misspelled_villages[t], n_obs[t])
        typo_real <- rep(real_typo[t], n_obs[t])
        ward_df <- rep(Ward_in_df[t], n_obs[t])
        
      }
      
      if(first.time==T)
      {
        missp <- misspell
        r_typo <- typo_real
        df_ward <- ward_df
        
        first.time=F
      } else {
        missp <- c(missp, misspell)
        r_typo <- c(r_typo, typo_real)
        df_ward <- c(df_ward, ward_df)
      }
    }
    
    for(u in 1:length(mylist_similar_villages))
    {
      if(length(mylist_similar_villages[[u]])==0)
      {
        mylist_similar_villages[[u]] <- NA
      } else {
        next
      }
    }
    
    villages_identified_all <- plyr::ldply(mylist_similar_villages, cbind)
    
    corrections <- cbind(missp, villages_identified_all, r_typo, df_ward)
    corrections <- as.data.frame(corrections)
    
    colnames(corrections)[1] <- "misspelling"
    colnames(corrections)[2] <- "correct_name"
    colnames(corrections)[3] <- "real_typo"
    colnames(corrections)[4] <- "ward_in_df"
    
    
    mis_names <- as.character(unique(corrections$misspelling))
    
    first.time=T
    for(x in 1:length(mis_names))
    {
      corrs <- dplyr::filter(corrections, misspelling==mis_names[x])
      
      mis <- as.character(mis_names[x])
      cor <- as.character(corrs$correct_name)
      
      lev_sim <- RecordLinkage::levenshteinSim(mis, cor)
      
      corrs <- cbind(corrs, lev_sim)
      corrs <- corrs[order(corrs$lev_sim, decreasing = T),]
      
      choice_number <- seq(1:nrow(corrs))
      corrs <- cbind(corrs, choice_number)
      
      if(first.time==T)
      {
        ordered_corrs <- corrs
        first.time=F
        
      } else {
        ordered_corrs <- rbind.data.frame(ordered_corrs, corrs)
        ordered_corrs <- as.data.frame(ordered_corrs)
      }
    }
    
    corrections <- ordered_corrs
    
    tot_obs <- nrow(corrections)
    correction_level <- c()
    correction_level <- as.character(correction_level)
    correction_level <- rep("Village", tot_obs)
    corrections <- cbind(corrections, correction_level)
    
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
          
          ## the correct villages and correct districts
          villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
          villages_correct_name <- villages_identified
          districts_identified <- plyr::ldply(mylist_respective_districts, rbind)
          districts_correct_name <- districts_identified
          
          n_obs <- rep(0, nrow(villages_correct_name))
          for(y in 1:nrow(villages_correct_name))
          {
            n_obs[y] <- sum(!is.na(villages_correct_name[y,]))
          }
          
          ## the misspelled villages
          misspelled_villages <- respective_vill
          misspelled_villages <- as.character(misspelled_villages)
          
          missp <- as.character()
          misspell <- as.character()
          
          r_typo <- as.character()
          typo_real <- as.character()
          
          
          first.time=T
          
          for(t in 1:length(n_obs))
          {
            if(sum(n_obs[t])==0)
            {
              misspell <- misspelled_villages[t]
              typo_real <- real_typo[t]
              
            } else {
              misspell <- rep(misspelled_villages[t], n_obs[t])
              typo_real <- rep(real_typo[t], n_obs[t])
              
            }
            
            if(first.time==T)
            {
              missp <- misspell
              r_typo <- typo_real
              
              first.time=F
            } else {
              missp <- c(missp, misspell)
              r_typo <- c(r_typo, typo_real)
            }
          }
          
          
          for(u in 1:length(mylist_similar_villages))
          {
            if(length(mylist_similar_villages[[u]])==0)
            {
              mylist_similar_villages[[u]] <- NA
            } else {
              next
            }
          }
          
          for(u in 1:length(mylist_respective_districts))
          {
            if(length(mylist_respective_districts[[u]])==0)
            {
              mylist_respective_districts[[u]] <- NA
            } else {
              next
            }
          }
          
          villages_identified_all <- plyr::ldply(mylist_similar_villages, cbind)
          districts_identified_all <- plyr::ldply(mylist_respective_districts, cbind)
          
          corrections <- cbind(missp, villages_identified_all, r_typo, districts_identified_all)
          corrections <- as.data.frame(corrections)
          
          colnames(corrections)[1] <- "misspelling"
          colnames(corrections)[2] <- "correct_name"
          colnames(corrections)[3] <- "real_typo"
          colnames(corrections)[4] <- "correct_name_in_district"
          
          corrections <- unique(corrections)
          
          mis_names <- as.character(unique(corrections$misspelling))
          
          first.time=T
          for(x in 1:length(mis_names))
          {
            corrs <- dplyr::filter(corrections, misspelling==mis_names[x])
            
            mis <- as.character(mis_names[x])
            cor <- as.character(corrs$correct_name)
            
            ## calculate the index of similarity and sort the results from the most similar to the most distant
            lev_sim <- RecordLinkage::levenshteinSim(mis, cor)
            
            corrs <- cbind(corrs, lev_sim)
            corrs <- corrs[order(corrs$lev_sim, decreasing = T),] 
            
            choice_number <- seq(1:nrow(corrs))
            corrs <- cbind(corrs, choice_number)
            
            if(first.time==T)
            {
              ordered_corrs <- corrs
              first.time=F
              
            } else {
              ordered_corrs <- rbind.data.frame(ordered_corrs, corrs)
              ordered_corrs <- as.data.frame(ordered_corrs)
            }
          }
          
          corrections <- ordered_corrs
          
          tot_obs <- nrow(corrections)
          correction_level <- c()
          correction_level <- as.character(correction_level)
          correction_level <- rep("Village", tot_obs)
          
          
          corrections <- cbind(corrections, correction_level)
          
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
          
          n_obs <- rep(0, nrow(villages_correct_name))
          for(y in 1:nrow(villages_correct_name))
          {
            n_obs[y] <- sum(!is.na(villages_correct_name[y,]))
          }
          
          misspelled_villages <- check_df$respective_vill
          misspelled_villages <- as.character(misspelled_villages)
          
          
          missp <- as.character()
          misspell <- as.character()
          
          r_typo <- as.character()
          typo_real <- as.character()
          
          ## cbind the district
          District_in_df <- check_df$respective_district
          District_in_df <- as.character(District_in_df)
          dis_df <- as.character()
          df_dis <- as.character()
          
          first.time=T
          
          for(t in 1:length(n_obs))
          {
            if(sum(n_obs[t])==0)
            {
              misspell <- misspelled_villages[t]
              typo_real <- real_typo[t]
              dis_df <- District_in_df[t]
              
            } else {
              misspell <- rep(misspelled_villages[t], n_obs[t])
              typo_real <- rep(real_typo[t], n_obs[t])
              dis_df <- rep(District_in_df[t], n_obs[t])
              
            }
            
            if(first.time==T)
            {
              missp <- misspell
              r_typo <- typo_real
              df_dis <- dis_df
              
              first.time=F
            } else {
              missp <- c(missp, misspell)
              r_typo <- c(r_typo, typo_real)
              df_dis <- c(df_dis, dis_df)
            }
          }
          
          
          for(u in 1:length(mylist_similar_villages))
          {
            if(length(mylist_similar_villages[[u]])==0)
            {
              mylist_similar_villages[[u]] <- NA
            } else {
              next
            }
          }
          
          villages_identified_all <- plyr::ldply(mylist_similar_villages, cbind)
          
          corrections <- cbind(missp, villages_identified_all, r_typo, df_dis)
          corrections <- as.data.frame(corrections)
          
          colnames(corrections)[1] <- "misspelling"
          colnames(corrections)[2] <- "correct_name"
          colnames(corrections)[3] <- "real_typo"
          colnames(corrections)[4] <- "district_in_df"
          
         
          mis_names <- as.character(unique(corrections$misspelling))
          
          first.time=T
          for(x in 1:length(mis_names))
          {
            corrs <- dplyr::filter(corrections, misspelling==mis_names[x])
            
            mis <- as.character(mis_names[x])
            cor <- as.character(corrs$correct_name)
            
            lev_sim <- RecordLinkage::levenshteinSim(mis, cor)
            
            corrs <- cbind(corrs, lev_sim)
            corrs <- corrs[order(corrs$lev_sim, decreasing = T),]
            
            choice_number <- seq(1:nrow(corrs))
            corrs <- cbind(corrs, choice_number)
            
            if(first.time==T)
            {
              ordered_corrs <- corrs
              first.time=F
              
            } else {
              ordered_corrs <- rbind.data.frame(ordered_corrs, corrs)
              ordered_corrs <- as.data.frame(ordered_corrs)
            }
          }
          
          corrections <- ordered_corrs
          
          tot_obs <- nrow(corrections)
          correction_level <- c()
          correction_level <- as.character(correction_level)
          correction_level <- rep("Village", tot_obs)
          
          
          corrections <- cbind(corrections, correction_level)
          
        }
        
      } else { ## if look_ward==T
        
        if (!is.na(only_in_distr))
        {
          only_in_distr <- as.character(only_in_distr)
          
          ## respective_vill: the misspelled village names
          respective_villages <- as.character(respective_vill)
          respective_vill <- as.character(respective_vill)
          
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
          
          missp_vill_ward <- as.character()
          
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
                missp_vill_ward[m] <- NA
                
              } else {
                mylist_similar_wards[[m]] <- number_w
                mylist_similar_wards[[m]] <- correct_subset_ward[mylist_similar_wards[[m]]]
                
                mylist_respective_districts_w[[m]] <- number_w
                mylist_respective_districts_w[[m]] <- correct_subset_dis[mylist_respective_districts_w[[m]]]
                
                missp_vill_ward[m] <- respective_vill[m]
                
                unique_ward <- !duplicated(mylist_similar_wards[[m]])
                unique_ind_w <- seq_along(mylist_similar_wards[[m]])[unique_ward]
                mylist_similar_wards[[m]] <- mylist_similar_wards[[m]][unique_ind_w]
                
                mylist_respective_districts_w[[m]] <- mylist_respective_districts_w[[m]][unique_ind_w]
                
              }
            }
          }
          
          #############################
          #############################          
          #############################         
          
          ## the correct villages and correct districts
          villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
          villages_correct_name <- villages_identified
          
          districts_identified <- plyr::ldply(mylist_respective_districts_v, rbind)
          districts_correct_name <- districts_identified
          
          n_obs <- rep(0, nrow(villages_correct_name))
          for(y in 1:nrow(villages_correct_name))
          {
            n_obs[y] <- sum(!is.na(villages_correct_name[y,]))
          }
          
          ## the misspelled villages
          misspelled_villages <- respective_villages
          misspelled_villages <- as.character(misspelled_villages)
          
          
          missp <- as.character()
          misspell <- as.character()
          
          r_typo <- as.character()
          typo_real <- as.character()
          
          first.time=T
          
          for(t in 1:length(n_obs))
          {
            if(sum(n_obs[t])==0)
            {
              misspell <- misspelled_villages[t]
              typo_real <- real_typo[t]
              
            } else {
              misspell <- rep(misspelled_villages[t], n_obs[t])
              typo_real <- rep(real_typo[t], n_obs[t])
              
            }
            
            if(first.time==T)
            {
              missp <- misspell
              r_typo <- typo_real
              first.time=F
              
            } else {
              missp <- c(missp, misspell)
              r_typo <- c(r_typo, typo_real)
            }
          }
          
          
          for(u in 1:length(mylist_similar_villages))
          {
            if(length(mylist_similar_villages[[u]])==0)
            {
              mylist_similar_villages[[u]] <- NA
            } else {
              next
            }
          }
          
          
          for(g in 1:length(mylist_respective_districts_v))
          {
            if(length(mylist_respective_districts_v[[g]])==0)
            {
              mylist_respective_districts_v[[g]] <- NA
            } else {
              next
            }
          }
          
          villages_identified_all <- plyr::ldply(mylist_similar_villages, cbind)
          districts_identified_all <- plyr::ldply(mylist_respective_districts_v, cbind)
          
          
          
          correction_level <- rep("Village", nrow(villages_identified_all))
          
          corrections <- cbind(missp, villages_identified_all, r_typo, 
                               districts_identified_all, correction_level)
          
          
          corrections <- as.data.frame(corrections)
          
          colnames(corrections)[1] <- "misspelling"
          colnames(corrections)[2] <- "correct_name"
          colnames(corrections)[3] <- "real_typo"
          colnames(corrections)[4] <- "correct_name_in_district"
          colnames(corrections)[5] <- "correction_level"
          
          corrections <- unique(corrections)
          corrections_v <- corrections
          
          
          #############################  
          
          ## the correct wards and correct districts
          wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
          wards_correct_name <- wards_identified
          districts_identified <- plyr::ldply(mylist_respective_districts_w, rbind)
          districts_correct_name <- districts_identified
          
          n_obs <- rep(0, nrow(wards_correct_name))
          for(y in 1:nrow(wards_correct_name))
          {
            n_obs[y] <- sum(!is.na(wards_correct_name[y,]))
          }
          
          ## the misspelled wards (which were stored as villages)
          misspelled_wards <- missp_vill_ward
          misspelled_wards <- as.character(misspelled_wards)
          
          missp <- as.character()
          misspell <- as.character()
          
          r_typo <- as.character()
          typo_real <- as.character()
          
          first.time=T
          
          for(t in 1:length(n_obs))
          {
            if(sum(n_obs[t])==0)
            {
              misspell <- misspelled_wards[t]
              
            } else {
              misspell <- rep(misspelled_wards[t], n_obs[t])
              
            }
            
            if(first.time==T)
            {
              missp <- misspell
              
              first.time=F
            } else {
              missp <- c(missp, misspell)
            }
          }
          
          
          for(u in 1:length(mylist_similar_wards))
          {
            if(length(mylist_similar_wards[[u]])==0)
            {
              mylist_similar_wards[[u]] <- NA
            } else {
              next
            }
          }
          
          
          for(g in 1:length(mylist_respective_districts_w))
          {
            if(length(mylist_respective_districts_w[[g]])==0)
            {
              mylist_respective_districts_w[[g]] <- NA
            } else {
              next
            }
          }
          
          wards_identified_all <- plyr::ldply(mylist_similar_wards, cbind)
          districts_identified_all <- plyr::ldply(mylist_respective_districts_w, cbind)
          
          r_typo <- rep("TRUE", nrow(wards_identified_all))
          correction_level <- rep("Ward", nrow(wards_identified_all))
          
          corrections <- cbind(missp, wards_identified_all, r_typo, 
                               districts_identified_all, correction_level)
          
          corrections <- as.data.frame(corrections)
          
          colnames(corrections)[1] <- "misspelling"
          colnames(corrections)[2] <- "correct_name"
          colnames(corrections)[3] <- "real_typo"
          colnames(corrections)[4] <- "correct_name_in_district"
          colnames(corrections)[5] <- "correction_level"
          
          corrections <- unique(corrections)
          corrections_w <- corrections
          
          ## The final dataframe without the uninformative rows
          nocol <- c("misspelling", "real_typo", "correction_level")
          nonums <- which(names(corrections_w) %in% nocol)
          corrections_w <- corrections_w[rowSums(!is.na(corrections_w[,-nonums])) > 0, ]
          
          #############################  
          
          corrections <- rbind(corrections_v, corrections_w)
          
          mis_names <- as.character(unique(corrections$misspelling))
          
          first.time=T
          for(x in 1:length(mis_names))
          {
            corrs <- dplyr::filter(corrections, misspelling==mis_names[x])
            
            mis <- as.character(mis_names[x])
            cor <- as.character(corrs$correct_name)
            
            ## calculate the index of similarity and sort the results from the most similar to the most distant
            lev_sim <- RecordLinkage::levenshteinSim(mis, cor)
            
            corrs <- cbind(corrs, lev_sim)
            corrs <- corrs[order(corrs$lev_sim, decreasing = T),] 
            
            choice_number <- seq(1:nrow(corrs))
            corrs <- cbind(corrs, choice_number)
            
            if(first.time==T)
            {
              ordered_corrs <- corrs
              first.time=F
              
            } else {
              ordered_corrs <- rbind.data.frame(ordered_corrs, corrs)
              ordered_corrs <- as.data.frame(ordered_corrs)
            }
          }
          
          corrections <- ordered_corrs
          
          
          
          
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
          
          respective_vill <- as.character(check_df$respective_vill) 
          respective_district <- as.character(check_df$respective_district) 
          
          missp_vill_ward <- as.character()
          missp_dis_ward <- as.character()
          
          
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
                  missp_dis_ward[q] <- NA
                  missp_vill_ward[q] <- NA
                  mylist_similar_wards[[q]] <- NA
                  mylist_respective_districts_w[[q]] <- NA
                  
                } else {
                  missp_dis_ward[q] <- respective_district[q]
                  missp_vill_ward[q] <- respective_vill[q]
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
                  missp_vill_ward[q] <- NA
                  
                } else {
                  missp_vill_ward[q] <- respective_vill[q]
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
          #############################          
          #############################          
          
          ## the correct villages
          villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
          villages_correct_name <- villages_identified
          
          n_obs <- rep(0, nrow(villages_correct_name))
          for(y in 1:nrow(villages_correct_name))
          {
            n_obs[y] <- sum(!is.na(villages_correct_name[y,]))
          }
          
          misspelled_villages <- check_df$respective_vill
          misspelled_villages <- as.character(misspelled_villages)
          
          missp <- as.character()
          misspell <- as.character()
          
          r_typo <- as.character()
          typo_real <- as.character()
          
          ## cbind the district
          District_in_df <- check_df$respective_district
          District_in_df <- as.character(District_in_df)
          dis_df <- as.character()
          df_dis <- as.character()
          
          first.time=T
          
          for(t in 1:length(n_obs))
          {
            if(sum(n_obs[t])==0)
            {
              misspell <- misspelled_villages[t]
              typo_real <- real_typo[t]
              dis_df <- District_in_df[t]
              
            } else {
              misspell <- rep(misspelled_villages[t], n_obs[t])
              typo_real <- rep(real_typo[t], n_obs[t])
              dis_df <- rep(District_in_df[t], n_obs[t])
              
            }
            
            if(first.time==T)
            {
              missp <- misspell
              r_typo <- typo_real
              df_dis <- dis_df
              
              first.time=F
            } else {
              missp <- c(missp, misspell)
              r_typo <- c(r_typo, typo_real)
              df_dis <- c(df_dis, dis_df)
            }
          }
          
          
          for(u in 1:length(mylist_similar_villages))
          {
            if(length(mylist_similar_villages[[u]])==0)
            {
              mylist_similar_villages[[u]] <- NA
            } else {
              next
            }
          }
          
          for(u in 1:length(mylist_respective_districts_v))
          {
            if(length(mylist_respective_districts_v[[u]])==0)
            {
              mylist_respective_districts_v[[u]] <- NA
            } else {
              next
            }
          }
          
          
          villages_identified_all <- plyr::ldply(mylist_similar_villages, cbind)
          districts_identified_all <- plyr::ldply(mylist_respective_districts_v, cbind)
          correction_level <- rep("Village", nrow(villages_identified_all))
          corrections <- cbind(missp, villages_identified_all, r_typo, df_dis, 
                               correction_level, districts_identified_all)
          
          corrections <- as.data.frame(corrections)
          
          colnames(corrections)[1] <- "misspelling"
          colnames(corrections)[2] <- "correct_name"
          colnames(corrections)[3] <- "real_typo"
          colnames(corrections)[4] <- "district_in_df"
          colnames(corrections)[5] <- "correction_level"
          colnames(corrections)[6] <- "correct_name_in_district"
          
          corrections <- unique(corrections)
          corrections_v <- corrections
          
          
          #############################  
          
          ## the correct wards 
          wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
          wards_correct_name <- wards_identified
          
          n_obs <- rep(0, nrow(wards_correct_name))
          for(y in 1:nrow(wards_correct_name))
          {
            n_obs[y] <- sum(!is.na(wards_correct_name[y,]))
          }
          
          ## the misspelled wards (which were stored as villages)
          misspelled_wards <- missp_vill_ward
          misspelled_wards <- as.character(misspelled_wards)
          
          missp <- as.character()
          misspell <- as.character()
          
          ## cbind the district
          District_in_df <- missp_dis_ward
          District_in_df <- as.character(District_in_df)
          dis_df <- as.character()
          df_dis <- as.character()
          
          first.time=T
          
          for(t in 1:length(n_obs))
          {
            if(sum(n_obs[t])==0)
            {
              misspell <- misspelled_wards[t]
              dis_df <- District_in_df[t]
              
            } else {
              misspell <- rep(misspelled_wards[t], n_obs[t])
              dis_df <- rep(District_in_df[t], n_obs[t])
              
            }
            
            if(first.time==T)
            {
              missp <- misspell
              df_dis <- dis_df
              
              first.time=F
            } else {
              missp <- c(missp, misspell)
              df_dis <- c(df_dis, dis_df)
            }
          }
          
          
          for(u in 1:length(mylist_similar_wards))
          {
            if(length(mylist_similar_wards[[u]])==0)
            {
              mylist_similar_wards[[u]] <- NA
            } else {
              next
            }
          }
          
          for(u in 1:length(mylist_respective_districts_w))
          {
            if(length(mylist_respective_districts_w[[u]])==0)
            {
              mylist_respective_districts_w[[u]] <- NA
            } else {
              next
            }
          }
          
          wards_identified_all <- plyr::ldply(mylist_similar_wards, cbind)
          districts_identified_all <- plyr::ldply(mylist_respective_districts_w, cbind)
          
          r_typo <- rep("TRUE", nrow(wards_identified_all))
          correction_level <- rep("Ward", nrow(wards_identified_all))
          corrections <- cbind(missp, wards_identified_all, r_typo, df_dis, 
                               correction_level, districts_identified_all)
          
          corrections <- as.data.frame(corrections)
          
          colnames(corrections)[1] <- "misspelling"
          colnames(corrections)[2] <- "correct_name"
          colnames(corrections)[3] <- "real_typo"
          colnames(corrections)[4] <- "district_in_df"
          colnames(corrections)[5] <- "correction_level"
          colnames(corrections)[6] <- "correct_name_in_district"
          
          corrections <- unique(corrections)
          corrections_w <- corrections
          
          
          ## The final dataframe without the uninformative rows
          nocol <- c("misspelling", "real_typo", "correction_level")
          nonums <- which(names(corrections_w) %in% nocol)
          corrections_w <- corrections_w[rowSums(!is.na(corrections_w[,-nonums])) > 0, ]
          
          
          #############################  
          
          corrections <- rbind(corrections_v, corrections_w)
          
          mis_names <- as.character(unique(corrections$misspelling))
          
          first.time=T
          for(x in 1:length(mis_names))
          {
            corrs <- dplyr::filter(corrections, misspelling==mis_names[x])
            
            mis <- as.character(mis_names[x])
            cor <- as.character(corrs$correct_name)
            
            ## calculate the index of similarity and sort the results from the most similar to the most distant
            lev_sim <- RecordLinkage::levenshteinSim(mis, cor)
            
            corrs <- cbind(corrs, lev_sim)
            corrs <- corrs[order(corrs$lev_sim, decreasing = T),] 
            
            choice_number <- seq(1:nrow(corrs))
            corrs <- cbind(corrs, choice_number)
            
            if(first.time==T)
            {
              ordered_corrs <- corrs
              first.time=F
              
            } else {
              ordered_corrs <- rbind.data.frame(ordered_corrs, corrs)
              ordered_corrs <- as.data.frame(ordered_corrs)
            }
          }
          
          corrections <- ordered_corrs
          
        }
      }
      
    } else { ## higher_level = Region
      
      only_in_distr=NA
      
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
        
        n_obs <- rep(0, nrow(villages_correct_name))
        for(y in 1:nrow(villages_correct_name))
        {
          n_obs[y] <- sum(!is.na(villages_correct_name[y,]))
        }
        
        misspelled_villages <- check_df$respective_vill
        misspelled_villages <- as.character(misspelled_villages)
        
        missp <- as.character()
        misspell <- as.character()
        
        r_typo <- as.character()
        typo_real <- as.character()
        
        ## cbind the region
        Region_in_df <- check_df$respective_region
        Region_in_df <- as.character(Region_in_df)
        reg_df <- as.character()
        df_reg <- as.character()
        
        first.time=T
        
        for(t in 1:length(n_obs))
        {
          if(sum(n_obs[t])==0)
          {
            misspell <- misspelled_villages[t]
            typo_real <- real_typo[t]
            reg_df <- Region_in_df[t]
            
          } else {
            misspell <- rep(misspelled_villages[t], n_obs[t])
            typo_real <- rep(real_typo[t], n_obs[t])
            reg_df <- rep(Region_in_df[t], n_obs[t])
            
          }
          
          if(first.time==T)
          {
            missp <- misspell
            r_typo <- typo_real
            df_reg <- reg_df
            
            first.time=F
          } else {
            missp <- c(missp, misspell)
            r_typo <- c(r_typo, typo_real)
            df_reg <- c(df_reg, reg_df)
          }
        }
        
        
        for(u in 1:length(mylist_similar_villages))
        {
          if(length(mylist_similar_villages[[u]])==0)
          {
            mylist_similar_villages[[u]] <- NA
          } else {
            next
          }
        }
        
        villages_identified_all <- plyr::ldply(mylist_similar_villages, cbind)
        
        corrections <- cbind(missp, villages_identified_all, r_typo, df_reg)
        corrections <- as.data.frame(corrections)
        
        colnames(corrections)[1] <- "misspelling"
        colnames(corrections)[2] <- "correct_name"
        colnames(corrections)[3] <- "real_typo"
        colnames(corrections)[4] <- "region_in_df"
        
        mis_names <- as.character(unique(corrections$misspelling))
        
        first.time=T
        for(x in 1:length(mis_names))
        {
          corrs <- dplyr::filter(corrections, misspelling==mis_names[x])
          
          mis <- as.character(mis_names[x])
          cor <- as.character(corrs$correct_name)
          
          lev_sim <- RecordLinkage::levenshteinSim(mis, cor)
          
          corrs <- cbind(corrs, lev_sim)
          corrs <- corrs[order(corrs$lev_sim, decreasing = T),]
          
          choice_number <- seq(1:nrow(corrs))
          corrs <- cbind(corrs, choice_number)
          
          if(first.time==T)
          {
            ordered_corrs <- corrs
            first.time=F
            
          } else {
            ordered_corrs <- rbind.data.frame(ordered_corrs, corrs)
            ordered_corrs <- as.data.frame(ordered_corrs)
          }
        }
        
        corrections <- ordered_corrs
        
        tot_obs <- nrow(corrections)
        correction_level <- c()
        correction_level <- as.character(correction_level)
        correction_level <- rep("Village", tot_obs)
        
        corrections <- cbind(corrections, correction_level)
        
        
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
        
        respective_vill <- as.character(check_df$respective_vill) 
        respective_region <- as.character(check_df$respective_region) 
        
        missp_vill_ward <- as.character()
        missp_reg_ward <- as.character()
        
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
            correct_subset_reg <- as.character(correct_subset_reg)
            
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
                missp_reg_ward[q] <- NA
                missp_vill_ward[q] <- NA
                mylist_similar_wards[[q]] <- NA
                mylist_respective_regions_w[[q]] <- NA
                
              } else {
                missp_reg_ward[q] <- respective_region[q]
                missp_vill_ward[q] <- respective_vill[q]
                
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
                missp_vill_ward[q] <- NA
                
              } else {
                missp_vill_ward[q] <- respective_vill[q]
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
        #############################          
        #############################    
        
        ## the correct villages
        villages_identified <- plyr::ldply(mylist_similar_villages, rbind)
        villages_correct_name <- villages_identified
        
        n_obs <- rep(0, nrow(villages_correct_name))
        for(y in 1:nrow(villages_correct_name))
        {
          n_obs[y] <- sum(!is.na(villages_correct_name[y,]))
        }
        
        misspelled_villages <- check_df$respective_vill
        misspelled_villages <- as.character(misspelled_villages)
        
        missp <- as.character()
        misspell <- as.character()
        
        r_typo <- as.character()
        typo_real <- as.character()
        
        ## cbind the district
        Region_in_df <- check_df$respective_region
        Region_in_df <- as.character(Region_in_df)
        reg_df <- as.character()
        df_reg <- as.character()
        
        
        first.time=T
        
        for(t in 1:length(n_obs))
        {
          if(sum(n_obs[t])==0)
          {
            misspell <- misspelled_villages[t]
            typo_real <- real_typo[t]
            reg_df <- Region_in_df[t]
            
          } else {
            misspell <- rep(misspelled_villages[t], n_obs[t])
            typo_real <- rep(real_typo[t], n_obs[t])
            reg_df <- rep(Region_in_df[t], n_obs[t])
            
          }
          
          if(first.time==T)
          {
            missp <- misspell
            r_typo <- typo_real
            df_reg <- reg_df
            
            first.time=F
          } else {
            missp <- c(missp, misspell)
            r_typo <- c(r_typo, typo_real)
            df_reg <- c(df_reg, reg_df)
          }
        }
        
        
        for(u in 1:length(mylist_similar_villages))
        {
          if(length(mylist_similar_villages[[u]])==0)
          {
            mylist_similar_villages[[u]] <- NA
          } else {
            next
          }
        }
        
        
        for(g in 1:length(mylist_respective_regions_v))
        {
          if(length(mylist_respective_regions_v[[g]])==0)
          {
            mylist_respective_regions_v[[g]] <- NA
          } else {
            next
          }
        }
        
        villages_identified_all <- plyr::ldply(mylist_similar_villages, cbind)
        regions_identified_all <- plyr::ldply(mylist_respective_regions_v, cbind)
        correction_level <- rep("Village", nrow(villages_identified_all))
        corrections <- cbind(missp, villages_identified_all, r_typo, df_reg, 
                             correction_level, regions_identified_all)
        
        corrections <- as.data.frame(corrections)
        
        colnames(corrections)[1] <- "misspelling"
        colnames(corrections)[2] <- "correct_name"
        colnames(corrections)[3] <- "real_typo"
        colnames(corrections)[4] <- "region_in_df"
        colnames(corrections)[5] <- "correction_level"
        colnames(corrections)[6] <- "correct_name_in_region"
        
        corrections <- unique(corrections)
        corrections_v <- corrections
        
        
        #############################  
        
        ## the correct wards 
        wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
        wards_correct_name <- wards_identified
        
        n_obs <- rep(0, nrow(wards_correct_name))
        for(y in 1:nrow(wards_correct_name))
        {
          n_obs[y] <- sum(!is.na(wards_correct_name[y,]))
        }
        
        ## the misspelled wards (which were stored as villages)
        misspelled_wards <- missp_vill_ward
        misspelled_wards <- as.character(misspelled_wards)
        
        missp <- as.character()
        misspell <- as.character()
        
        ## cbind the Region
        Region_in_df <- missp_reg_ward
        Region_in_df <- as.character(Region_in_df)
        reg_df <- as.character()
        df_reg <- as.character()
        
        first.time=T
        
        for(t in 1:length(n_obs))
        {
          if(sum(n_obs[t])==0)
          {
            misspell <- misspelled_wards[t]
            reg_df <- Region_in_df[t]
            
          } else {
            misspell <- rep(misspelled_wards[t], n_obs[t])
            reg_df <- rep(Region_in_df[t], n_obs[t])
            
          }
          
          if(first.time==T)
          {
            missp <- misspell
            df_reg <- reg_df
            
            first.time=F
          } else {
            missp <- c(missp, misspell)
            df_reg <- c(df_reg, reg_df)
          }
        }
        
        
        for(u in 1:length(mylist_similar_wards))
        {
          if(length(mylist_similar_wards[[u]])==0)
          {
            mylist_similar_wards[[u]] <- NA
          } else {
            next
          }
        }
        
        
        for(g in 1:length(mylist_respective_regions_w))
        {
          if(length(mylist_respective_regions_w[[g]])==0)
          {
            mylist_respective_regions_w[[g]] <- NA
          } else {
            next
          }
        }
        
        wards_identified_all <- plyr::ldply(mylist_similar_wards, cbind)
        regions_identified_all <- plyr::ldply(mylist_respective_regions_w, cbind)
        
        r_typo <- rep("TRUE", nrow(wards_identified_all))
        correction_level <- rep("Ward", nrow(wards_identified_all))
        corrections <- cbind(missp, wards_identified_all, r_typo, df_reg, 
                             correction_level, regions_identified_all)
        
        corrections <- as.data.frame(corrections)
        
        colnames(corrections)[1] <- "misspelling"
        colnames(corrections)[2] <- "correct_name"
        colnames(corrections)[3] <- "real_typo"
        colnames(corrections)[4] <- "region_in_df"
        colnames(corrections)[5] <- "correction_level"
        colnames(corrections)[6] <- "correct_name_in_region"
        
        corrections <- unique(corrections)
        corrections_w <- corrections
        
        
        ## The final dataframe without the uninformative rows
        nocol <- c("misspelling", "real_typo", "correction_level")
        nonums <- which(names(corrections_w) %in% nocol)
        corrections_w <- corrections_w[rowSums(!is.na(corrections_w[,-nonums])) > 0, ]
        
        
        #############################  
        
        corrections <- rbind(corrections_v, corrections_w)
        
        mis_names <- as.character(unique(corrections$misspelling))
        
        first.time=T
        for(x in 1:length(mis_names))
        {
          corrs <- dplyr::filter(corrections, misspelling==mis_names[x])
          
          mis <- as.character(mis_names[x])
          cor <- as.character(corrs$correct_name)
          
          ## calculate the index of similarity and sort the results from the most similar to the most distant
          lev_sim <- RecordLinkage::levenshteinSim(mis, cor)
          
          corrs <- cbind(corrs, lev_sim)
          corrs <- corrs[order(corrs$lev_sim, decreasing = T),] 
          
          choice_number <- seq(1:nrow(corrs))
          corrs <- cbind(corrs, choice_number)
          
          if(first.time==T)
          {
            ordered_corrs <- corrs
            first.time=F
            
          } else {
            ordered_corrs <- rbind.data.frame(ordered_corrs, corrs)
            ordered_corrs <- as.data.frame(ordered_corrs)
          }
        }
        
        corrections <- ordered_corrs
      }
    }
  }
  
  ########################################################################
  ########################################################################
  
  if(higher_level=="Ward")
  {
    corrections_II <- corrections
    corr_vill <- corrections_II$correct_name
    corr_vill <- as.character(corr_vill)
    Loc_ID <- rep(NA, length(corr_vill))
    Loc_ID <- as.character(corr_vill)
    wardname <- rep(NA, length(corr_vill))
    wardname <- as.character(wardname)
    
    for(b in 1:length(corr_vill))
    {
      sub_set <- dplyr::filter(shapefile@data, Vil_Mtaa_N == corr_vill[b])
      
      if(nrow(sub_set)>0)
      {
        wardname[b] <- as.character(sub_set$Ward_Name)
        Loc_ID[b] <- as.character(sub_set$Loc_ID)
        
      } else {
        wardname[b] <- as.character(NA)
        Loc_ID[b] <- as.character(NA)
      }
    }
    
    correct_name_in_ward <- wardname
    corrections_II <- cbind(corrections_II, correct_name_in_ward, Loc_ID)
    corrections_II <- as.data.frame(corrections_II)
    
  } else {
    ## If the "only_in_distr" argument is not NA
    if (!is.na(only_in_distr))
    {
      corrections_II <- corrections
      index_ward <- which(corrections_II$correction_level=="Ward")
      corr_vill <- corrections_II$correct_name
      corr_vill <- as.character(corr_vill)
      
      Loc_ID <- rep(NA, length(corr_vill))
      Loc_ID <- as.character(corr_vill)
      
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
      
      corrections_II <- cbind(corrections_II, Loc_ID)
      corrections_II <- as.data.frame(corrections_II)
      
      corrections_II$Loc_ID[index_ward] <- NA
      
      if(length(index_ward)>0)
      {
        corr_ward <- corrections_II$correct_name[index_ward]
        corr_ward <- as.character(corr_ward)
        
        sub_set <- dplyr::filter(ward_shapefile@data, Ward_Name %in% corr_ward)
        
        if(nrow(sub_set)>0)
        {
          right_order <- match(corr_ward, sub_set$Ward_Name)
          
          sub_set$Loc_ID <- as.character(sub_set$Loc_ID)
          corrections_II$Loc_ID <- as.character(corrections_II$Loc_ID)
          
          corrections_II$Loc_ID[index_ward] <- sub_set$Loc_ID[right_order]
          
        } else {
          # do nothing
          sub_set <- sub_set
        }
        
      } else {
        # do nothing
        index_ward <- index_ward
      }
      
    } else { ## If the "only_in_distr" argument is NA
      
      corrections_II <- corrections
      
      index_ward <- which(corrections_II$correction_level=="Ward")
      
      corr_vill <- corrections_II$correct_name
      corr_vill <- as.character(corr_vill)
      
      Loc_ID <- rep(NA, length(corr_vill))
      Loc_ID <- as.character(Loc_ID)
      
      if(higher_level=="District")
      {
        disname <- rep(NA, length(corr_vill))
        disname <- as.character(disname)
        
        for(b in 1:length(corr_vill))
        {
          sub_set <- dplyr::filter(shapefile@data, Vil_Mtaa_N == corr_vill[b])
          
          if(nrow(sub_set)>0)
          {
            disname[b] <- as.character(sub_set$District_N)
            Loc_ID[b] <- as.character(sub_set$Loc_ID)
            
          } else {
            disname[b] <- as.character(NA)
            Loc_ID[b] <- as.character(NA)
          }
        }
        
        correct_name_in_district <- disname
        corrections_II <- cbind(corrections_II, correct_name_in_district, Loc_ID)
        corrections_II <- as.data.frame(corrections_II)
        
        corrections_II$Loc_ID[index_ward] <- NA
        corrections_II$correct_name_in_district[index_ward] <- NA
        
        correct_name_in_district <- corrections_II$correct_name_in_district 
        Loc_ID <- corrections_II$Loc_ID
        
        if(length(index_ward)>0)
        {
          corr_ward <- corrections_II$correct_name[index_ward]
          corr_ward <- as.character(corr_ward)
          
          sub_set <- dplyr::filter(ward_shapefile@data, Ward_Name %in% corr_ward)
          
          if(nrow(sub_set)>0)
          {
            right_order <- match(corr_ward, sub_set$Ward_Name)
            sub_set$Loc_ID <- as.character(sub_set$Loc_ID)
            Loc_ID <- as.character(Loc_ID)
            sub_set$District_N <- as.character(sub_set$District_N)
            correct_name_in_district <- as.character(correct_name_in_district)

            Loc_ID[index_ward] <- sub_set$Loc_ID[right_order]
            correct_name_in_district[index_ward] <- sub_set$District_N[right_order]
            
            corrections_II$Loc_ID <- NULL
            corrections_II$correct_name_in_district <- NULL
            
            corrections_II$Loc_ID <- Loc_ID
            corrections_II$correct_name_in_district <- correct_name_in_district
            
            
          } else {
            # do nothing
            sub_set <- sub_set
          }
          
        } else {
          # do nothing
          index_ward <- index_ward
        }
        
      } else { ## higher_level == Region
        
        regname <- rep(NA, length(corr_vill))
        regname <- as.character(regname)
        
        for(b in 1:length(corr_vill))
        {
          sub_set <- dplyr::filter(shapefile@data, Vil_Mtaa_N == corr_vill[b])
          
          if(nrow(sub_set)>0)
          {
            regname[b] <- as.character(sub_set$Region_Nam)
            Loc_ID[b] <- as.character(sub_set$Loc_ID)
            
          } else {
            regname[b] <- as.character(NA)
            Loc_ID[b] <- as.character(NA)
          }
        }
        
        correct_name_in_region <- regname
        corrections_II <- cbind(corrections_II, correct_name_in_region, Loc_ID)
        corrections_II <- as.data.frame(corrections_II)
        
        corrections_II$Loc_ID[index_ward] <- NA
        corrections_II$correct_name_in_region[index_ward] <- NA
        
        correct_name_in_region <- corrections_II$correct_name_in_region 
        Loc_ID <- corrections_II$Loc_ID
        
        if(length(index_ward)>0)
        {
          corr_ward <- corrections_II$correct_name[index_ward]
          corr_ward <- as.character(corr_ward)
          
          sub_set <- dplyr::filter(ward_shapefile@data, Ward_Name %in% corr_ward)
          
          if(nrow(sub_set)>0)
          {
            right_order <- match(corr_ward, sub_set$Ward_Name)
            
            sub_set$Loc_ID <- as.character(sub_set$Loc_ID)
            Loc_ID <- as.character(Loc_ID)
            sub_set$Region_Nam <- as.character(sub_set$Region_Nam)
            correct_name_in_region <- as.character(correct_name_in_region)
            
            Loc_ID[index_ward] <- sub_set$Loc_ID[right_order]
            correct_name_in_region[index_ward] <- sub_set$Region_Nam[right_order]
            
            corrections_II$Loc_ID <- NULL
            corrections_II$correct_name_in_region <- NULL
            
            corrections_II$Loc_ID <- Loc_ID
            corrections_II$correct_name_in_region <- correct_name_in_region
            
          } else {
            # do nothing
            sub_set <- sub_set
          }
          
        } else {
          # do nothing
          index_ward <- index_ward
        }
      }
    }
  }
  
  return(corrections_II)
}



