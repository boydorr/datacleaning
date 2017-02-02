#' @title Find similar ward names
#'
#' @description Identifies the misspelled ward names in the project dataset and
#' proposes a set of similar correct ward names.
#'
#' @param dataframe the project dataframe with misspelled ward names
#' @param  label the name of the column in the project dataframe storing the ward names. NOTE: The label must be specified as "label" (i.e. "ward", for instance)
#' @param  higher_level MUST be either "District" or "Region".
#' @param  label_higher_level the name of the column in the project dataframe storing the CORRECT district/region names. NOTE: The label must be specified as "label" (i.e. "district", for instance)
#' @param  shapefile the shapefile object used to retrieve the legal names
#' @param  distnc numeric value indicating the maximum distance allowed for a match (see ?agrep)
#' @param  only_in_distr (optional) a character vector containing the names of the districts in which the study took place. Default = NA
#' @param  additional_places (optional) a dataframe containing the additional names of the new wards (post 2012 census). Default = NA. NOTE: The ward/district/region column of this dataframe must be called like the @data slot in the wards shapefile
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
#' @return a new dataframe with:
#'
#' i) the ward misspellings, stored in the first column ("misspelling");
#'
#' ii) the similar matches, each one stored in a different row of the same column
#' ("correct_name"). The matches are ordered in the different rows according to their 
#' level of similarity with the misspelled ward;  
#'  
#' iii) a column specifying whether the misspelling is a real alphanumeric typo
#' or not ("real_typo);   
#'    
#' iv) a column storing the region/district information in the project dataframe 
#' associated to the misspelled ward ("region_in_df", "district_in_df"), depending 
#' on the higher_level argument specified in the function call. 
#' This column is only returned when only_in_distr=FALSE;         
#'    
#' v) a column storing the level of similarity of the proposed corrections ("lev_sim"); 
#'      
#' vi) a column specifying, for each misspelling, the hierarchical rank of each 
#' proposed correction ("choice_number"), with choice_number=1 indicating the most 
#' strongely suggested correction;     
#'     
#' vii) a column specifying the level of the proposed correction ("correction_level");
#'  
#' viii) a column specifying the region/district in the shapefile where each 
#' proposed correction is located ("correct_name_in_region", "correct_name_in_district");         
#'       
#' ix) a column storing the location ID of each proposed correction ("Loc_ID").    
#'
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
    shp_df <- dplyr::select(shapefile@data, Ward_Name, District_N, Region_Nam)
    shp_df$Ward_Name <- as.character(shp_df$Ward_Name)
    shp_df$District_N <- as.character(shp_df$District_N)
    shp_df$Region_Nam <- as.character(shp_df$Region_Nam)
    legal_names <- c(shp_df$Ward_Name, additional_places$Ward_Name)
    fullset <- rbind(shp_df, additional_places)
    
  } else {
    legal_names <- shapefile@data$Ward_Name
    fullset <- dplyr::select(shapefile@data, Ward_Name, District_N, Region_Nam)
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
  
  ## the correct wards: consider only alphanumeric characters, case insensitive
  c_v <- gsub("[^[:alnum:]]", "", tolower(wrd_correct))
  
  ## initiate vector with the wrong ward names in the csv file
  misspelled_wards <- c()
  
  ## loop through the unique wards
  for(i in 1:ward_length)
  {
    ## if in the dataframe there is a correct ward, do nothing
    if((wrd[i] %in% wrd_correct)==T)
    {
      misspelled_wards[i] <- NA
      
    } else { # otherwise, take note of the misspelling
      
      no_matches <- as.character("Looking for similar results because there are no matches for the ward: ")
      missp_ward <- as.character(wrd[i])
      
      message(no_matches, missp_ward)
      
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
    ## If the study districts are specified in the "only_in_distr" vector
    if (!is.na(only_in_distr))
    {
      only_in_distr <- as.character(only_in_distr)
      
      ## respective_ward: the misspelled ward names
      
      ## the respective district name
      respective_district <- c()
      respective_district <- as.character(respective_district)
      
      ## generate empty list for similar results
      Tot_misspell <- length(respective_ward)
      mylist_similar_wards <- vector('list', Tot_misspell)
      mylist_respective_districts <- vector('list', Tot_misspell)
      
      ## initialise the real_typo vector
      real_typo <- c(rep(NA, Tot_misspell))
      real_typo <- as.character(real_typo)
      
      ##find the similar ward names within the district subset
      correct_subset <- dplyr::filter(fullset, District_N%in%only_in_distr)
      
      correct_subset_ward <- correct_subset$Ward_Name
      correct_subset_ward <- as.character(correct_subset_ward)
      
      correct_subset_dis <- correct_subset$District_N
      correct_subset_dis <- as.character(correct_subset_dis)
      
      ## only consider alphanumeric characters of the misspelled/correct ward names.
      m_v <- gsub("[^[:alnum:]]", "", tolower(respective_ward))
      c_sub_v <- gsub("[^[:alnum:]]", "", tolower(correct_subset_ward))
      
      for(m in 1:Tot_misspell)
      {
        ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
        if((m_v[m] %in% c_sub_v)==T)
        {
          # it is not a real typo
          real_typo[m] <- FALSE
          number <- which(c_sub_v %in% m_v[m])
          mylist_similar_wards[[m]] <- number
          mylist_similar_wards[[m]] <- correct_subset_ward[mylist_similar_wards[[m]]]
          mylist_respective_districts[[m]] <- number
          mylist_respective_districts[[m]] <- correct_subset_dis[mylist_respective_districts[[m]]]
          
          unique_wards <- !duplicated(mylist_similar_wards[[m]])
          unique_ind <- seq_along(mylist_similar_wards[[m]])[unique_wards]
          
          mylist_similar_wards[[m]] <- mylist_similar_wards[[m]][unique_ind]
          mylist_respective_districts[[m]] <- mylist_respective_districts[[m]][unique_ind]
          
        } else { # if there are no case insensitive alphanumeric matches
          
          # it is a real typo
          real_typo[m] <- TRUE
          
          ## find the similar ward names within the subset
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
      
      ## the correct wards and correct districts
      wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
      wards_correct_name <- wards_identified
      districts_identified <- plyr::ldply(mylist_respective_districts, rbind)
      districts_correct_name <- districts_identified
      
      n_obs <- rep(0, nrow(wards_correct_name))
      for(y in 1:nrow(wards_correct_name))
      {
        n_obs[y] <- sum(!is.na(wards_correct_name[y,]))
      }
      
      ## the misspelled wards
      misspelled_wards <- respective_ward
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
          typo_real <- real_typo[t]
          
        } else {
          misspell <- rep(misspelled_wards[t], n_obs[t])
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
      
      wards_identified_all <- plyr::ldply(mylist_similar_wards, cbind)
      districts_identified_all <- plyr::ldply(mylist_respective_districts, cbind)
      
      corrections <- cbind(missp, wards_identified_all, r_typo, districts_identified_all)
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
      correction_level <- rep("Ward", tot_obs)
      corrections <- cbind(corrections, correction_level)
      
      #return(corrections)
      
    } else { ## if the argument only_in_distr is NA
      
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
      
      ## initialise the real_typo vector
      real_typo <- c(rep(NA, Tot_misspell))
      real_typo <- as.character(real_typo)
      
      ## only consider alphanumeric characters of the misspelled/correct village names.
      ## same names' indices
      m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_ward))
      
      for(q in 1:Tot_misspell)
      {
        if((is.na(check_df$respective_district[q]))==F) ## if there is district information
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
            real_typo[q] <- FALSE
            number <- which(c_sub_v %in% m_v[q])
            mylist_similar_wards[[q]] <- number
            mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])
            
          } else {
            real_typo[q] <- TRUE
            ##find the similar village names within the subset
            mylist_similar_wards[[q]] <- agrep(m_v[q], c_sub_v, max.distance = distnc)
            mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])
          }
          
        } else {
          ## if there is no district information
          ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
          if((m_v[q] %in% c_v)==T)
          {
            ## If there are matches, it is not a real typo
            real_typo[q] <- FALSE
            number <- which(c_v %in% m_v[q])
            mylist_similar_wards[[q]] <- number
            mylist_similar_wards[[q]] <- unique(wrd_correct[mylist_similar_wards[[q]]])
            
          } else {
            ## If there are not matches, it is a real typo
            real_typo[q] <- TRUE
            # if there are no case insensitive matches
            ##look for similar village names in the whole correct village list
            mylist_similar_wards[[q]] <- agrep(m_v[q], c_v, max.distance = distnc)
            mylist_similar_wards[[q]] <- unique(wrd_correct[mylist_similar_wards[[q]]])
          }
        }
      }
      
      
      wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
      wards_correct_name <- wards_identified
      
      n_obs <- rep(0, nrow(wards_correct_name))
      for(y in 1:nrow(wards_correct_name))
      {
        n_obs[y] <- sum(!is.na(wards_correct_name[y,]))
      }
      
      misspelled_wards <- check_df$respective_ward
      misspelled_wards <- as.character(misspelled_wards)
      
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
          misspell <- misspelled_wards[t]
          typo_real <- real_typo[t]
          dis_df <- District_in_df[t]
          
        } else {
          misspell <- rep(misspelled_wards[t], n_obs[t])
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
      
      wards_identified_all <- plyr::ldply(mylist_similar_wards, cbind)
      
      corrections <- cbind(missp, wards_identified_all, r_typo, df_dis)
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
      correction_level <- rep("Ward", tot_obs)
      corrections <- cbind(corrections, correction_level)
      
    }
    
  } else { ## higher_level = "Region"
    
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
    
    ## initialise the real_typo vector
    real_typo <- c(rep(NA, Tot_misspell))
    real_typo <- as.character(real_typo)
    
    ## only consider alphanumeric characters of the misspelled/correct village names.
    ## same names' indices
    m_v <- gsub("[^[:alnum:]]", "", tolower(check_df$respective_ward))
    
    for(q in 1:Tot_misspell)
    {
      if(!is.na(check_df$respective_region[q]))
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
          real_typo[q] <- FALSE
          number <- which(c_sub_v %in% m_v[q])
          mylist_similar_wards[[q]] <- number
          mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])
          
        } else {
          real_typo[q] <- TRUE
          ##find the similar names within the subset
          mylist_similar_wards[[q]] <- agrep(m_v[q], c_sub_v, max.distance = distnc)
          mylist_similar_wards[[q]] <- unique(correct_subset_wards[mylist_similar_wards[[q]]])
        }
        
      } else {
        ## if there is no region information
        ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
        if((m_v[q] %in% c_v)==T)
        {
          real_typo[q] <- FALSE
          number <- which(c_v %in% m_v[q])
          mylist_similar_wards[[q]] <- number
          mylist_similar_wards[[q]] <- unique(wrd_correct[mylist_similar_wards[[q]]])
        } else {
          real_typo[q] <- TRUE
          # if there are no case insensitive matches
          ##look for similar village names in the whole correct village list
          mylist_similar_wards[[q]] <- agrep(m_v[q], c_v, max.distance = distnc)
          mylist_similar_wards[[q]] <- unique(wrd_correct[mylist_similar_wards[[q]]])
        }
      }
    }
    
    wards_identified <- plyr::ldply(mylist_similar_wards, rbind)
    wards_correct_name <- wards_identified
    
    n_obs <- rep(0, nrow(wards_correct_name))
    for(y in 1:nrow(wards_correct_name))
    {
      n_obs[y] <- sum(!is.na(wards_correct_name[y,]))
    }
    
    misspelled_wards <- check_df$respective_ward
    misspelled_wards <- as.character(misspelled_wards)
    
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
        misspell <- misspelled_wards[t]
        typo_real <- real_typo[t]
        reg_df <- Region_in_df[t]
        
      } else {
        misspell <- rep(misspelled_wards[t], n_obs[t])
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
    
    wards_identified_all <- plyr::ldply(mylist_similar_wards, cbind)
    
    corrections <- cbind(missp, wards_identified_all, r_typo, df_reg)
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
    correction_level <- rep("Ward", tot_obs)
    corrections <- cbind(corrections, correction_level)
    
  }
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  
  ## If the "only_in_distr" argument is not NA
  if (!is.na(only_in_distr))
  {
    corrections_II <- corrections
    corr_ward <- corrections_II$correct_name
    corr_ward <- as.character(corr_ward)
    
    Loc_ID <- rep(NA, length(corr_ward))
    Loc_ID <- as.character(corr_ward)
    
    for(b in 1:length(corr_ward))
    {
      sub_set <- dplyr::filter(shapefile@data, Ward_Name == corr_ward[b])
      
      if(nrow(sub_set)>0)
      {
        Loc_ID[b] <- as.character(sub_set$Loc_ID)
        
      } else {
        Loc_ID[b] <- as.character(NA)
      }
    }
    
    corrections_II <- cbind(corrections_II, Loc_ID)
    corrections_II <- as.data.frame(corrections_II)
    
    #return(corrections_II)
    
  } else { ## If the "only_in_distr" argument is NA
    
    corrections_II <- corrections
    
    corr_ward <- corrections_II$correct_name
    corr_ward <- as.character(corr_ward)
    
    Loc_ID <- rep(NA, length(corr_ward))
    Loc_ID <- as.character(Loc_ID)
    
    if(higher_level=="District")
    {
      disname <- rep(NA, length(corr_ward))
      disname <- as.character(disname)
      
      for(b in 1:length(corr_ward))
      {
        sub_set <- dplyr::filter(shapefile@data, Ward_Name == corr_ward[b])
        
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
      
    } else { ## higher_level == Region
      
      regname <- rep(NA, length(corr_ward))
      regname <- as.character(regname)
      
      for(b in 1:length(corr_ward))
      {
        sub_set <- dplyr::filter(shapefile@data, Ward_Name == corr_ward[b])
        
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
      
    }
  }
  
  return(corrections_II)
  
}
