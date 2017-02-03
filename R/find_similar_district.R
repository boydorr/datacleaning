#' @title Find similar district names
#'
#' @description Identifies the misspelled district names in the project dataset
#' and proposes a set of similar correct district names.
#'
#' @param dataframe the project dataframe with misspelled district names
#' @param  label the name of the column in the project dataframe storing the district names. NOTE: The label must be specified as "label" (i.e. "district", for instance)
#' @param  label_higher_level the name of the column in the project dataframe storing the CORRECT region names. NOTE: The label must be specified as "label" (i.e. "region", for instance)
#' @param  shapefile the shapefile object used to retrieve the legal names
#' @param  distnc numeric value indicating the maximum distance allowed for a match (see ?agrep)
#' @param  additional_places (optional) a dataframe containing the additional names of the new districts (post 2012 census). Default = NA. NOTE: The district/region column of this dataframe must be called like the @data slot in the wards shapefile
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
#' @return a new dataframe with:
#'
#' i) the district misspellings, stored in the first column ("misspelling");
#'
#' ii) the similar matches, each one stored in a different row of the same column
#' ("correct_name"). The matches are ordered in the different rows according to their 
#' level of similarity with the misspelled district;  
#'  
#' iii) a column specifying whether the misspelling is a real alphanumeric typo
#' or not ("real_typo);   
#'    
#' iv) a column storing the region information in the project dataframe associated
#' to the misspelled district ("region_in_df");   
#'    
#' v) a column storing the level of similarity of the proposed corrections ("lev_sim"); 
#'      
#' vi) a column specifying, for each misspelling, the hierarchical rank of each 
#' proposed correction ("choice_number"), with choice_number=1 indicating the most 
#' strongely suggested correction;     
#'     
#' vii) a column specifying the level of the proposed correction ("correction_level");
#'  
#' viii) a column specifying the region in the shapefile where each proposed 
#' correction is located ("correct_name_in_region");       
#'     
#' ix) a column storing the location ID of each proposed correction ("Loc_ID").    
#'    
#' @export
#'
#' @examples
#' #
#'

find_similar_district <- function(dataframe, label, label_higher_level,
                                  shapefile, distnc,
                                  additional_places=NA)
{
  
  if(!is.na(additional_places))
  {
    shp_df <- dplyr::select(shapefile@data, District_N, Region_Nam)
    shp_df$District_N <- as.character(shp_df$District_N)
    shp_df$Region_Nam <- as.character(shp_df$Region_Nam)
    legal_names <- c(shp_df$District_N, additional_places$District_N)
    fullset <- rbind(shp_df, additional_places)
    
  } else {
    legal_names <- shapefile@data$District_N
    fullset <- dplyr::select(shapefile@data, District_N, Region_Nam)
  }
  
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)
  
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
    } else { # otherwise, take note of the misspelling
      
      no_matches <- as.character("Looking for similar results because there are no matches for the district: ")
      missp_dis <- as.character(ds[i])
      
      message(no_matches, missp_dis)
      
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
    
    ## initialise the real_typo vector
    real_typo <- c(rep(NA, Tot_misspell))
    real_typo <- as.character(real_typo)
    
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
          real_typo[q] <- FALSE
          number <- which(c_sub_d %in% m_d[q])
          mylist_similar_districts[[q]] <- number
          mylist_similar_districts[[q]] <- unique(correct_subset_dis[mylist_similar_districts[[q]]])
        } else {
          real_typo[q] <- TRUE
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
          real_typo[q] <- FALSE
          number <- which(c_d %in% m_d[q])
          mylist_similar_districts[[q]] <- number
          mylist_similar_districts[[q]] <- unique(ds_correct[mylist_similar_districts[[q]]])
        } else {
          real_typo[q] <- TRUE
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
  
  n_obs <- rep(0, nrow(districts_correct_name))
  for(y in 1:nrow(districts_correct_name))
  {
    n_obs[y] <- sum(!is.na(districts_correct_name[y,]))
  }
  
  misspelled_districts <- check_df$respective_district
  misspelled_districts <- as.character(misspelled_districts)
  
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
      misspell <- misspelled_districts[t]
      typo_real <- real_typo[t]
      reg_df <- Region_in_df[t]
      
    } else {
      misspell <- rep(misspelled_districts[t], n_obs[t])
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
  
  for(u in 1:length(mylist_similar_districts))
  {
    if(length(mylist_similar_districts[[u]])==0)
    {
      mylist_similar_districts[[u]] <- NA
    } else {
      next
    }
  }
  
  districts_identified_all <- plyr::ldply(mylist_similar_districts, cbind)
  
  corrections <- cbind(missp, districts_identified_all, r_typo, df_reg)
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
  correction_level <- rep("District", tot_obs)
  corrections <- cbind(corrections, correction_level)
  
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  
  corrections_II <- corrections
  
  corr_dis <- corrections_II$correct_name
  corr_dis <- as.character(corr_dis)
  
  regname <- rep(NA, length(corr_dis))
  regname <- as.character(regname)
  
  Loc_ID <- rep(NA, length(corr_dis))
  Loc_ID <- as.character(Loc_ID)
  
  for(b in 1:length(corr_dis))
  {
    sub_set <- dplyr::filter(shapefile@data, District_N == corr_dis[b])
    
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
  
  return(corrections_II)
}




