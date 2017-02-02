#' @title Find similar region names
#'
#' @description Identifies the misspelled region names in the project dataset
#' and proposes a set of similar correct region names.
#'
#' @param dataframe the project dataframe with misspelled region names
#' @param label the name of the column in the project dataframe storing the region names. NOTE: The label must be specified as "label" (i.e. "region", for instance)
#' @param distnc numeric value indicating the maximum distance allowed for a match (see ?agrep)
#' @param shapefile the shapefile object used to retrieve the legal names
#' @param additional_places (oOptional) a dataframe containing the additional names of the new districts (post 2012 census). Default = NA. NOTE: The district/region column of this dataframe must be called like the @data slot in the wards shapefile
#'
#' @details \code{find_similar_region} finds the misspellings in the region column
#' in the project dataset, looks for the respective most similar legal names and
#' proposes them as potential corrections to the typos. The typos dataframe produced
#' by this function can be used as argument of fix_typos.
#'
#' @return a new dataframe with:  
#'   
#' i) the region misspellings, stored in the first column ("misspelling");
#'   
#' ii) the similar matches, each one stored in a different row of the same column
#' ("correct_name"). The matches are ordered in the different rows according to their 
#' level of similarity with the misspelled region;           
#'    
#' iii) a column specifying whether the misspelling is a real alphanumeric typo
#' or not ("real_typo);   
#'          
#' iv) a column storing the level of similarity of the proposed corrections ("lev_sim");   
#'     
#' v) a column specifying, for each misspelling, the hierarchical rank of each 
#' proposed correction ("choice_number"), with choice_number=1 indicating the most 
#' strongely suggested correction;      
#'     
#' vi) a column specifying the level of the proposed correction ("correction_level");
#'  
#' vii) a column storing the location ID of each proposed correction ("Loc_ID").    
#'   
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
    shp_df <- dplyr::select(shapefile@data, Region_Nam)
    shp_df$Region_Nam <- as.character(shp_df$Region_Nam)
    legal_names <- c(shp_df$Region_Nam, additional_places$Region_Nam)
    fullset <- rbind(shp_df, additional_places)
    
  } else {
    legal_names <- shapefile@data$Region_Nam
    fullset <- dplyr::select(shapefile@data, Region_Nam)
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
      misspelled_regions[i] <- NA
    }
    else { # otherwise, take note of the misspelling
      
      no_matches <- as.character("Looking for similar results because there are no matches for the region: ")
      missp_reg <- as.character(rg[i])
      
      message(no_matches, missp_reg)
      
      ## the misspellings
      misspelled_regions[i] <- rg[i]
      
    }
  }
  
  ## remove the NAs (that are the correct region names)
  misspelled_regions <- misspelled_regions[!is.na(misspelled_regions)]
  
  ## the vector with unique misspelled regions
  misspelled_regions <- unique(misspelled_regions)
  misspelled_regions <- as.character(misspelled_regions)
  
  ## the misspelled region position in the dataframe
  miss_reg_index <- which((dataframe[[label]]) %in% misspelled_regions)
  
  if(length(miss_reg_index)==0)
  {
    stop("There are no misspellings")
    
  } else {
    
    ## the misspelled district names
    misspelled_regions <- ((dataframe[[label]])[miss_reg_index])
    misspelled_regions <- as.character(misspelled_regions)
    
    check_df <- cbind(misspelled_regions)
    check_df <- as.data.frame(check_df)
    check_df <- unique(check_df)
    
    check_df$misspelled_regions <- as.character(check_df$misspelled_regions)
    
    ## generate empty list for similar results
    Tot_misspell <- nrow(check_df)
    mylist_similar_regions <- vector('list', Tot_misspell)
    
    ## initialise the real_typo vector
    real_typo <- c(rep(NA, Tot_misspell))
    real_typo <- as.character(real_typo)
    
    ## only consider alphanumeric characters of the misspelled/correct district names.
    ## same names' indices
    m_d <- gsub("[^[:alnum:]]", "", tolower(check_df$misspelled_regions))
    c_d <- gsub("[^[:alnum:]]", "", tolower(rg_correct))
    
    for(q in 1:Tot_misspell)
    {
      
      ## find the CASE INSENSITIVE matches, only considering alphanumeric characters
      if((m_d[q] %in% c_d)==T)
      {
        real_typo[q] <- FALSE
        number <- which(c_d %in% m_d[q])
        mylist_similar_regions[[q]] <- number
        mylist_similar_regions[[q]] <- unique(rg_correct[mylist_similar_regions[[q]]])
      } else {
        real_typo[q] <- TRUE
        # if there are no case insensitive matches
        ##look for similar region names in the whole correct names list
        mylist_similar_regions[[q]] <- agrep(m_d[q], c_d, max.distance = distnc)
        mylist_similar_regions[[q]] <- unique(rg_correct[mylist_similar_regions[[q]]])
      }
      
    }
  }
  
  regions_identified <- plyr::ldply(mylist_similar_regions, rbind)
  regions_correct_name <- regions_identified
  
  n_obs <- rep(0, nrow(regions_correct_name))
  for(y in 1:nrow(regions_correct_name))
  {
    n_obs[y] <- sum(!is.na(regions_correct_name[y,]))
  }
  
  
  misspelled_regions <- check_df$misspelled_regions
  misspelled_regions <- as.character(misspelled_regions)
  
  missp <- as.character()
  misspell <- as.character()
  
  r_typo <- as.character()
  typo_real <- as.character()
  
  first.time=T
  
  for(t in 1:length(n_obs))
  {
    if(sum(n_obs[t])==0)
    {
      misspell <- misspelled_regions[t]
      typo_real <- real_typo[t]
      
    } else {
      misspell <- rep(misspelled_regions[t], n_obs[t])
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
  
  regions_identified_all <- plyr::ldply(mylist_similar_regions, cbind)
  
  corrections <- cbind(missp, regions_identified_all, r_typo)
  corrections <- as.data.frame(corrections)
  
  colnames(corrections)[1] <- "misspelling"
  colnames(corrections)[2] <- "correct_name"
  colnames(corrections)[3] <- "real_typo"
  
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
  correction_level <- rep("Region", tot_obs)
  corrections <- cbind(corrections, correction_level)
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  corrections_II <- corrections
  
  corr_reg <- corrections_II$correct_name
  corr_reg <- as.character(corr_reg)
  
  Loc_ID <- rep(NA, length(corr_reg))
  Loc_ID <- as.character(Loc_ID)
  
  for(b in 1:length(corr_reg))
  {
    sub_set <- dplyr::filter(shapefile@data, Region_Nam == corr_reg[b])
    
    if(nrow(sub_set)>0)
    {
      Loc_ID[b] <- as.character(sub_set$Loc_ID)
      
    } else {
      Loc_ID[b] <- as.character(NA)
    }
  }
  
  
  corrections_III <- cbind(corrections_II, Loc_ID)
  corrections_III <- as.data.frame(corrections_III)
  
  return(corrections_III)

}




