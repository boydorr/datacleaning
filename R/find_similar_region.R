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
#' i) the region misspellings, stored in the first column ("misspelled_regions");
#'
#' ii) all the similar matches, each one stored under a separate column
#' ("Correct_Name_1", "Correct_Name_2", etc.);
#'
#' iii) a column specifying whether the misspelling is a real alphanumeric typo
#' or not ("real_typo);
#'
#' iv) a column specifying the level of the proposed correction ("correction_level");
#'
#' v) all the location IDs, for each proposed correction ("Loc_ID_1", Loc_ID_2, etc.).
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

  misspelled_regions <- check_df$misspelled_regions
  misspelled_regions <- as.character(misspelled_regions)

  corrections <- cbind(misspelled_regions, regions_correct_name)
  corrections <- as.data.frame(corrections)

  ## rename the column of corrected regions
  totcol <- length(corrections)

  for(q in 2:totcol)
  {
    newname <- paste("Correct_Name_", q-1, sep="")
    colnames(corrections)[q] <- newname
  }

  ## cbind the real_typo column
  real_typo <- as.character(real_typo)
  corrections <- cbind(corrections, real_typo)

  tot_obs <- nrow(corrections)
  correction_level <- c()
  correction_level <- as.character(correction_level)
  correction_level <- rep("Region", tot_obs)
  corrections <- cbind(corrections, correction_level)

  ##############################################################################
  ##############################################################################
  ##############################################################################
  corrections_II <- as.data.frame(regions_correct_name)
  totcol <- length(corrections_II)

  mylist_resp_code <- vector('list', totcol)

  for(w in 1:totcol)
  {
    corr_reg <- corrections_II[,w]
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

  corrections_III <- cbind(corrections, code_identified)
  corrections_III <- as.data.frame(corrections_III)

  return(corrections_III)

}
