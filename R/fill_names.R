#' @title Fill in missing place names
#'
#' @description Identifies the unique village/ward/district/region combination
#' and fills in the missing names where needed. Additionally, identifies the
#' mismatches (for instance, the cases in which the dataset has a correct
#' village name and a correct ward name, but that village is not in that ward).
#'
#' @param dataframe the project dataframe with the CORRECTLY SPELLED village, ward, district and region names
#' @param  df_vill the name of the column in the project dataframe storing the village names. NOTE: The label must be specified as "label" (i.e. "village", for instance)
#' @param  df_ward the name of the column in the project dataframe storing the ward names. NOTE: The label must be specified as "label" (i.e. "ward", for instance)
#' @param  df_distr the name of the column in the project dataframe storing the district names. NOTE: The label must be specified as "label" (i.e. "district", for instance)
#' @param  df_reg the name of the column in the project dataframe storing the region names. NOTE: The label must be specified as "label" (i.e. "region", for instance)
#' @param  legal_names_vill a vector containing the legal village names specified by the shapefile
#' @param  legal_names_ward a vector containing the legal ward names specified by the shapefile
#' @param  legal_names_distr a vector containing the legal district names specified by the shapefile
#' @param  legal_names_reg a vector containing the legal region names specified by the shapefile
#' @param  only_in_distr Optional. A character vector containing the names of the districts in which the study took place. Default = NA
#'
#' @details \code{fill_names} uses the CORRECT village/ward/district/region names
#' in the dataset and checks that there are no mismatches (i.e. that the specific village
#' is contained in the respective ward and so forth). If there are no mismatches,
#' it fills in the missing names if needed. If there is a mismatch, it fills the
#' mismatching village/ward/district/region row with NAs.
#'
#' @return a new dataframe with 4 columns, in which each column stores the matching
#' village, ward, district and region. The rows with all NAs are rows where there
#' is a mismatch. The rows with some NAs are the rows in which it was not possible
#' to retrieve the nformation for the village, ward, ... from the original dataset.
#'
#' @export
#'
#' @examples
#' #
#'

fill_names <- function(dataframe, df_vill, df_ward, df_distr, df_reg,
                       legal_names_vill, legal_names_ward,
                       legal_names_distr, legal_names_reg,
                       only_in_distr=NA)
{
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)

  vll <- dataframe[[df_vill]]
  vll <- as.character(vll)
  vll_correct <- legal_names_vill
  vll_correct <- as.character(vll_correct)

  wrd <- dataframe[[df_ward]]
  wrd <- as.character(wrd)
  wrd_correct <- legal_names_ward
  wrd_correct <- as.character(wrd_correct)

  ds <- dataframe[[df_distr]]
  ds <- as.character(ds)
  ds_correct <- legal_names_distr
  ds_correct <- as.character(ds_correct)

  rg <- dataframe[[df_reg]]
  rg <- as.character(rg)
  rg_correct <- legal_names_reg
  rg_correct <- as.character(rg_correct)

  ## number of villages
  vill_length <- length(vll)
  ## generate empty list for results
  mylist_villages <- vector('list', vill_length)

  ## number of wards
  ward_length <- length(wrd)
  ## generate empty list for results
  mylist_wards <- vector('list', ward_length)

  ## number of districts
  dis_length <- length(ds)
  ## generate empty list for results
  mylist_districts <- vector('list', dis_length)

  ## number of regions
  reg_length <- length(rg)
  ## generate empty list for results
  mylist_regions <- vector('list', reg_length)

  ## list to store the indices
  mylist_nums <- vector('list', ntot)

  ## list to store the names
  mylist_names <- vector('list', 4)

  ## write a loop to identify the rows of the shapefile to plot
  for(i in 1:ntot)
  {
    # villages
    if(is.na(vll[i])==F)
    {
      # villages
      mylist_villages[[i]] <- which(vll_correct==vll[i])
    } else {
      mylist_villages[[i]] <- c(1:length(vll_correct))
    }

    # ward
    if(is.na(wrd[i])==F)
    {
      mylist_wards[[i]] <- which(wrd_correct==wrd[i])
    } else {
      mylist_wards[[i]] <- c(1:length(wrd_correct))
    }

    # districts
    if (!is.na(only_in_distr))
    {
      mylist_districts[[i]] <- which(ds_correct %in% only_in_distr)
    } else {
      if(is.na(ds[i])==F)
      {
        mylist_districts[[i]] <- which(ds_correct==ds[i])
      } else {
        mylist_districts[[i]] <- c(1:length(ds_correct))
      }
    }

    # regions
    if(is.na(rg[i])==F)
    {
      mylist_regions[[i]] <- which(rg_correct==rg[i])
    } else {
      mylist_regions[[i]] <- c(1:length(rg_correct))
    }

    mylist_nums[[i]] <- Reduce(intersect, list(mylist_villages[[i]],
                                               mylist_wards[[i]],
                                               mylist_districts[[i]],
                                               mylist_regions[[i]]))

  }

  Updated_Villages <- rep(NA, ntot)
  Updated_Wards <- rep(NA, ntot)
  Updated_Districts <- rep(NA, ntot)
  Updated_Regions <- rep(NA, ntot)

  for(h in 1:ntot)
  {
    if(length(mylist_nums[[h]])==0)
    {
      next

    } else {

      ## if the intersection comprised only one number:
      if(length(mylist_nums[[h]])==1)
      {

        ## if there is no village info in the original dataset
        if(is.na(vll[h])==T)
        {
          ## no chance of retrieving that info
          Updated_Villages[h] <- NA
        } else {
          ## otherwise, take note of the village name
          Updated_Villages[h] <- vll_correct[mylist_nums[[h]]]
        }

        ## if there is no ward info in the original dataset and no village info
        if(is.na(wrd[h])==T && is.na(vll[h])==T)
        {
          ## there is no chance of retrieving that info
          Updated_Wards[h] <- NA
        } else {
          ## otherwise, take note of the ward name
          Updated_Wards[h] <- wrd_correct[mylist_nums[[h]]]
        }

        ## if there is no district info in the original dataset, no ward and no village info
        if(is.na(ds[h])==T && is.na(wrd[h])==T && is.na(vll[h])==T)
        {
          ## there is no chance of retrieving that info
          Updated_Districts[h] <- NA
        } else {
          ## otherwise, take note of the district name
          Updated_Districts[h] <- ds_correct[mylist_nums[[h]]]
        }

        ## if there is no region info in the original dataset and no district, no ward and no village info
        if(is.na(rg[h])==T && is.na(ds[h])==T && is.na(wrd[h])==T && is.na(vll[h])==T)
        {
          ## there is no chance of retrieving that info
          Updated_Regions[h] <- NA
        } else {
          ## otherwise, take note of the region name
          Updated_Regions[h] <- rg_correct[mylist_nums[[h]]]
        }


      } else {  ## if the intersection comprised more numbers:

        ## if there is no village info in the original dataset
        if(is.na(vll[h])==T)
        {
          ## no chance of retrieving that info
          Updated_Villages[h] <- NA

        } else { ## otherwise
          if(length(unique(vll_correct[mylist_nums[[h]]])==1)) ## if multiple numbers indicate the same name
          {
            ## take note of the village name
            Updated_Villages[h] <- unique(vll_correct[mylist_nums[[h]]])

          } else { ## if multiple numbers indicate different names
            Updated_Villages[h] <- NA
          }
        }


        ## if there is no ward info in the original dataset and no village info
        if(is.na(wrd[h])==T && is.na(vll[h])==T)
        {
          ## no chance of retrieving that info
          Updated_Wards[h] <- NA

        } else { ## otherwise
          if(length(unique(wrd_correct[mylist_nums[[h]]])==1)) ## if multiple numbers indicate the same name
          {
            ## take note of the ward name
            Updated_Wards[h] <- unique(wrd_correct[mylist_nums[[h]]])

          } else { ## if multiple numbers indicate different names
            Updated_Wards[h] <- NA
          }
        }


        ## if there is no district info in the original dataset, no ward and no village info
        if(is.na(ds[h])==T && is.na(wrd[h])==T && is.na(vll[h])==T)
        {
          ## there is no chance of retrieving that info
          Updated_Districts[h] <- NA
        } else {
          if(length(unique(ds_correct[mylist_nums[[h]]])==1))
          {
            Updated_Districts[h] <- unique(ds_correct[mylist_nums[[h]]])
          } else {
            Updated_Districts[h] <- NA
          }
        }


        ## if there is no region info in the original dataset and no district, no ward and no village info
        if(is.na(rg[h])==T && is.na(ds[h])==T && is.na(wrd[h])==T && is.na(vll[h])==T)
        {
          ## there is no chance of retrieving that info
          Updated_Regions[h] <- NA
        } else {
          if(length(unique(rg_correct[mylist_nums[[h]]])==1))
          {
            Updated_Regions[h] <- unique(rg_correct[mylist_nums[[h]]])
          } else {
            Updated_Regions[h] <- NA
          }
        }
      }
    }
  }

  Updated_Villages <- as.character(Updated_Villages)
  Updated_Wards <- as.character(Updated_Wards)
  Updated_Districts <- as.character(Updated_Districts)
  Updated_Regions <- as.character(Updated_Regions)

  updated_df <- cbind(Updated_Villages, Updated_Wards,
                      Updated_Districts, Updated_Regions)

  updated_df <- as.data.frame(updated_df)

  return(updated_df)
}

