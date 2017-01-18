#' @title Correct any location typos in the project dataset
#'
#' @description Corrects any misspelled village/ward/district/region names in
#' the project dataset based on an existing dataframe containing the typos and
#' the respective corrections.
#'
#' @param dataframe the project dataframe with misspelled names to correct
#' @param  typo_df the typos dataframe containing all the typos and their respective corrections
#' @param  dataframe_label the name of the dataframe's column in which to look for the misspellings. NOTE: The label must be specified as "label" (i.e. "village", for instance)
#' @param  typo_label_wrong the name of the typos dataframe's column with the misspellings. NOTE: The label must be specified as "label"
#' @param  typo_label_correct the name of the typos dataframe's column with the respective corrections. NOTE: The label must be specified as "label"
#' @param  legal_names a vector containing the legal village/ward/district/region names specified by the shapefile
#' @param  mixed_typo (default = FALSE). logical, TRUE/FALSE. Specifies whether the correction_level in the typos dataframe is only Village or both Village and Ward
#'
#' @details \code{fix_typos} corrects the misspellings in a given column - specified by
#' the argument dataframe_label - of the project dataset based on a typos dataframe
#' of the same type as the one returned by find_similar_village, find_similar_ward, etc.etc.
#'
#' @return new data frame where all the original information is preserved and the
#' correct village/ward/district/region names are stored under a new column
#'
#' @export
#'
#' @examples
#' #
#'

fix_typos <- function(dataframe, typo_df, dataframe_label, typo_label_wrong,
                      typo_label_correct, legal_names, mixed_typo=F)
{
  ## the number of observation in the project dataset
  ntot <- nrow(dataframe)

  df_name <- dataframe[[dataframe_label]]
  df_name <- as.character(df_name)

  if(mixed_typo==F)
  {
    name_correct <- typo_df[[typo_label_correct]]
    name_correct <- as.character(name_correct)
    name_typo <- typo_df[[typo_label_wrong]]
    name_typo <- as.character(name_typo)

    ## initialise a column with the right names
    Correct <- rep(NA, ntot)
    Correct <- as.character(Correct)

    ## loop through the rows of the dataframe
    for (i in 1:ntot)
    {
      if (df_name[i] %in% name_typo)
      { # if the name in the dataframe is also in the typos dataframe
        ## find the row number of the typo in the typo dataframe
        typo_index <- which(name_typo %in% df_name[i])
        Correct[i] <- name_correct[typo_index]
      } else { # if in the dataframe there is a legal name, copy it
        if (df_name[i] %in% legal_names)
        {
          Correct[i] <- df_name[i]
        } else {
          Correct[i] <- NA
        }
      }
    }

    ## append the correct region names to the dataframe as a new column
    new_df <- cbind(dataframe, Correct)

    ## the right name for the column
    rightname <- paste("Correct_", dataframe_label, sep="")

    ## rename the column
    names(new_df)[names(new_df)=="Correct"] <- rightname
    return(new_df)

  } else { ## if mixed typo == T

    typo_df_v <- dplyr::filter(typo_df, correction_level=="Village")
    typo_df_w <- dplyr::filter(typo_df, correction_level=="Ward")

    name_correct_v <- typo_df_v[[typo_label_correct]]
    name_correct_v <- as.character(name_correct_v)

    name_correct_w <- typo_df_w[[typo_label_correct]]
    name_correct_w <- as.character(name_correct_w)

    name_typo_v <- typo_df_v[[typo_label_wrong]]
    name_typo_v <- as.character(name_typo_v)

    name_typo_w <- typo_df_w[[typo_label_wrong]]
    name_typo_w <- as.character(name_typo_w)

    ## initialise a column with the right names
    Correct_v <- rep(NA, ntot)
    Correct_v <- as.character(Correct_v)

    ## initialise a column with the right names
    Correct_w <- rep(NA, ntot)
    Correct_w <- as.character(Correct_w)

    # VILLAGE
    ## loop through the rows of the dataframe
    for (i in 1:ntot)
    {
      if (df_name[i] %in% name_typo_v)
      { # if the name in the dataframe is also in the typos dataframe
        ## find the row number of the typo in the typo dataframe
        typo_index <- which(name_typo_v %in% df_name[i])
        Correct_v[i] <- name_correct_v[typo_index]
      } else { # if in the dataframe there is a legal name, copy it
        if (df_name[i] %in% legal_names)
        {
          Correct_v[i] <- df_name[i]
        } else {
          Correct_v[i] <- NA
        }
      }
    }

    # Ward
    ## loop through the rows of the dataframe
    for (i in 1:ntot)
    {
      if (df_name[i] %in% name_typo_w)
      { # if the name in the dataframe is also in the typos dataframe
        ## find the row number of the typo in the typo dataframe
        typo_index <- which(name_typo_w %in% df_name[i])
        Correct_w[i] <- name_correct_w[typo_index]
      } else { # if in the dataframe there is a legal name, copy it
        if (df_name[i] %in% legal_names)
        {
          Correct_w[i] <- NA
        } else {
          Correct_w[i] <- NA
        }
      }
    }

    Correct_Village <- Correct_v
    Correct_Ward <- Correct_w

    ## append the correct region names to the dataframe as a new column
    new_df <- cbind(dataframe, Correct_Village)

    if(length(which(colnames(new_df)=="Correct_Ward"))==0)
    {
      new_df <- cbind(dataframe, Correct_Village, Correct_Ward)

    } else {
      new_df <- cbind(dataframe, Correct_Village, Correct_w)

      new_df$Correct_Ward <- as.character(new_df$Correct_Ward)
      new_df$Correct_w <- as.character(new_df$Correct_w)

      index <- which(!is.na(new_df$Correct_w))

      new_df$Correct_Ward[index] <- new_df$Correct_w[index]

      new_df$Correct_w <- NULL
    }

    return(new_df)
  }
}
