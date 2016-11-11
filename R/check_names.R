#' check_names / Check that the names in the project dataset are correct 
#' 
#' Checks that the village/ward/district/region names in the project dataset are 
#' correct based on the legal village/ward/district/region names specified by the shapefile.
#'
#' @param dataframe - the project dataframe with the names that need to be checked
#' @param  label - the name of the column in the project dataframe storing the village/ward/district/region names that need to be checked. NOTE: The label must be specified as "label" (i.e. "village", for instance) 
#' @param  legal_names - a vector containing the legal village/ward/district/region names specified by the shapefile 
#' 
#' @details \code{check_names} checks that there are no misspellings in the project dataset 
#' by comparing the names stored in the dataset with the legal names contained in the 
#' shapefile
#' 
#' @return a list with two slots. The first slot contains a TRUE/FALSE statement: 
#' if TRUE, it means that there are no misspellings; if FALSE, there are still misspellings.
#' The second slot stores the misspelled names.
#' @export
#' 
#' @examples 
#' #
#' 

check_names <- function(dataframe, label, legal_names)
{
  df_name <- unique(dataframe[[label]])
  df_name <- df_name[!is.na(df_name)]
  df_name <- as.character(df_name)

  ## number of unique names (of the element specified by the label reg/dis/ward/vill) in the df
  df_name_length <- length(df_name)
  
  ## the correct names in the shapefile
  df_name_correct <- legal_names
  df_name_correct <- as.character(df_name_correct)
  
  ## initiate vector with the wrong names in the csv file
  misspelled_names <- c() 
  
  ## loop through the rows of the dataframe
  for(i in 1:df_name_length)
  { 
    ## if in the dataframe there is a correct region, do nothing
    if((df_name[i] %in% df_name_correct)==T) 
    { 
      print(paste("CORRECT name:", df_name[i]))
      misspelled_names[i] <- NA
    }
    else { # otherwise, it is a misspelling
      
      ## the misspelled_names
      misspelled_names[i] <- df_name[i]
    }
  }
  
  misspelled_names <- misspelled_names[!is.na(misspelled_names)]
  misspelled_names <- as.character(misspelled_names)
  
  t_or_f <- c()
  
  if((length(misspelled_names)==0)==T)
  {
    print("No misspellings")
    t_or_f <- T
    
  } else {
    print("There are still misspellings. Check the names before continuing the analysis.")
    t_or_f <- F
  }
  
  check_list <- vector('list', 2)
  check_list[[1]] <- t_or_f
  check_list[[2]] <- misspelled_names
  
  return(check_list)
}





