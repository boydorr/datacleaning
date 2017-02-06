#' @title Produce a table with all the misspellings and the respective corrections
#'
#' @description Create a dataframe with all the misspellings and the respective 
#' corrections used for processing the raw data. 
#'
#' @param processed_df the processed project dataframe with raw names and the respective corrections.
#' @param  processed_df_raw_col the name of the dataframe's column in which to look for the raw names. NOTE: The label must be specified as "label" (i.e. "village", for instance)
#' @param  processed_df_corr_col the name of the dataframe's column in which to look for the corrected names. NOTE: The label must be specified as "label" (i.e. "correct_village", for instance)
#'
#' @details \code{create_correction_df} create a dataframe that stores the unique 
#' misspellings and the respective correction used to process the data.   
#'  
#' @return a data frame with two columns. The first column contains the misspellings 
#' and the second column stores the respective correction.
#'
#' @export
#'
#' @examples
#' #
#'

create_correction_df <- function(processed_df, processed_df_raw_col, 
                                 processed_df_corr_col)
{
  raw_corr <- dplyr::select_(processed_df, processed_df_raw_col, processed_df_corr_col)
  
  raw_corr <- unique(raw_corr)
  
  raw_corr[,1] <- as.character(raw_corr[,1])
  raw_corr[,2] <- as.character(raw_corr[,2])
  
  index <- which(raw_corr[[processed_df_raw_col]] != raw_corr[[processed_df_corr_col]]
                 | is.na(raw_corr[[processed_df_raw_col]] != raw_corr[[processed_df_corr_col]]))
  
  raw_corr <- raw_corr[index,]
  
  return(raw_corr)
}
