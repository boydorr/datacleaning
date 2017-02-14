#' @title Produce a table with all the misspellings and the respective corrections
#'
#' @description Create a dataframe with all the misspellings and the respective 
#' corrections used for processing the raw data. 
#'
#' @param processed_df the processed project dataframe with raw names and the respective corrections.
#' @param  processed_df_raw_col the name of the dataframe's column in which to look for the raw names. NOTE: The label must be specified as "label" (i.e. "village", for instance)
#' @param  processed_df_corr_col the name of the dataframe's column in which to look for the corrected names. NOTE: The label must be specified as "label" (i.e. "correct_village", for instance)
#' @param  extra_cols optional, default is FALSE. If TRUE, the function will add extra columns (storing the additional ward/district/region information) to the resulting dataframe
#' @param  reg_col optional, default == NA. It is only sensible when extra_cols==T. The name of the dataframe's column in which to look for the region information. NOTE: The label must be specified as "label" (i.e. "correct_region", for instance)
#' @param  dis_col optional, default == NA. It is only sensible when extra_cols==T. The name of the dataframe's column in which to look for the district information. NOTE: The label must be specified as "label" (i.e. "correct_district", for instance)
#' @param  ward_col optional, default == NA. It is only sensible when extra_cols==T. The name of the dataframe's column in which to look for the ward information. NOTE: The label must be specified as "label" (i.e. "correct_ward", for instance)
#'
#' @details \code{create_correction_df} create a dataframe that stores the unique 
#' misspellings and the respective correction used to process the data.   
#'  
#' @return If extra_cols=F, a data frame with two columns. The first column contains the misspellings 
#' and the second column stores the respective correction. If extra_cols=T, the function 
#' will add the extra columns containing the additional ward/district/region information.
#'
#' @export
#'
#' @examples
#' #
#'

create_correction_df <- function(processed_df, processed_df_raw_col, 
                                 processed_df_corr_col, extra_cols=F, 
                                 reg_col=NA, dis_col=NA, ward_col=NA)
{
  if(extra_cols==F)
  {
    raw_corr <- dplyr::select_(processed_df, processed_df_raw_col, processed_df_corr_col)

  } else {
    
    if(is.na(reg_col))
    {
      NA_Region <- rep(NA, nrow(processed_df))
      NA_Region <- as.character(NA_Region)
      processed_df$NA_Region <- NA_Region 
      reg_col <- "NA_Region"
    } else {
      reg_col <- reg_col
    }
    
    if(is.na(dis_col))
    {
      NA_District <- rep(NA, nrow(processed_df))
      NA_District <- as.character(NA_District)
      processed_df$NA_District <- NA_District 
      dis_col <- "NA_District"
      
    } else {
      dis_col <- dis_col
    }
    
    if(is.na(ward_col))
    {
      NA_Ward <- rep(NA, nrow(processed_df))
      NA_Ward <- as.character(NA_Ward)
      processed_df$NA_Ward <- NA_Ward 
      ward_col <- "NA_Ward"
      
    } else {
      ward_col <- ward_col
    }
    
    raw_corr <- dplyr::select_(processed_df, processed_df_raw_col, processed_df_corr_col,
                               reg_col, dis_col, ward_col)
    
    allcol <- as.character(colnames(raw_corr[3:ncol(raw_corr)]))

    for(i in 1:length(allcol))
    {
      ## if an additional column is only populated by NAs, remove it
      if(sum(as.numeric(is.na(raw_corr[[allcol[i]]])))==nrow(raw_corr))
      {
        raw_corr[[allcol[i]]] <- NULL
        
      } else {
        raw_corr <- raw_corr
      }
    }
  }
  
  raw_corr <- unique(raw_corr)
  
  raw_corr[,1] <- as.character(raw_corr[,1])
  raw_corr[,2] <- as.character(raw_corr[,2])
  
  index <- which(raw_corr[[processed_df_raw_col]] != raw_corr[[processed_df_corr_col]]
                 | is.na(raw_corr[[processed_df_raw_col]] != raw_corr[[processed_df_corr_col]]))
  
  raw_corr <- raw_corr[index,]
  
  return(raw_corr)
}
