#' @title Create the final location dataframe
#'
#' @description Takes as input the corrected dataframe produced by fill_names and 
#' the Location_ID_df to produce the final location dataframe.
#'   
#' @param corrected_df the corrected dataframe, output of the function fill_names
#' @param  corr_df_vill the name of the column in the corrected dataframe storing the village names. NOTE: The label must be specified as "label" (i.e. "Village", for instance)
#' @param  corr_df_ward the name of the column in the corrected dataframe storing the ward names. NOTE: The label must be specified as "label" (i.e. "Ward", for instance)
#' @param  corr_df_distr the name of the column in the corrected dataframe storing the district names. NOTE: The label must be specified as "label" (i.e. "District", for instance)
#' @param  corr_df_reg the name of the column in the corrected dataframe storing the region names. NOTE: The label must be specified as "label" (i.e. "Region", for instance)
#' @param  ID_df the Location_ID_df, which is available in the datacleaning package
#'  
#' @details \code{create_final_df} uses the CORRECT village/ward/district/region names
#' in the dataset and the Location ID dataframe to produce the final Location dataset 
#' that can be used for plotting. 
#'  
#' @return a new dataframe with 3 columns:   
#'    
#' i) Location: column storing the location names; 
#'   
#' ii) Location_Type: column storing the location type (i.e. Village/Ward/District/Region);  
#'   
#' iii) Loc_ID: the unique ID associated to each location  
#'    
#' @export
#'
#' @examples
#' #
#'

create_final_df <- function(corrected_df, corr_df_vill, corr_df_ward, corr_df_distr, 
                        corr_df_reg, ID_df)
  
{
  corrected_df <- unique(corrected_df)
  
  ## the number of observation in the project dataset
  ntot <- nrow(corrected_df)
  
  ID_df_vill <- "Vil_Mtaa_N"
  ID_df_ward <- "Ward_Name"
  ID_df_distr <- "District_N"
  ID_df_reg <- "Region_Nam"
  
  ID_df_vill_code <- "Village_ID"
  ID_df_ward_code <- "Ward_ID"
  ID_df_distr_code <- "District_ID"
  ID_df_reg_code <- "Region_ID"
  
  vill <- corrected_df[[corr_df_vill]]
  vill <- as.character(vill)
  vill_ID <- ID_df[[ID_df_vill]]
  vill_ID <- as.character(vill_ID)
  vill_ID_code <- ID_df[[ID_df_vill_code]]
  vill_ID_code <- as.character(vill_ID_code)
  ## number of villages
  vill_length <- length(vill)
  ## generate empty list for results
  mylist_villages <- vector('list', vill_length)
  
  ward <- corrected_df[[corr_df_ward]]
  ward <- as.character(ward)
  ward_ID <- ID_df[[ID_df_ward]]
  ward_ID <- as.character(ward_ID)
  ward_ID_code <- ID_df[[ID_df_ward_code]]
  ward_ID_code <- as.character(ward_ID_code)
  ## number of wards
  ward_length <- length(ward)
  ## generate empty list for results
  mylist_wards <- vector('list', ward_length)
  
  dis <- corrected_df[[corr_df_distr]]
  dis <- as.character(dis)
  dis_ID <- ID_df[[ID_df_distr]]
  dis_ID <- as.character(dis_ID)
  dis_ID_code <- ID_df[[ID_df_distr_code]]
  dis_ID_code <- as.character(dis_ID_code)
  ## number of districts
  dis_length <- length(dis)
  ## generate empty list for results
  mylist_districts <- vector('list', dis_length)
  
  reg <- corrected_df[[corr_df_reg]]
  reg <- as.character(reg)
  reg_ID <- ID_df[[ID_df_reg]]
  reg_ID <- as.character(reg_ID)
  reg_ID_code <- ID_df[[ID_df_reg_code]]
  reg_ID_code <- as.character(reg_ID_code)
  ## number of regions
  reg_length <- length(reg)
  ## generate empty list for results
  mylist_regions <- vector('list', reg_length)
  
  ## list to store the indices
  mylist_nums <- vector('list', ntot)
  
  # identify the relevant rows 
  for(i in 1:ntot)
  {
    # villages
    if(is.na(vill[i])==F)
    {
      # villages
      mylist_villages[[i]] <- which(vill_ID==vill[i])
    } else {
      mylist_villages[[i]] <- c(1:length(vill_ID))
    }
    
    # ward
    if(is.na(ward[i])==F)
    {
      mylist_wards[[i]] <- which(ward_ID==ward[i])
    } else {
      mylist_wards[[i]] <- c(1:length(ward_ID))
    }
    
    # districts
    if(is.na(dis[i])==F)
    {
      mylist_districts[[i]] <- which(dis_ID==dis[i])
    } else {
      mylist_districts[[i]] <- c(1:length(dis_ID))
    }
    
    # regions
    if(is.na(reg[i])==F)
    {
      mylist_regions[[i]] <- which(reg_ID==reg[i])
    } else {
      mylist_regions[[i]] <- c(1:length(reg_ID))
    }
    
    mylist_nums[[i]] <- Reduce(intersect, list(mylist_villages[[i]],
                                               mylist_wards[[i]],
                                               mylist_districts[[i]],
                                               mylist_regions[[i]]))
    
  }
  
  code_v <- rep(NA, ntot)
  code_w <- rep(NA, ntot)
  code_d <- rep(NA, ntot)
  code_r <- rep(NA, ntot)
  
  # VILLAGE
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
        if(is.na(vill[h])==T)
        {
          ## no chance of retrieving that info
          code_v[h] <- NA
          
        } else {
          ## otherwise, take note of the village name
          code_v[h] <- vill_ID_code[mylist_nums[[h]]]
        }
        
      } else {  ## if the intersection comprised more numbers:
        
        ## if there is no village info in the original dataset
        if(is.na(vill[h])==T)
        {
          ## no chance of retrieving that info
          code_v[h] <- NA
          
        } else { ## otherwise
          if(length(unique(vill_ID_code[mylist_nums[[h]]])==1)) ## if multiple numbers indicate the same code
          {
            ## take note of the village name
            code_v[h] <- unique(vill_ID_code[mylist_nums[[h]]])
            
          } else { ## if multiple numbers indicate different names
            code_v[h] <- NA
          }
        }
        
      }
    }
  }
  Location_Type_v <- c(rep("Village", vill_length))
  vill_df <- cbind(vill, Location_Type_v, code_v)
  vill_df <- as.data.frame(vill_df)
  colnames(vill_df)[1] <- "Location"
  colnames(vill_df)[2] <- "Location_Type"
  colnames(vill_df)[3] <- "Loc_ID"
  
  # WARD
  for(w in 1:ntot)
  {
    if(length(mylist_nums[[w]])==0)
    {
      next
      
    } else {
      
      ## if the intersection comprised only one number:
      if(length(mylist_nums[[w]])==1)
      {
        ## if there is no village info in the original dataset
        if(is.na(ward[w])==T)
        {
          ## no chance of retrieving that info
          code_w[w] <- NA
          
        } else {
          ## otherwise, take note of the village name
          code_w[w] <- ward_ID_code[mylist_nums[[w]]]
        }
        
      } else {  ## if the intersection comprised more numbers:
        
        ## if there is no village info in the original dataset
        if(is.na(ward[w])==T)
        {
          ## no chance of retrieving that info
          code_w[w] <- NA
          
        } else { ## otherwise
          if(length(unique(ward_ID_code[mylist_nums[[w]]])==1)) ## if multiple numbers indicate the same code
          {
            ## take note of the village name
            code_w[w] <- unique(ward_ID_code[mylist_nums[[w]]])
            
          } else { ## if multiple numbers indicate different names
            code_w[w] <- NA
          }
        }
        
      }
    }
  }
  Location_Type_w <- c(rep("Ward", ward_length))
  ward_df <- cbind(ward, Location_Type_w, code_w)
  ward_df <- as.data.frame(ward_df)
  colnames(ward_df)[1] <- "Location"
  colnames(ward_df)[2] <- "Location_Type"
  colnames(ward_df)[3] <- "Loc_ID"
  
  
  # DISTRICT
  for(d in 1:ntot)
  {
    if(length(mylist_nums[[d]])==0)
    {
      next
      
    } else {
      
      ## if the intersection comprised only one number:
      if(length(mylist_nums[[d]])==1)
      {
        ## if there is no village info in the original dataset
        if(is.na(dis[d])==T)
        {
          ## no chance of retrieving that info
          code_d[d] <- NA
          
        } else {
          ## otherwise, take note of the village name
          code_d[d] <- dis_ID_code[mylist_nums[[d]]]
        }
        
      } else {  ## if the intersection comprised more numbers:
        
        ## if there is no village info in the original dataset
        if(is.na(dis[d])==T)
        {
          ## no chance of retrieving that info
          code_d[d] <- NA
          
        } else { ## otherwise
          if(length(unique(dis_ID_code[mylist_nums[[d]]])==1)) ## if multiple numbers indicate the same code
          {
            ## take note of the village name
            code_d[d] <- unique(dis_ID_code[mylist_nums[[d]]])
            
          } else { ## if multiple numbers indicate different names
            code_d[d] <- NA
          }
        }
        
      }
    }
  }
  
  Location_Type_d <- c(rep("District", dis_length))
  dis_df <- cbind(dis, Location_Type_d, code_d)
  dis_df <- as.data.frame(dis_df)
  colnames(dis_df)[1] <- "Location"
  colnames(dis_df)[2] <- "Location_Type"
  colnames(dis_df)[3] <- "Loc_ID"
  
  
  # REGION
  for(r in 1:ntot)
  {
    if(length(mylist_nums[[r]])==0)
    {
      next
      
    } else {
      
      ## if the intersection comprised only one number:
      if(length(mylist_nums[[r]])==1)
      {
        ## if there is no village info in the original dataset
        if(is.na(reg[r])==T)
        {
          ## no chance of retrieving that info
          code_r[r] <- NA
          
        } else {
          ## otherwise, take note of the village name
          code_r[r] <- reg_ID_code[mylist_nums[[r]]]
        }
        
      } else {  ## if the intersection comprised more numbers:
        
        ## if there is no village info in the original dataset
        if(is.na(reg[r])==T)
        {
          ## no chance of retrieving that info
          code_r[r] <- NA
          
        } else { ## otherwise
          if(length(unique(reg_ID_code[mylist_nums[[r]]])==1)) ## if multiple numbers indicate the same code
          {
            ## take note of the village name
            code_r[r] <- unique(reg_ID_code[mylist_nums[[r]]])
            
          } else { ## if multiple numbers indicate different names
            code_r[r] <- NA
          }
        }
        
      }
    }
  }
  
  Location_Type_r <- c(rep("Region", reg_length))
  reg_df <- cbind(reg, Location_Type_r, code_r)
  reg_df <- as.data.frame(reg_df)
  colnames(reg_df)[1] <- "Location"
  colnames(reg_df)[2] <- "Location_Type"
  colnames(reg_df)[3] <- "Loc_ID"
  
  
  final_df <- plyr::rbind.fill(vill_df, ward_df, dis_df, reg_df)
  final_df <- as.data.frame(final_df)
  
  ## The final dataframe without the uninformative rows
  nocol <- c("Location_Type")
  nonums <- which(names(final_df) %in% nocol)
  final_df <- final_df[rowSums(!is.na(final_df[,-nonums])) > 0, ]
  
  return(final_df)
}
