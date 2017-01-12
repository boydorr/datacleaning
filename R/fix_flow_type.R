#' @title Alter water flow type to standardised format
#'
#' @description In datasets with a column stating Flow type of a water source
#' (Flowing/Pooling/Dry), different formats are often used (i.e. an initial,
#' full word, verb etc.). \code{fix_flow_type} replaces any existing format to
#' either Flow, Pool or Dry.
#'
#' If a cell is empty, there is no effect. The column should be checked after
#' running the function to check for any which have been missed.
#'
#' @param dataframe a dataframe containing a column stating flow type.
#' @param column the name of the column stating flow type.
#'
#' @return the original dataframe containing corrected flow type.
#'
#' @export

fix_flow_type <- function(dataframe, column)
{
  # Save original class format
  oc <- class(dataframe[[column]])

  # Reformat as characters
  dataframe[[column]] <- as.character(dataframe[[column]])

  # Fix differences in Flow formatting
  dataframe[[column]][dataframe[[column]] == "F"] <- "Flow"
  dataframe[[column]][dataframe[[column]] == "F "] <- "Flow"
  dataframe[[column]][dataframe[[column]] == "f"] <- "Flow"
  dataframe[[column]][dataframe[[column]] == "f "] <- "Flow"
  dataframe[[column]][dataframe[[column]] == "Flowing"] <- "Flow"
  dataframe[[column]][dataframe[[column]] == "Flowing "] <- "Flow"
  dataframe[[column]][dataframe[[column]] == "flowing"] <- "Flow"
  dataframe[[column]][dataframe[[column]] == "flowing "] <- "Flow"

  # Fix differences in Pool formatting
  dataframe[[column]][dataframe[[column]] == "P"] <- "Pool"
  dataframe[[column]][dataframe[[column]] == "P "] <- "Pool"
  dataframe[[column]][dataframe[[column]] == "p"] <- "Pool"
  dataframe[[column]][dataframe[[column]] == "p "] <- "Pool"
  dataframe[[column]][dataframe[[column]] == "Pooling"] <- "Pool"
  dataframe[[column]][dataframe[[column]] == "Pooling "] <- "Pool"
  dataframe[[column]][dataframe[[column]] == "pooling"] <- "Pool"
  dataframe[[column]][dataframe[[column]] == "pooling "] <- "Pool"

  # Fix differences in Dry formatting
  dataframe[[column]][dataframe[[column]] == "D"] <- "Dry"
  dataframe[[column]][dataframe[[column]] == "D "] <- "Dry"
  dataframe[[column]][dataframe[[column]] == "d"] <- "Dry"
  dataframe[[column]][dataframe[[column]] == "d "] <- "Dry"
  dataframe[[column]][dataframe[[column]] == "Dry"] <- "Dry"
  dataframe[[column]][dataframe[[column]] == "Dry "] <- "Dry"
  dataframe[[column]][dataframe[[column]] == "dry"] <- "Dry"
  dataframe[[column]][dataframe[[column]] == "dry "] <- "Dry"

  # Reformat as factors
  dataframe[[column]] <- as.factor(dataframe[[column]])

  return(dataframe)
}
