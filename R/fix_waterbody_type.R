#' @title Alter water body type to standardised format
#'
#' @description In datasets with a column stating the type of a water source
#' (Dam, River, Spring, Waterhole), different formats are often used (i.e. an
#' initial, full word, verb etc.). \code{fix_waterbody_type} replaces any
#' existing format to either Dam, River, Spring or Water Hole.
#'
#' If a cell is empty, there is no effect. The column should be checked after
#' running the function to check for any which have been missed.
#'
#' @inheritParams fix_flow_type
#'
#' @return the original dataframe containing corrected flow type.
#'
#' @export

fix_waterbody_type <- function(dataframe, column)
{
  # Save original class format
  oc <- class(dataframe[[column]])

  # Reformat as characters
  dataframe[[column]] <- as.character(dataframe[[column]])

  # Fix differences in Dam formatting
  dataframe[[column]][dataframe[[column]] == "d"] <- "Dam"
  dataframe[[column]][dataframe[[column]] == "d "] <- "Dam"
  dataframe[[column]][dataframe[[column]] == "D"] <- "Dam"
  dataframe[[column]][dataframe[[column]] == "D "] <- "Dam"
  dataframe[[column]][dataframe[[column]] == "dam"] <- "Dam"
  dataframe[[column]][dataframe[[column]] == "dam "] <- "Dam"
  dataframe[[column]][dataframe[[column]] == "DAM"] <- "Dam"
  dataframe[[column]][dataframe[[column]] == "DAM "] <- "Dam"

  # Fix differences in River formatting
  dataframe[[column]][dataframe[[column]] == "r"] <- "River"
  dataframe[[column]][dataframe[[column]] == "r "] <- "River"
  dataframe[[column]][dataframe[[column]] == "R"] <- "River"
  dataframe[[column]][dataframe[[column]] == "R "] <- "River"
  dataframe[[column]][dataframe[[column]] == "river"] <- "River"
  dataframe[[column]][dataframe[[column]] == "river "] <- "River"
  dataframe[[column]][dataframe[[column]] == "RIVER"] <- "River"
  dataframe[[column]][dataframe[[column]] == "RIVER "] <- "River"

  # Fix differences in Spring formatting
  dataframe[[column]][dataframe[[column]] == "s"] <- "Spring"
  dataframe[[column]][dataframe[[column]] == "s "] <- "Spring"
  dataframe[[column]][dataframe[[column]] == "S"] <- "Spring"
  dataframe[[column]][dataframe[[column]] == "S "] <- "Spring"
  dataframe[[column]][dataframe[[column]] == "SPR"] <- "Spring"
  dataframe[[column]][dataframe[[column]] == "SPR "] <- "Spring"
  dataframe[[column]][dataframe[[column]] == "spring"] <- "Spring"
  dataframe[[column]][dataframe[[column]] == "spring "] <- "Spring"
  dataframe[[column]][dataframe[[column]] == "SPRING"] <- "Spring"
  dataframe[[column]][dataframe[[column]] == "SPRING "] <- "Spring"

  # Fix differences in Spring formatting
  dataframe[[column]][dataframe[[column]] == "Wat.h"] <- "Water Hole"
  dataframe[[column]][dataframe[[column]] == "Wat.h "] <- "Water Hole"
  dataframe[[column]][dataframe[[column]] == "wh"] <- "Water Hole"
  dataframe[[column]][dataframe[[column]] == "wh "] <- "Water Hole"
  dataframe[[column]][dataframe[[column]] == "WH"] <- "Water Hole"
  dataframe[[column]][dataframe[[column]] == "WH "] <- "Water Hole"
  dataframe[[column]][dataframe[[column]] == "water hole"] <- "Water Hole"
  dataframe[[column]][dataframe[[column]] == "water hole "] <- "Water Hole"
  dataframe[[column]][dataframe[[column]] == "WATER HOLE"] <- "Water Hole"
  dataframe[[column]][dataframe[[column]] == "WATER HOLE "] <- "Water Hole"

  # Fix extra errors
  dataframe[[column]][dataframe[[column]] == "Bologonja"] <- ""

  return(dataframe)
}
