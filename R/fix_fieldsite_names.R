#' @title Alter Field site name to standardised format
#'
#' @description Standardises fieldsite names to reduce risk of confusion.
#'
#' If a cell is empty, there is no effect. The column should be checked after
#' running the function to check for any which have been missed.
#'
#' @inheritParams fix_flow_type
#'
#' @return the original dataframe containing corrected fieldsite names
#'
#' @export

fix_fieldsite_names <- function(dataframe, column)
{
  # Save original class format
  oc <- class(dataframe[[column]])

  # Reformat as characters
  dataframe[[column]] <- as.character(dataframe[[column]])

  # Fix differences in names
  dataframe[[column]][dataframe[[column]] == "BUSHTOPS CAMP / KITALO"] <- "Bushtops Camp - Kitalo"
  dataframe[[column]][dataframe[[column]] == "BUSHTOPS CAMP"] <- "Bushtops Camp"
  dataframe[[column]][dataframe[[column]] == "Ft. Ikoma Bridge / Grumeti River"] <- "Fort Ikoma Bridge - Grumeti River"
  dataframe[[column]][dataframe[[column]] == "IKOMA GATE"] <- "Ikoma Gate"
  dataframe[[column]][dataframe[[column]] == "KIFARU CAMP (NDASIATA)"] <- "Kifaru camp (Ndasiata)"
  dataframe[[column]][dataframe[[column]] == "KIRAWIRA B RANGER POST"] <- "Kirawira B Ranger Post"
  dataframe[[column]][dataframe[[column]] == "LERAI FOREST / Ngare Nanyuki River"] <- "Lerai Forest - Ngare Nanyuki River"
  dataframe[[column]][dataframe[[column]] == "Makao boundary"] <- "Makao Boundary"
  dataframe[[column]][dataframe[[column]] == "MBUZI MAWE"] <- "Mbuzi Mawe"
  dataframe[[column]][dataframe[[column]] == "MORU RANGER POST"] <- "Moru Ranger Post"
  dataframe[[column]][dataframe[[column]] == "NAABI GATE"] <- "Naabi Gate"
  dataframe[[column]][dataframe[[column]] == "NAMIRI SPRING"] <- "Namiri Spring"
  dataframe[[column]][dataframe[[column]] == "NDUTU RANGER POST"] <- "Ndutu Ranger Post"
  dataframe[[column]][dataframe[[column]] == "Nyabogati Bridge/Hippo Pool"] <- "Nyabogati Bridge - Hippo Pool"
  dataframe[[column]][dataframe[[column]] == "OLDUVAI RANGER POST"] <- "Olduvai Ranger Post"
  dataframe[[column]][dataframe[[column]] == "OLDUVAI RIVER"] <- "Olduvai River"
  dataframe[[column]][dataframe[[column]] == "SIMIYU RANGER POST"] <- "Simiyu Ranger Post"
  dataframe[[column]][dataframe[[column]] == "SOIT LEMOTONYI/ ASILIA CAMP"] <- "Soit Lemotonyi - Asila Camp"
  dataframe[[column]][dataframe[[column]] == "SOIT LEMOTONYI"] <- "Soit Lemotonyi"
  dataframe[[column]][dataframe[[column]] == "TAWIRI RESEARCH STATION"] <- "Tawiri Research Station"
  dataframe[[column]][dataframe[[column]] == "NGORONGORO OLDUVAI RANGER POST"] <- "Ngorongoro Olduvai Ranger Post"

  return(dataframe)
}
