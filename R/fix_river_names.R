#' @title Alter River names to standardised format
#'
#' @description Standardises River names to reduce risk of confusion.
#'
#' If a cell is empty, there is no effect. The column should be checked after
#' running the function to check for any which have been missed.
#'
#' @inheritParams fix_flow_type
#' @param remove.unknown (Optional) Default is \code{TRUE}; if \code{TRUE}, all
#' unknowns in the dataset are replaced with NAs rather than stating Unknown.
#'
#' @return the original dataframe containing corrected fieldsite names
#'
#' @export

fix_river_names <- function(dataframe, column, remove.unknown=TRUE)
{
  # Reformat as characters
  dataframe[[column]] <- as.character(dataframe[[column]])

  # Fix names
  dataframe[[column]][dataframe[[column]] == "B/Olduvai"] <- "Big Olduvai"

  dataframe[[column]][dataframe[[column]] == "BANAGI"] <- "Banagi"
  dataframe[[column]][dataframe[[column]] == "Banagi River"] <- "Banagi"

  dataframe[[column]][dataframe[[column]] == "Barafu River"] <- "Barafu"

  dataframe[[column]][dataframe[[column]] == "BOLOGONJA"] <- "Bologonja"
  dataframe[[column]][dataframe[[column]] == "Bologonja River"] <- "Bologonja"
  dataframe[[column]][dataframe[[column]] == "BOLOGONJA RIVER"] <- "Bologonja"
  dataframe[[column]][dataframe[[column]] == "Bologonja SPR"] <- "Bologonja"

  dataframe[[column]][dataframe[[column]] == "Bonas loop river"] <- "Bonas Loop"

  dataframe[[column]][dataframe[[column]] == "Gay lands Drift"] <- "Gay Lands Drift"

  dataframe[[column]][dataframe[[column]] == "Goal "] <- "Goal"
  dataframe[[column]][dataframe[[column]] == "Goal River"] <- "Goal"

  dataframe[[column]][dataframe[[column]] == "GRUMETI"] <- "Grumeti"

  dataframe[[column]][dataframe[[column]] == "GRUMET-FORT"] <- "Fort Grumeti"
  dataframe[[column]][dataframe[[column]] == "Grumeti fort"] <- "Fort Grumeti"
  dataframe[[column]][dataframe[[column]] == "FORTGRUMETI"] <- "Fort Grumeti"
  dataframe[[column]][dataframe[[column]] == "Grumet-Fort"] <- "Fort Grumeti"
  dataframe[[column]][dataframe[[column]] == "Grumeti-Fot"] <- "Fort Grumeti"

  dataframe[[column]][dataframe[[column]] == "Grumeti KIRAWIRA"] <- "Grumeti Kirawira"
  dataframe[[column]][dataframe[[column]] == "Grumeti(kirawira)"] <- "Grumeti Kirawira"

  dataframe[[column]][dataframe[[column]] == "Grumeti-Ndasiata"] <- "Grumeti Ndasiata"
  dataframe[[column]][dataframe[[column]] == "Grumeti/ndasiata"] <- "Grumeti Ndasiata"

  dataframe[[column]][dataframe[[column]] == "Grumeti/Kifaru"] <- "Grumeti Kifaru"

  dataframe[[column]][dataframe[[column]] == "Grumet/ikoma"] <- "Grumeti Ikoma"

  dataframe[[column]][dataframe[[column]] == "IkomaGate)"] <- "Ikoma Gate"

  dataframe[[column]][dataframe[[column]] == "Kirawira (A)"] <- "Kirawira"

  dataframe[[column]][dataframe[[column]] == "KIROMBE"] <- "Kirombe"

  dataframe[[column]][dataframe[[column]] == "Kirombe/Klen,s"] <- "Kirombe Kleins"
  dataframe[[column]][dataframe[[column]] == "KIROMBE/KLEN,S"] <- "Kirombe Kleins"
  dataframe[[column]][dataframe[[column]] == "Kirombe Klens"] <- "Kirombe Kleins"

  dataframe[[column]][dataframe[[column]] == "KLEIN,S"] <- "Kleins"
  dataframe[[column]][dataframe[[column]] == "KLEINS"] <- "Kleins"
  dataframe[[column]][dataframe[[column]] == "Klen,s"] <- "Kleins"
  dataframe[[column]][dataframe[[column]] == "Klein's"] <- "Kleins"

  dataframe[[column]][dataframe[[column]] == "loligalani"] <- "Loligalani"
  dataframe[[column]][dataframe[[column]] == "LOLIGALANI"] <- "Loligalani"
  dataframe[[column]][dataframe[[column]] == "loyaligalani"] <- "Loligalani"
  dataframe[[column]][dataframe[[column]] == "Loyaligalani"] <- "Loligalani"
  dataframe[[column]][dataframe[[column]] == "Loyangalani"] <- "Loligalani"

  dataframe[[column]][dataframe[[column]] == "MAKUTANO"] <- "Makutano"

  dataframe[[column]][dataframe[[column]] == "MARA"] <- "Mara"
  dataframe[[column]][dataframe[[column]] == "Mara river"] <- "Mara"
  dataframe[[column]][dataframe[[column]] == "Mara River"] <- "Mara"
  dataframe[[column]][dataframe[[column]] == "MARA RIVER"] <- "Mara"

  dataframe[[column]][dataframe[[column]] == "MAREU"] <- "Mareu"
  dataframe[[column]][dataframe[[column]] == "Mareu River"] <- "Mareu"

  dataframe[[column]][dataframe[[column]] == "Mbalageti"] <- "Mbalageti"
  dataframe[[column]][dataframe[[column]] == "MBALAGETI"] <- "Mbalageti"
  dataframe[[column]][dataframe[[column]] == "Mbalageti River"] <- "Mbalageti"
  dataframe[[column]][dataframe[[column]] == "MBALEGETI"] <- "Mbalageti"
  dataframe[[column]][dataframe[[column]] == "MBALAGETI LODGE"] <- "Mbalageti"
  dataframe[[column]][dataframe[[column]] == "Mbalageti  "] <- "Mbalageti"

  dataframe[[column]][dataframe[[column]] == "Mokas"] <- "Mokasi"
  dataframe[[column]][dataframe[[column]] == "MOKASI"] <- "Mokasi"

  dataframe[[column]][dataframe[[column]] == "MORU"] <- "Moru"
  dataframe[[column]][dataframe[[column]] == "Moru River"] <- "Moru"

  dataframe[[column]][dataframe[[column]] == "Musabi River"] <- "Musabi"
  dataframe[[column]][dataframe[[column]] == "Musubi"] <- "Musabi"

  dataframe[[column]][dataframe[[column]] == "Nyahogati"] <- "Nyabogati"

  dataframe[[column]][dataframe[[column]] == "OLDUVAI"] <- "Olduvai"
  dataframe[[column]][dataframe[[column]] == "Olduvai River"] <- "Olduvai"
  dataframe[[column]][dataframe[[column]] == "Oluduvai"] <- "Olduvai"

  dataframe[[column]][dataframe[[column]] == "ORANGI"] <- "Orangi"

  dataframe[[column]][dataframe[[column]] == "SAND RIVER"] <- "Sand"
  dataframe[[column]][dataframe[[column]] == "Sandi River"] <- "Sand"
  dataframe[[column]][dataframe[[column]] == "SandRiver"] <- "Sand"
  dataframe[[column]][dataframe[[column]] == "SANDRIVER"] <- "Sand"
  dataframe[[column]][dataframe[[column]] == "Sand-River"] <- "Sand"
  dataframe[[column]][dataframe[[column]] == "Sand river - Kenya"] <- "Sand"

  dataframe[[column]][dataframe[[column]] == "Seronera River"] <- "Seronera"
  dataframe[[column]][dataframe[[column]] == "Seronere River"] <- "Seronera"
  dataframe[[column]][dataframe[[column]] == "Seronira"] <- "Seronera"

  dataframe[[column]][dataframe[[column]] == "simba"] <- "Simba"
  dataframe[[column]][dataframe[[column]] == "Simba "] <- "Simba"
  dataframe[[column]][dataframe[[column]] == "SIMBA"] <- "Simba"

  dataframe[[column]][dataframe[[column]] == "Simba-Dam"] <- "Simba Dam"

  dataframe[[column]][dataframe[[column]] == "SIMIYU"] <- "Simiyu"
  dataframe[[column]][dataframe[[column]] == "simiyu B"] <- "Simiyu"
  dataframe[[column]][dataframe[[column]] == "Simiyu B"] <- "Simiyu"
  dataframe[[column]][dataframe[[column]] == "SIMIYU B"] <- "Simiyu"
  dataframe[[column]][dataframe[[column]] == "Simiyi B"] <- "Simiyu"

  dataframe[[column]][dataframe[[column]] == "SMALL OLD"] <- "Small Olduvai"
  dataframe[[column]][dataframe[[column]] == "small Olduvai"] <- "Small Olduvai"
  dataframe[[column]][dataframe[[column]] == "Small olduvai"] <- "Small Olduvai"
  dataframe[[column]][dataframe[[column]] == "Small Olduvai River"] <- "Small Olduvai"
  dataframe[[column]][dataframe[[column]] == "S/Olduvai"] <- "Small Olduvai"

  dataframe[[column]][dataframe[[column]] == "WANDAM"] <- "Wandamu"
  dataframe[[column]][dataframe[[column]] == "wandamu"] <- "Wandamu"
  dataframe[[column]][dataframe[[column]] == "Wandam"] <- "Wandamu"
  dataframe[[column]][dataframe[[column]] == "Wandamu River"] <- "Wandamu"

  if(remove.unknown==TRUE){
    dataframe[[column]][dataframe[[column]] == "Unknown"] <- ""
  }

  return(dataframe)
}
