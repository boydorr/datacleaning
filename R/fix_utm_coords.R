#' @title Remove incorrect UTM coordinates from a dataset
#'
#' @description Removes coordinates (in UTM format based within Tanzania) from a
#' dataframe and replaces them with NA. This enables onward use of other functions
#' within the \code{datacleaning} package without issue of mixed coordinate
#' formats or wrongly input numbers in raw data.
#'
#' @param dataframe a dataframe containing a column for x- coordinates and
#' y-coordinates in the UTM format.
#' @param x_label the column header belonging to x-UTM coordinates.
#' @param y_label the column header belonging to y-UTM coordinates.
#' @param remove.extra (Optional) if \code{TRUE}, either coordinate is replaced
#' with NA if the other coordinate exists as NA; this NA will be a result of an
#' original missing entry, or fixes done within the function to replace unusual
#' coordinates.Using \code{remove.extra=TRUE} enables onward use of other
#' coordinate functions within the \code{datacleaning} package.
#'
#' @return the original dataframe with fixes in place in the X- and y- coordinate
#' columns.
#'
#' @export
#'
#' @examples
#' # Load in data
#' data("example_utm_coordinates2")
#'
#' # View dataframe
#' example_utm_coordinates2
#'
#' # Run without remove.extra
#' fix_utm_coords(dataframe=example_utm_coordinates2, x_label="x_label", y_label="y_label")
#'
#' # Run with remove.extra
#' fix_utm_coords(dataframe=example_utm_coordinates2, x_label="x_label", y_label="y_label", remove.extra=TRUE)

fix_utm_coords <- function(dataframe, x_label, y_label, remove.extra=FALSE)
{
  # View numbers as such
  dataframe[[x_label]] <- as.numeric(as.character(dataframe[[x_label]]))
  dataframe[[y_label]] <- as.numeric(as.character(dataframe[[y_label]]))

  # Remove x-coordinate number if outside of range
  dataframe[[x_label]] <- lapply(dataframe[[x_label]],
                              function(x) ifelse(x<500000, "", x))
  dataframe[[x_label]] <- lapply(dataframe[[x_label]],
                              function(x) ifelse(x>1000000, "", x))

  # Remove y-coordinate number if outside of range
  dataframe[[y_label]] <- lapply(dataframe[[y_label]],
                            function(x) ifelse(x<6000000, "", x))
  dataframe[[y_label]] <- lapply(dataframe[[y_label]],
                            function(x) ifelse(x>11000000, "", x))

  # Replace empty cells with NA
  dataframe[[x_label]] <- sub("^$", NA, dataframe[[x_label]])
  dataframe[[y_label]] <- sub("^$", NA, dataframe[[y_label]])

  # Replace any handwritten "NA" cells with NA
  dataframe[[x_label]] <- sub("^NA$", NA, dataframe[[x_label]])
  dataframe[[y_label]] <- sub("^NA$", NA, dataframe[[y_label]])

  # If remove.extra=TRUE, when X or Y contains an empty cell, remove other too.
  if(remove.extra==TRUE)
  {
    # Isolate the coordinate columns
    df <- dataframe[c(x_label, y_label)]

    # Find which rows contain one or more NA values
    indx <- which(!complete.cases(df[[x_label]]) | !complete.cases(df[[y_label]]))

    # If at least one coordinate value=NA, make the the NA too.
    df[indx,] <- NA

    # Bind the altered columns back to the original dataframe
    dataframe[[x_label]] <- df[[x_label]]
    dataframe[[y_label]] <- df[[y_label]]
  }

  return(dataframe)
}
