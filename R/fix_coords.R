#' @title Remove incorrect coordinates from a dataset
#'
#' @description Removes coordinates based within Tanzania from a dataframe and
#' replaces them with NA. This enables onward use of other functions within the
#' \code{datacleaning} package without issue of mixed coordinate formats or
#' wrongly input numbers in raw data.
#'
#' @param dataframe a dataframe containing a column for x- coordinates and
#' y-coordinates.
#' @param x_label the column header belonging to x-coordinates.
#' @param y_label the column header belonging to y-coordinates.
#' @param coord_type the type of coordinates to be changed; takes "UTM" by
#' default, but will accept "Geo-WGS84" as an alternative argument to alter
#' coordinates in the Geographic WGS84 projection.
#' @param remove.extra (Optional) this is TRUE by default; when TRUE, either
#' coordinate is replaced with NA if the other coordinate exists as NA. Using
#' \code{remove.extra=TRUE} enables onward use of other coordinate functions
#' within the \code{datacleaning} package.
#'
#' @return the original dataframe with fixes in place in the X- and y- coordinate
#' columns.
#'
#' @export
#'
#' @examples
#' # WITH UTM COORDINATES
#' data("example_utm_coordinates2")
#'
#' # View dataframe
#' example_utm_coordinates2
#'
#' # Run with remove.extra
#' fix_coords(dataframe=example_utm_coordinates2, x_label="x_label", y_label="y_label", coord_type="UTM")
#'
#' # Run without remove.extra
#' fix_coords(dataframe=example_utm_coordinates2, x_label="x_label", y_label="y_label", coord_type="UTM", remove.extra=FALSE)
#'
#' # WITH GEOGRAPHIC WGS84 COORDINATES
#' data("example_geo_coordinates")
#'
#' # View dataframe
#' example_geo_coordinates
#'
#' # Run with remove.extra
#' fix_coords(dataframe=example_geo_coordinates, x_label="x_label", y_label="y_label", coord_type="Geo-WGS84")
#'
#' # Run without remove.extra
#' fix_coords(dataframe=example_geo_coordinates, x_label="x_label", y_label="y_label", coord_type="Geo-WGS84", remove.extra=FALSE)

fix_coords <- function(dataframe, x_label, y_label, coord_type="UTM",
                       remove.extra=TRUE)
{
  # View numbers as such
  dataframe[[x_label]] <- as.numeric(as.character(dataframe[[x_label]]))
  dataframe[[y_label]] <- as.numeric(as.character(dataframe[[y_label]]))

  if(coord_type=="UTM"){
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
  }

  if(coord_type=="Geo-WGS84"){
    # Remove x-coordinate number if outside of range
    dataframe[[x_label]] <- lapply(dataframe[[x_label]],
                                   function(x) ifelse(x<=29.0, "", x))
    dataframe[[x_label]] <- lapply(dataframe[[x_label]],
                                   function(x) ifelse(x>=42.0, "", x))

    # Remove y-coordinate number if outside of range
    dataframe[[y_label]] <- lapply(dataframe[[y_label]],
                                   function(x) ifelse(x<=-13.0, "", x))
    dataframe[[y_label]] <- lapply(dataframe[[y_label]],
                                   function(x) ifelse(x>=0, "", x))
  }

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
