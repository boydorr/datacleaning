#' change_projection / Change the projection of a shapefile to LongLat WGS84
#'
#' Alters the projection of a shapefile to that of the entire project
#' (+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0).
#'
#' @param shapefile - a shapefile object loaded in the original projection.
#'
#' @return a shapefile object in the LongLat WGS84 projection.
#'
#' @export
#'
#' @examples
#' #

change_projection <- function(shapefile)
{
  # Set intended projection
  Geoproj <-
    sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Transform shapefile to intended projection
  proj.shapefile <- sp::spTransform(shapefile, Geoproj)

  return(proj.shapefile)
}
