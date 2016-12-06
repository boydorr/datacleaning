#' @title Change the projection of a shapefile to LongLat WGS84
#'
#' @description \code{change_projection} alters the projection of a shapefile to
#' that of the entire project
#' (+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0).
#'
#' @param shapefile a shapefile object loaded in the original projection.
#'
#' @return a shapefile object in the LongLat WGS84 projection.
#'
#' @export
#'
#' @examples
#' # Load in shapefile in the UTM 36S projection
#' data("example_utm36S_shapefile")
#'
#' # Check projection
#' sp::proj4string(example_utm36S_shapefile)
#'
#' # Change projection
#' proj.shapefile <- change_projection(example_utm36S_shapefile)
#'
#' # Check projection has been changed to Geographic WGS84
#' sp::proj4string(proj.shapefile)

change_projection <- function(shapefile)
{
  # Set intended projection
  Geoproj <-
    sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Transform shapefile to intended projection
  proj.shapefile <- sp::spTransform(shapefile, Geoproj)

  return(proj.shapefile)
}
