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
#' # Load in shapefile which is in the UTM 36S projection
#' data("example_utm36S_shapefile")
#'
#' # Check projection
#' sp::proj4string(example_utm36S_shapefile)
#'
#' # Change projection to Geographic WGS84
#' proj.shapefile <- change_projection(example_utm36S_shapefile)
#'
#' # Check projection has been changed to Geographic WGS84
#' sp::proj4string(proj.shapefile)

change_projection <- function(shapefile)
{
  # Set intended projection
  Geoproj <-
    sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Check projection of shapefile
  projection <- sp::proj4string(shapefile)

  if(!projection=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  {
    print("Shapefile Tranformed")
    # Transform shapefile to intended projection
    proj.shapefile <- sp::spTransform(shapefile, Geoproj)
  } else {
    print("Shapefile already in correct projection")
    proj.shapefile <- shapefile
  }

  return(proj.shapefile)
}
