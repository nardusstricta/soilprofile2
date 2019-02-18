#' \code{soiprofile2} package
#'
#' soiprofile2 R API
#' @importFrom tidyr separate gather
#' @importFrom aqp munsell2rgb
#' @importFrom dplyr %>%
#' @importFrom lwgeom st_split
#' @importFrom usethis use_data
#' @importFrom stats rgamma rnorm runif sd
#' @importFrom utils data
#' @importFrom methods as
#' @importFrom raster raster brick extent mask cut rasterToPolygons
#' @importFrom magick image_read image_write
#' @importFrom ggspatial layer_spatial

NULL
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))