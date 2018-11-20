#' Bv
#'
#' This function draws a Bv pattern, with the following  helper fuction \link[soilprofile2]{basic_line}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' 
#' @return A unit sf layer with an parID column
#'
#' @export

Bv <- function(polygon, ...){
  basic_line(polygon = polygon, cellnumber = 22, rotation = 12)
}
