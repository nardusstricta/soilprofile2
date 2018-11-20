#' Ah
#'
#' This function draws a Ah pattern, with the following  helper fuction \link[soilprofile2]{basic_point}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' 
#' @return A unit sf layer with an parID column
#'
#' @export

Ah <- function(polygon, ...){
  tem_point <- basic_point(polygon = polygon, cellsize = 4, random = F)
  erg_list <- st_sf(par_ID = 1:2, geometry = c(st_geometry(polygon), st_geometry(tem_point)))
  return(erg_list)
}
