#' AhBv
#'
#' This function draws a AhBv pattern, with the following  helper fuctions: \link[soilprofile2]{basic_line}, \link[soilprofile2]{basic_random_polygon}, \link[soilprofile2]{basic_point}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' 
#' @return A unit sf layer with an parID column
#'
#' @export


AhBv <- function(polygon, ...){
  erg_line <- basic_line(polygon = polygon, cellnumber = 22, rotation = 12)
  erg_polygon <- basic_random_polygon(polygon = polygon, size = 0.5, number = 5, nSides =10, sm = T)
  erg_point <- basic_point(polygon = erg_polygon, cellsize = 4, random = F) %>% 
    st_intersection(erg_polygon)
  erg_list <- st_sf(par_ID = 1:3, geometry = c(st_geometry(erg_line), st_geometry(erg_polygon), 
                                               st_geometry(erg_point)))
  return(erg_list)
  
  
}
