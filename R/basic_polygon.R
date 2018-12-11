#' basic polygon 
#'
#' This function draws a polygon, with the following  helper fuction \link[sf]{st_make_grid}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' 
#' @return A unit polygon (sf) with an parID column
#'
#' @export

basic_polygon <- function(polygon, cellsize, square = F){
  sf::st_make_grid(polygon, what = "polygons", cellsize = cellsize,  square = square) %>% 
    sf::st_union()%>% 
    sf::st_sf(parID = 1)
}

  