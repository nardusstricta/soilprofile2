#' basic random polygon 
#'
#' This function draws a set of randompolygons.
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param size Percentage area of input polygon
#' @param number Number of polygons
#' @param nSides number of polygon sides
#' @param sm smooth by the \link[smoothr]{smooth} function with the methode "ksmooth"
#' 
#' @return A unit multipolygon (sf) with one parID column
#'
#' @export

basic_random_polygon <- function(polygon, size, number, nSides, sm = T){
  area <- sf::st_area(polygon) * size/number
  point_samp <- sf::st_sample(polygon, number)
  
  var <-  point_samp %>% 
    sf::st_coordinates() %>% 
    as.data.frame()
  par_df <- data.frame(
    nSides = nSides,
    area = area, 
    xstart = var$X, 
    ystart = var$Y
  )
  
  for(i in 1 : nrow(var)){
    point_samp[i] <- convex_poly(par_df[i,])
  }
  
  erg <- point_samp %>% 
    sf::st_sf(parID = 1) %>% 
    dplyr::summarise(do_union = F) %>%
    sf::st_buffer(0.0)  %>% 
    sf::st_intersection(polygon) %>% 
    sf::st_union() 
  
  
  if(sm == T){
    erg  <-  smoothr::smooth(erg, method = "ksmooth")
  }
  
  return(erg)
  
}
