#' process_layer
#'
#' This function draws an layer with processes
#'
#' @param polygon four coordinates..
#' @param number  par variable e.g col
#' @param buffer value of variable e.g. blue
#' @param rotation angle of the pointgrid 
#' @param ... the "id" which defines the layer 
#' @return A unit sf layer with an parID column
#'
#' @export

process_layer <- function(polygon, number = 1, buffer = 1, rotation = 45, ...){
  #bbox from polygon...
  polygon <- matrix(c(polygon[1], polygon[2], polygon[3],
                      polygon[2], polygon[3], polygon[4], 
                      polygon[1], polygon[4], polygon[1], 
                      polygon[2]), ncol=2, byrow=TRUE)
  #build buffer
  polygon <- sf::st_sfc(sf::st_polygon(list(polygon)))
  polygon <- sf::st_buffer(polygon, -buffer)
  
  #union additional layers
  geom_list <- list(...)
  
  if(length(geom_list) >= 1){
    geom <-  sapply(geom_list, sf::st_union) 
    geom1 <- do.call(
      rbind, lapply(
        1:length(geom), function(i){
          sf::st_sf(geometry = sf::st_sfc(sf::st_intersection(polygon, geom[[i]]))) 
        }
      )
    ) 
    
    #if point make buffer:
    
    if(any(sf::st_geometry_type(geom1) == "POINT")){
      geom2 <- sf::st_collection_extract(geom1, "POINT") %>% 
        sf::st_buffer(buffer)
      geom3 <- sf::st_collection_extract(geom1, "POLYGON") %>% 
        sf::st_buffer(buffer) %>% 
        rbind(geom2)
    }else{
      geom3 <- sf::st_buffer(geom1, buffer)
    }
    
    #make the difference to check the free placec
    geom4 <- sf::st_difference(polygon, sf::st_union(geom3)) %>% 
      sf::st_cast("POLYGON") %>% 
      sf::st_sf(geometry = ., area = sf::st_area(.)) %>% 
      dplyr::filter_(~ area > 2)
    if(nrow(geom4)==0){
      return(NULL)
    }else{
      free_geom <- basic_regular_point(polygon, 
                                       cellnumber = c(number, number), 
                                       rotation = rotation) %>%
        sf::st_intersection(geom4) %>% 
        sf::st_union()
      return(free_geom)
    }
    
  }else{
    free_geom <- basic_regular_point(polygon, 
                                     cellnumber = c(number, number), 
                                     rotation = rotation) %>%
      sf::st_union()
    return(free_geom)
  }
  
  
}

