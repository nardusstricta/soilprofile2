#' build random texture 
#'
#' The actual task to build random pattern if no texture information and no specific horizont function is available and you still want to draw a texture. for example for black and white representations.
#' 
#' @param polygon polygon A Simple Feature or  just  a "sfc_MULTIPOLYGON" or "sfc_POLYGON" geometry
#' @param ... ignored
#' @return This function returns a new Simple File geometry 
#' @export


random_line_pattern <- function(polygon, ...){
  
  stopifnot("sf" %in% class(polygon) | 
              "sfc_MULTIPOLYGON" %in% class(polygon) |
              "sfc_POLYGON" %in% class(polygon))
  
  line_number <- round(rnorm(1, sf::st_bbox(polygon)["xmax"], 1))
  
  erg <- basic_line(polygon, cellnumber = c(1, line_number),
                    rotation = sample(-70:70, 1))
  return(erg)
}

#' build soil_texture 
#'
#' The actual task to build random pattern if no texture information and no specific horizont function is available and you still want to draw a texture. for example for black and white representations.
#' 
#' @param polygon polygon A Simple Feature or  just  a "sfc_MULTIPOLYGON" or "sfc_POLYGON" geometry.
#' @param ... information about texture size and varation. 
#' @return This function returns a new Simple File geometry 
#' @export

fun_grain_size <- function(polygon, ...){
  
  stopifnot("sf" %in% class(polygon) | 
              "sfc_MULTIPOLYGON" %in% class(polygon) |
              "sfc_POLYGON" %in% class(polygon))
  
  parms <- list(...)
  par_size <- parms[[1]]
  background <- parms[[2]]
  
  if(background != TRUE){
    
    x_number <-  round(sqrt(sf::st_area(polygon)) / par_size)
    y_number <-  round(sqrt(sf::st_area(polygon)) / par_size)
    
    erg <- basic_regular_point(polygon = polygon,
                               cellnumber =  c(x_number, y_number), 
                               rotation = 45) %>% 
      sf::st_cast("POINT") %>% 
      dplyr::mutate_(par_ID = ~ 1:nrow(.)) 
    
  }else{
    polygon1 <- sf::st_buffer(polygon, -1)
    
    x_number <-  round(sqrt(sf::st_area(polygon)) / par_size)
    y_number <-  round(sqrt(sf::st_area(polygon)) / par_size)

    
    tem_point <- basic_regular_point(polygon = polygon1,
                                     cellnumber =  c(x_number, y_number), 
                                     rotation = 45) %>% 
      sf::st_cast("POINT")
    erg <- sf::st_sf(par_ID = 1:(nrow(tem_point)+1),
                          geometry = c(sf::st_geometry(polygon), 
                                       sf::st_geometry(tem_point)))
  }

  return(erg)
  
}


