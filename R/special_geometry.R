#' Bv
#'
#' This function draws a Ah pattern, with the following  helper fuction \link[soilprofile2]{basic_regular_point}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param ... ignored
#' @return A unit sf layer with an parID column
#'
#' @export

Bv <- function(polygon, ...){
  tem_point <- basic_regular_point(polygon = sf::st_buffer(polygon, -1), 
                                   cellnumber = c(7, 7), rotation = 140)
  erg_list <- sf::st_sf(par_ID = 1:2,
                        geometry = c(sf::st_geometry(polygon), 
                                     sf::st_geometry(tem_point)))
  return(erg_list)
}

#' AhBv
#'
#' This function draws a AhBv pattern, with the following  helper fuctions: \link[soilprofile2]{basic_line}, \link[soilprofile2]{basic_random_polygon}, \link[soilprofile2]{basic_regular_point}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param ... ignored
#' @return A unit sf layer with an parID column
#'
#' @export


AhBv <- function(polygon, ...){
  erg_line <- basic_line(polygon = polygon, cellnumber = c(1, 10), rotation = 45)
  erg_polygon <- basic_random_polygon(polygon = polygon,
                                      size = 0.5,
                                      number = 5,
                                      nSides =10,
                                      sm = T)
  
  erg_point <- basic_regular_point(polygon = sf::st_buffer(erg_polygon, -1), 
                                   cellnumber = c(15, 10), rotation = 145)
  
  erg_list <- sf::st_sf(par_ID = 1:3, geometry = c(sf::st_geometry(erg_line),
                                                   sf::st_geometry(erg_polygon), 
                                                   sf::st_geometry(erg_point))
                        )
  return(erg_list)
  
  
}

#' Ah
#'
#' This function draws a Bv pattern, with the following  helper fuction \link[soilprofile2]{basic_line}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param ... ignored
#' 
#' @return A unit sf layer with an parID column
#'
#' @export

Ah <- function(polygon, ...){
  basic_line(polygon = polygon, cellnumber = c(1, 15), rotation = 20)
}


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
    
    x_number <-  sqrt(sf::st_area(polygon)) / par_size
    y_number <-  sqrt(sf::st_area(polygon)) / par_size
    
    erg <- basic_regular_point(polygon = polygon,
                               cellnumber =  c(x_number, y_number), 
                               rotation = sample(-70:70, 1)) %>% 
      sf::st_cast("POINT") %>% 
      dplyr::mutate_(par_ID = ~ 1:nrow(.)) 
    
  }else{
    polygon1 <- sf::st_buffer(polygon, -1)
    
    x_number <-  sqrt(sf::st_area(polygon)) / par_size
    y_number <-  sqrt(sf::st_area(polygon)) / par_size

    
    tem_point <- basic_regular_point(polygon = polygon1,
                                     cellnumber =  c(x_number, y_number), 
                                     rotation = sample(-70:70, 1)) %>% 
      sf::st_cast("POINT")
    erg <- sf::st_sf(par_ID = 1:(nrow(tem_point)+1),
                          geometry = c(sf::st_geometry(polygon), 
                                       sf::st_geometry(tem_point)))
  }

  return(erg)
  
}


