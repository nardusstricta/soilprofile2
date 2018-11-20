#' build random texture 
#'
#' The actual task is to join the horizon file with the corresponding functions  and to apply it   
#' 
#' @param polygon A sf tibble with a list fuction column and  a list geom column
#' @param ... A list with a name column that contains as many horizon 
#' shortcuts as possible and a column with the corresponding functions that create a pattern.  
#' 
#' @return This function returns a new geometry of the sf-file equal to \link[soilprofile2]{texture} (which is used internally)
#' 
#' @export

build_random_pattern <- function(polygon, ...){
  par_list <- list(...)
  random_par <- list(radnom_line <- 
                       function(polygon){
                         basic_line(
                           polygon = polygon,
                           cellnumber = round(rnorm(1, mean=22, sd = 4),0),
                           rotation = sample(1:100, 1))
                       },
                     random_point <- 
                       function(polygon){
                         basic_point(
                           polygon = polygon,
                           cellsize = rnorm(1, mean=12, sd = 5), 
                           random = sample(c(F, T), 1)
                         )
                       },
                     random_polygon <- function(polygon){
                       basic_polygon(
                         polygon = polygon,
                         cellsize = rnorm(1, mean=12, sd = 5),
                         square = sample(c(F, T), 1)
                       )
                       
                     }
                     
  )
  
  
  index <- sample(length(random_par), 1)
  layer1 <- random_par[[index]](polygon = polygon)
  
  erg_sf <- st_sf(par_ID = 1, geometry = st_geometry(layer1)) 
  par_ID <- 1
  size <- rnorm(1, mean = 1, sd = 1)
  #number_char <- nameC
  number_char <- nchar(par_list[1])
  for(i in 1:length(number_char-2)){
    par_ID <- par_ID + 1
    size  <- size * .5
    number <- sample(4:30, 1)
    nSides <- sample(4:50, 1)
    sm <- T
    layer_temp <- basic_random_polygon(polygon = polygon,
                                       size = size,
                                       number = number,
                                       nSides = nSides,
                                       sm = sm
    )
    erg_sf <- rbind(erg_sf,
                    st_sf(par_ID = par_ID,
                          geometry = st_geometry(layer_temp)
                    )
    )
    
    par_ID <- par_ID + 1
    
    index <- sample(length(random_par), 1)
    polygon <- st_geometry(erg_sf[length(erg_sf$geometry),])
    
    geom <- random_par[[index]](polygon = polygon) %>% 
      st_intersection(polygon)
    
    erg_sf <- rbind(erg_sf,
                    st_sf(par_ID = par_ID,
                          geometry = st_geometry(geom)
                    )
    )
    
    return(erg_sf)
    
    
  }
  
}

