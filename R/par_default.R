#' par_default 
#'
#' This function draws a set of randompolygons.
#'
#' @param polygon A Simple Feature geometry plot information see \link[soilprofile2]{apply_texture}
#' @return A unit multipolygon (sf) with one parID column
#'
#' @export


par_default <-  function(polygon){
  
  #Füllen der Standardeinstellungen:
  polygon <- polygon %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate_(col = 
                     ~ ifelse(par_ID != 1 & is.na(col),
                    "grey", 
                    col
             ),
           col = 
             ~  ifelse(par_ID == 1,
                    "black", 
                    col
             ),
           bgc = 
             ~ ifelse(sf::st_geometry_type(geometry) == "MULTIPOLYGON" & is.na(bgc),
                    "grey40", 
                    bgc
             ),
           
           linetype = 
             ~ ifelse(sf::st_geometry_type(geometry) == "MULTILINESTRING" & is.na(linetype) & par_ID != 1,
                    as.integer(sample(0:6, 1)), 
                    1
             ),
           pch = ~ ifelse(sf::st_geometry_type(geometry) == "MULTIPOINT" & is.na(linetype) & par_ID != 1,
                        as.integer(sample(25, 1)), 
                        19
           )
           
           
    ) %>% 
    sf::st_sf()
  
  return(polygon)
}

#' soil_theme 
#'
#' This function draws a set of randompolygons.
#'
#' @return A unit multipolygon (sf) with one parID column
#'
#' @export
soil_theme <- function(){
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank())
  
}

