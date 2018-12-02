#' fill standard par 
#'
#' This function draws a set of randompolygons.
#'
#' @param polygon
#' @return A unit multipolygon (sf) with one parID column
#'
#' @export


par_default <-  function(polygon){
  
  #Füllen der Standardeinstellungen:
  polygon <- polygon %>% 
    rowwise() %>% 
    mutate(col = 
             ifelse(par_ID != 1 & is.na(col),
                    "grey", 
                    col
             ),
           col = 
             ifelse(par_ID == 1,
                    "black", 
                    col
             ),
           bgc = 
             ifelse(st_geometry_type(geometry) == "MULTIPOLYGON" & is.na(bgc),
                    "grey40", 
                    bgc
             ),
           
           linetype = 
             ifelse(st_geometry_type(geometry) == "MULTILINESTRING" & is.na(linetype) & par_ID != 1,
                    as.integer(sample(0:6, 1)), 
                    1
             ),
           pch = ifelse(st_geometry_type(geometry) == "MULTIPOINT" & is.na(linetype) & par_ID != 1,
                        as.integer(sample(25, 1)), 
                        19
           )
           
           
    ) %>% 
    st_sf()
  
  return(polygon)
}

