#' Clip the point layer by horizons 
#'
#' The buffer allows points to lie outside the horizons, these are cut out with this function.
#'
#' @param temp The point layer, which has been build by the \link[soilprofile2]{smooth_trans} function.
#' @param shape A sf multipolygon set which has been biuld by the   \link[soilprofile2]{st_polygon}
#'
#' @return A point layer with the dimension of the orginal horizons
#'
#'
#' @export

clean_cat <- function(temp, shape, Line){
  temp_int <- temp %>% 
    rowwise() %>% 
    mutate(int = if_else(any(st_intersects(geometry, Line, sparse = F)) == T, T, F)) %>%
    filter(int == F) %>% 
    st_sf()

  temp3 <- temp_int %>% 
    select(name, geometry) %>%
    group_by(name) %>% 
    summarise(do_union = F) %>%
    st_buffer(0.0)  %>% 
    st_intersection(st_union(shape))

    
  return(temp3)
  
}
