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

clean_cat <- function(temp, shape){
  temp2 <- temp 
  shape_areas <- shape
  temp3 <- temp2 %>% 
    select(name, geometry) %>%
    group_by(name) %>% 
    summarise(do_union = F) %>%
    st_buffer(0.0) %>% 
    st_intersection(st_union(shape_areas))
  return(temp3)
  
}