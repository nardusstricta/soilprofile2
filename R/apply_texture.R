#' This is a helper function which applies the texture function.  
#'
#' The actual task is to join the horizon file with the corresponding functions  and to apply it   
#' 
#' @param shape A sf tibble with a list fuction column and  a list geom column
#' @param fun_list A list with a name column that contains as many horizon 
#' shortcuts as possible and a column with the corresponding functions that create a pattern.  
#' 
#' @return This function returns a new geometry of the sf-file equal to \link[soilprofile2]{texture} (which is used internally)
#' 
#' @import tidyverse
#' @import sf
#' @export

apply_texture <- function(fun_list, shape = shape_mod){
  texture_sf <- shape %>% 
    group_by(name) %>% 
    left_join(fun_list, by = "nameC")  %>%
    na.omit(fun) %>%
    ungroup()
  #mutate(geometry =  texture(.))
  
  for (i in 1:nrow(texture_sf)){
    st_geometry(texture_sf)[i] <- texture(shape = texture_sf[i,], 
                                          fun_horizont = texture_sf$fun[[i]])
  }
  return(texture_sf)
  
}
