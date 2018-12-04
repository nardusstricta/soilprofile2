#' This is a helper function which applies the texture function.  
#'
#' The actual task is to join the horizon file with the corresponding functions  and to apply it   
#' 
#' @param shape A sf tibble with a list fuction column and  a list geom column
#' @param fun_list A list with a name column that contains as many horizon 
#' shortcuts as possible and a column with the corresponding functions that create a pattern.  
#' 
#' @return This function returns a new geometry of the sf-file equal to \link[soilprofile2]{texture} (which is used internally)

#' @export

apply_texture <- function(shape){
  #Data import:
  data("fun_list")
  data("df_par_wide.rda")
  
  texture_sf <- shape %>% 
    left_join(fun_list, by = "nameC") %>% 
    mutate(fun = ifelse(fun == "NULL", c(build_random_pattern), fun)) 
  
  temp_geom <- st_sf(par_ID = 0,
                     nameC = "empty",
                     geometry = shape$geometry[1])
  
  for (i in 1:nrow(texture_sf)){
    temp_geom <- texture(shape = texture_sf[i,],
                         fun_horizont = texture_sf$fun[[i]]
    ) %>% 
      rbind(temp_geom)

  }
  temp_geom <- temp_geom[-nrow(temp_geom),]

  ##
  #add graphic pars:
  ##
  
  
  erg <- temp_geom %>% 
    left_join(df_par_wide, by = c("nameC", "par_ID")) 
  
  return(erg)
  
}



