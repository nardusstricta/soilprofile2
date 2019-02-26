#' save_horizont_fun
#'
#' This function save the new horizont function in the global list
#'
#' @param name name of the function same to the horizontname
#' @param variable  par variable e.g col
#' @param value value of variable e.g. blue
#' @param par_ID the "id" which defines the layer 
#' @return A unit sf layer with an parID column
#'
#' @export


save_par_setting <- function(name, variable, value, par_ID){
  df_par <- data.frame("nameC" = name,
                       "variable" = variable, 
                       "value" = value,
                       "par_ID" = par_ID) 
  
  df_par_wide_temp <- tidyr::spread_(df_par, "variable", "value", convert = T)
  df_par_wide_temp$par_ID <- as.numeric(df_par_wide_temp$par_ID)
  
  df_par_wide <- df_par_wide_temp %>% 
    dplyr::bind_rows(df_par_wide)
  
  usethis::use_data(df_par_wide, overwrite = TRUE, internal = TRUE)
}

#' save_struct_poly
#'
#' This function stores the structure. Input is a raster (png) and output is a polygon (sf) needed to store the data for the Shiny app
#'
#' @param name name of the function structure
#' @param path  path of the png
#' @return A unit sf layer
#'
#' @export
save_struct_poly <- function(name, path){
  polygon1 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,-1), c(0,-1), c(0,0)))) %>% 
    sf::st_sfc() %>% 
    sf::st_sf()
  
  str_BvCv <- system.file("extdata", path, package = "soilprofile2")
  
  struc_temp <- multiple_png(polygon1,  str_BvCv) %>% 
    dplyr::mutate(name = name)
  
  struc_poly <- struc_temp %>% 
    rbind(struc_poly)
  
  usethis::use_data(struc_poly, overwrite = TRUE)
}




