#' This is a wrapper function which applies the texture function 
#' 
#'  A list of functions is stored in the package.
#'  These functions represent specific patterns for each horizon 
#'  and are named according to the horizons. 
#'  
#'  The actual task is to link the horizon rows to the corresponding functions and 
#'  then apply these functions
#'  
#'  If a horizonname do not matched a Function it 
#'  will be used the \link[soilprofile2]{build_random_pattern} function
#'
#'   
#' 
#' @param shape A Simple Features with one row for each hoizon. At least with one geometry column and one "nameC" column (C for character)
#' @param buffer Usually a negative value that indicates the distance between the pattern and the horizon boundary. 
#' @param fun_list A tibble with a name column that contains as many horizon 
#' shortcuts as possible and a column with the corresponding functions that create a pattern.
#'  An example tibble, which can be extended with the function \link[soilprofile2]{save_horizont_fun},
#'  is stored in the package (data("fun_list"))
#' @param df_par_wide A tibble with the graphical parameters in wide format. 
#'  The key to join the tables is the "name" column and the "par_Id" column. 
#'  The "par_Id" column starts with one for the background layer and then ascends to the top layer. 
#'  An example tibble, which can be extended with the function \link[soilprofile2]{save_par_setting},
#'  is stored in the package (data("df_par_wide"))
#' 
#' @return This function returns a new Simple Features which
#'  contains all informations (columns) of the Input Simple Features (shape).
#'  Columns with the graphic parameters are added. 
#'  Adding patterns to the given layer can result in multiple geometries.  (e.g. points and lines). 
#'  To save this information, rows are added to the dataset.  
#' @examples 
#' 
#' library(dplyr)
#' library(sf)
#' ## Example data with two horizont (Ah + Bv)
#' df_example <- data.frame(x = c(0, 20, 20, 0, 0), 
#' y = c(0, 0, 20, 20,0),
#' nameC = rep("Ah", 5)) %>%
#' rbind(data.frame(x = c(0, 20, 20, 0, 0), 
#' y = c(20, 20, 50, 50,20),
#' nameC = rep("Bv", 5))
#' )
#' 
#' ## Build an Simple Features
#' shape_example <- df_example %>%
#' st_as_sf(coords = c("x", "y")) %>% 
#' group_by(nameC) %>%
#' summarise(do_union = FALSE) %>%
#' st_cast("POLYGON") %>%
#' st_cast("MULTIPOLYGON")
#' \dontrun{
#' ## Creation of the patterns
#' texture_example <- apply_texture(shape = shape_example, buffer = -1.3) 
#' 
#' ## Plotting Data
#' library(ggplot2)
#' texture_example %>% ggplot() + geom_sf()
#' }
#' @export
#' @author Gabriel Holz


apply_texture <- function(shape, buffer = -1){

  texture_sf <- shape %>% 
    dplyr::left_join(fun_list, by = "nameC") %>% 
    dplyr::mutate(fun = ifelse(fun == "NULL", c(soilprofile2::build_random_pattern), fun)) 

  temp_geom <- sf::st_sf(par_ID = 0,
                         nameC = "empty",
                         geometry = shape$geometry[1])
                
  
  for (i in 1:nrow(texture_sf)){
    temp_geom <- texture(shape = texture_sf[i,],
                         fun_horizont = texture_sf$fun[[i]],
                         buffer = buffer
    ) %>% 
      rbind(temp_geom)

  }
  temp_geom <- temp_geom[-nrow(temp_geom),]

  ##
  #add graphic pars:
  ##
  erg <- temp_geom %>% 
    dplyr::left_join(df_par_wide, 
                     by = c("nameC", "par_ID")) 
  
  return(erg)
}


