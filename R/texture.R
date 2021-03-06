#' Creates a pattern for a layer with a given function  
#'
#' For each horizon name there should be a function which creates a corresponding pattern. This function applies the pattern function and creates a new layer. This new layer is smaller than the original layer due to a defined buffer.This is a helper function of the \link[soilprofile2]{apply_texture}. Here only pattern for one horizont is generated. With the \link[soilprofile2]{apply_texture} the whole profile will be created as a pattern. 
#' 
#' @param shape A Simple Features with one row (one geometry). At least with one geometry column and one "nameC" column (C for character)
#' @param fun_horizont A function that creates a pattern for this specific horizon. 
#' @param buffer Usually a negative value that indicates the distance between the pattern and the horizon boundary.
#' @param ... is passed on to the specific texture function  
#' @return This function returns a new Simple Features which
#'  contains all informations (columns) of the Input Simple Features (shape).
#'  Columns with the graphic parameters are added. 
#'  Adding patterns to the given layer can result in multiple geometries. (e.g. points and lines). 
#'  To save this information, rows are added to the dataset
#' @examples 
#' ## Example data with one horizont (Ah)
#' library(dplyr)
#' library(sf)
#' df_example <- data.frame(x = c(0, 20, 20, 0, 0), 
#' y = c(0, 0, 20, 20,0), 
#' nameC = rep("Ah", 5))
#' 
#' ## Build an Simple Features
#' shape_example <- df_example %>% 
#' st_as_sf(coords = c("x", "y")) %>% 
#' group_by(nameC) %>% 
#' summarise(do_union = FALSE) %>%
#' st_cast("POLYGON") %>%
#' st_cast("MULTIPOLYGON") 
#' 
#' ## example Function
#' fun_example <- function(polygon){
#' st_sample(polygon, size = 50) %>%
#' st_union() %>% 
#' st_sf(parID = 1)
#' }
#' 
#' 
#' ## Apply Function
#' texture_example <- texture(shape = shape_example,
#' fun_horizont = fun_example, 
#' buffer = -1)
#' 
#' ##Plotting
#' library(ggplot2)
#' 
#' texture_example %>% 
#' ggplot() + 
#' geom_sf()
#'  
#' @export


texture <- function(shape, fun_horizont, buffer, ...){
  
  pars <- shape$geometry
  
  inner_polygon <- sf::st_buffer(pars, buffer) %>% 
    sf::st_intersection(pars)
  
  if(length(inner_polygon) == 0){
    stop("Your buffer is bigger than your horizon. 
         This means there is no area where the pattern can be drawn. Try again with a smaller buffer!")
  }
  
  out_line <- sf::st_cast(pars, 'MULTILINESTRING', do_split = FALSE)
  
  
  grid <- fun_horizont(polygon = inner_polygon, ...)
  
  output_list <- sf::st_sf(par_ID = c(1:(nrow(grid) + 1)), 
                       nameC = shape$nameC,
                       geometry = c(sf::st_geometry(out_line), 
                                    sf::st_geometry(grid)
                                    )
                       )

  return(output_list)
  
  
}
