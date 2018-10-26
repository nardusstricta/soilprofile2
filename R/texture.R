#' creates a pattern with a given layer and function 
#'
#' For each horizon name there should be a function which creates a corresponding pattern. This function applies the pattern function and creates a new layer. This is smaller than the original layer due to a defined buffer. This is displayed using a line. 
#' 
#' @param shape A sf tibble with a list fuction column and  a list geom column
#' @param fun_horizont The Pattern function, i.e. the column from the "shape" file (könnte man sich vlt auch sparen)
#' @param buffer A negetive numeric value that defines the distance to the outer boundary. The default is -0.5
#' 
#' @return This function returns a new geometry of the sf-file
#' @import tidyverse
#' @import sf
#' @export

texture <- function(shape, fun_horizont, buffer = -1){
  
  pars <- shape$geometry
  
  inner_polygon <- st_buffer(pars, buffer) %>% 
    st_intersection(pars)
  
  out_line <- st_cast(pars, 'MULTILINESTRING', do_split=FALSE)
  
  
  grid <- fun_horizont(polygon = inner_polygon)
  
  output_list <- st_sf(par_ID = c(1:(nrow(grid) +1)), nameC = shape$nameC, geometry = c(st_geometry(out_line), st_geometry(st_intersection(grid, inner_polygon))))

  return(output_list)
  
  
}
