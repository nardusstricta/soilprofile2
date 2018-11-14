#' wrapper function for plot
#'
#' This function draws a Ah pattern, with the following  helper fuction \link[soilprofile2]{basic_point}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' 
#' @return A unit sf layer with an parID column
#'
#' @export
plot_soil <- function(data){
  
  texture_sf <- apply_texture(shape = data)
  
  #Füllen der Standardeinstellungen:
  texture_sf$col <- ifelse(is.na(texture_sf$col), "black", texture_sf$col)
  
  plotOut <- texture_sf %>% 
    group_by(nameC) %>% 
    ggplot() +
    geom_sf(fill = texture_sf$bgc, col = texture_sf$col, shape = texture_sf$pch) 
  
  return(plotOut)
}
