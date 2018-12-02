#' wrapper function for plot
#'
#' This function draws a Ah pattern, with the following  helper fuction \link[soilprofile2]{basic_point}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' 
#' @return A unit sf layer with an parID column
#'
#' @export
plot_soil <- function(data, raster_list = NULL){
  
  texture_sf <- apply_texture(shape = data)
  
  #Füllen der Standardeinstellungen:
  texture_sf <- par_default(texture_sf)
  
  
  plotOut <- texture_sf %>% 
    group_by(nameC) %>% 
    ggplot() +
    geom_sf(fill = texture_sf$bgc, col = texture_sf$col, shape = texture_sf$pch, 
            linetype = texture_sf$linetype)
  
    #geom_spraster_rgb(raster_list[[i]])
    if(!is.null(raster_list)){
      for(i in 1:length(raster_list)){
        plotOut <- plotOut + RStoolbox::ggRGB(raster_list[[i]], 1,2,3,
                                              ggLayer = TRUE, alpha = 0.7)
        
      }
    }
  
  return(plotOut)
}
