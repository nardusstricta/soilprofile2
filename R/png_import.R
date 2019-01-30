#' imports png file an maks to the specific horizont position
#'
#' sourse sollte noch angepasst werden, evt dropbox oder db
#' 
#' 
#' 
#' @param path a numeric value between 1 and 6 calling the stored L-system
#' @param mod_sf A layer with the skeleton content as polygon (bei fehlen gibt es noch einen Fehler)
#' @export 
#' 

multiple_png <- function(shape_temp, path_temp, ...){
  stopifnot(nrow(shape_temp)==length(path_temp))
  geom1 <- do.call(
    rbind, lapply(
      1:nrow(shape_temp), function(i){
        raster2polygon(file_path = path_temp[i], horizont = shape_temp[i,], ...)
      }
    )
  )
  return(geom1)
}


#' imports png file an maks to the specific horizont position
#'
#' sourse sollte noch angepasst werden, evt dropbox oder db
#' 
#' 
#' 
#' @param path a numeric value between 1 and 6 calling the stored L-system
#' @param mod_sf A layer with the skeleton content as polygon (bei fehlen gibt es noch einen Fehler)
#' @export 
#' 
png_import <- function(file_path, horizont, smoothness, raster2polygon = T){
  
  png_temp <- magick::image_read(file_path)
  
  tiff_file <- tempfile()
  
  magick::image_write(png_temp, path = tiff_file, format = 'tiff')
  
  r <- raster::raster(tiff_file)
  
  bbox1 <- sf::st_bbox(horizont)
  srtm_masked <- list()
  
  #r_index <- which(names(raster_list) == mod_sf$nameC[i])
  raster::extent(r) <- bbox1[c(1,3,2,4)]
  
  srtm_masked <- raster::mask(r, as(horizont, "Spatial"))
  
  r <- srtm_masked
  
  if(raster2polygon == T ){
    r <- raster::cut(r, breaks = c( 150,-Inf, Inf)) 
    
    r_poly <- raster::rasterToPolygons(r, function(x){x == 1}, dissolve = TRUE) %>% 
      sf::st_as_sf()
    
    erg <- smoothr::smooth(r_poly, method = "ksmooth", smoothness = smoothness)
    return(erg)
    
  }else{
    return(r)
  }

}
