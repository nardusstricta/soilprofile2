#' imports multiple png file an maks to the specific horizont position
#'
#' @param shape_temp a simple feature with the same length of "path_temp"
#' @param path_temp a character string with the path of each texture .png
#' @param ... is passed to function \link[soilprofile2]{png_import}
#' @examples 
#' library(dplyr) 
#' library(ggplot2)
#' 
#' sf_example <- soilprofile2::soil_example_sf
#' str_Bvh <- system.file("extdata", "broeckel.png", package = "soilprofile2")
#' str_BvCv <- system.file("extdata", "prismen.png", package = "soilprofile2")
#' 
#' str_all <- multiple_png(sf_example[c(2,3), ], c(str_Bvh, str_BvCv))
#' 
#' sf_example %>% 
#'   ggplot() +
#'   geom_sf(fill = sf_example$rgb_col) +
#'   geom_sf(data = str_all, fill = "black") +
#'   soil_theme()
#' @export 

multiple_png <- function(shape_temp, path_temp, ...){
  stopifnot(nrow(shape_temp)==length(path_temp))
  geom1 <- do.call(
    rbind, lapply(
      1:nrow(shape_temp), function(i){
        png_import(file_path = path_temp[i], horizont = shape_temp[i,], ...)
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
#' @param file_path a numeric value between 1 and 6 calling the stored L-system
#' @param horizont A layer with the skeleton content as polygon (bei fehlen gibt es noch einen
#' @param smoothness passed to the smoothr function
#' @param raster2polygon an integer if the raster geometry shoud be transform to an vector polygon 
#' @examples 
#' library(dplyr) 
#' library(ggplot2)
#' sf_example <- soilprofile2::soil_example_sf
#' photo_Ah_path <- system.file("extdata", "photo_example.png", package = "soilprofile2")
#' photo_Ah <- png_import(photo_Ah_path, sf_example[1,], raster2polygon = FALSE)
#' 
#' 
#' sf_example %>%
#'   ggplot() +
#'   geom_sf(fill = sf_example$rgb_col) +
#'   ggspatial::layer_spatial(photo_Ah)
#' soil_theme()
#' @export 

png_import <- function(file_path, horizont, smoothness = 2, raster2polygon = T){
  
  png_temp <- magick::image_read(file_path)
  
  tiff_file <- tempfile()
  
  magick::image_write(png_temp, path = tiff_file, format = 'tiff')
  
  if(raster2polygon == T){
    r <- raster::raster(tiff_file)
    }else{
     r <- raster::brick(tiff_file)
  }
  
  
  
  bbox1 <- sf::st_bbox(horizont)
  srtm_masked <- list()
  
  #r_index <- which(names(raster_list) == mod_sf$nameC[i])
  raster::extent(r) <- bbox1[c(1,3,2,4)]
  
  srtm_masked <- raster::mask(r, as(sf::st_union(horizont), "Spatial"))
  
  r <- srtm_masked
  
  if(raster2polygon == T){
    r <- raster::cut(r, breaks = c( 150,-Inf, Inf)) 
    
    r_poly <- raster::rasterToPolygons(r, function(x){x == 1}, dissolve = TRUE) %>% 
      sf::st_as_sf()
    
    erg <- smoothr::smooth(r_poly, method = "ksmooth", smoothness = smoothness)
    
    return(erg)
    
  }else{
    return(r)
  }

}

