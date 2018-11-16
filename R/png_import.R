#' imports png file an maks to the specific horizont position
#'
#' sourse sollte noch angepasst werden, evt dropbox oder db
#' 
#' 
#' 
#' @param path a numeric value between 1 and 6 calling the stored L-system
#' @param mod_sf A layer with the skeleton content as polygon (bei fehlen gibt es noch einen Fehler)

png_import <- function(path, mod_sf){
  #import file:
  path_list <- list.files(path, full.names = F, pattern ="*png") 
  
  #select only imag from existing sf_mod horizont:
  name_raw0 <- substr(path_list, 1,nchar(path_list)-4)
  
  name_temp <- path_list[which(name_raw0 %in% mod_sf$nameC)]
  name_raw <- name_raw0[which(name_raw0 %in% mod_sf$nameC)]
  
  mod_sf <- mod_sf %>% 
    filter(nameC %in% name_raw)
  
  #read image
  png_veg <- magick::image_read(paste0(path, path_list), name_temp)

  #build temp path:
  tiff_path <- file.path(tempdir(), name_raw)
  #tempfile()
  #write image
  for (i in 1:length(png_veg)){
    magick::image_write(png_veg[i], path = tiff_path[i], format = 'tiff')
  }
  
  raster_list <- sapply(tiff_path, raster::brick)
  names(raster_list) <- name_raw

  #set extent and mask the image:
  
   bbox1 <- sapply(mod_sf$geometry, st_bbox)
   srtm_masked <- list()
   for(i in 1:length(raster_list)){
     r_index <- which(names(raster_list) == mod_sf$nameC[i])
     raster::extent(raster_list[[r_index]]) <- bbox1[c(1,3,2,4),i]
     srtm_masked[[i]] <- raster::mask(raster_list[[r_index]], as(mod_sf[i,], "Spatial"))
   }
   
    

  return(srtm_masked)
  
  
}
