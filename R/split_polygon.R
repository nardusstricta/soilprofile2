#' Split the soil body by lines to sepreate the horizons
#'
#' If you don't want a straight line as horizon transition, you can create different transition shapes with this function  
#'
#' @param Polygon The defult soil polygon, which has been build by the \link[soilprofile2]{sf_polygon} function.
#' @param Line The modifide  horizont trassition, which has been build by the \link[soilprofile2]{line_mod} function.
#' 
#' @return A new sf-Polygon set with modified horizons transitions
#'
#'
#'
#' @export

split_polygon <- function(Polygon, Line){
  Polygon_temp <- Polygon
  split2 <- st_union(Polygon_temp) %>%
    st_cast("LINESTRING") %>%
    st_union(Line$geometry[1]) %>%
    st_convex_hull()
  
  for (i in 1:nrow(Line)-1){
    split2 <- lwgeom::st_split(x = split2[1], y = Line$geometry[i+1]) %>% 
      st_collection_extract("POLYGON")
    Polygon_temp$geometry[i] <- split2[2]
  }
  Polygon_temp$geometry[nrow(Line)] <- split2[1]
  return(Polygon_temp)
}

