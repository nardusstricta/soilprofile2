#' basic point 
#'
#' This function draws a pointlayer, with the following helper fuction \link[sf]{st_sample} by random points and \link[sf]{st_make_grid}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param random if random or not
#' @return A unit point layer (sf) with one rown in the parID column
#'
#' @export

basic_point <- function(polygon, cellsize = 12, random = FALSE){
  if(random == T){
    sf::st_sample(polygon, size = cellsize) %>% 
      sf::st_union() %>% 
      sf::st_sf(parID = 1) #geometry colum named "."
  }else{
    sf::st_make_grid(polygon, cellsize = cellsize,  what = "centers", square = TRUE) %>% 
      sf::st_union() %>% 
      sf::st_sf(parID = 1) 
  }
  
}
