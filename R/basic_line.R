#' basic line 
#'
#' This function draws a line-layer, with the following helper fuction \link[sf]{st_bbox}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param rotation The angle of the line
#' @param cellnumber The distance between the lines
#' @return A unit point layer (sf) with one rown in the parID column
#'
#' @export

basic_line <- function(polygon, cellnumber = 22, rotation = 12){
  x <- sf::st_bbox(polygon)[c("xmin", "xmax")]
  x["xmin"] <- x["xmin"] - rotation
  x["xmax"] <- x["xmax"] + rotation 
  x <- rep(rep(seq(x[1], x[2],
                   length.out = cellnumber),2), each =2)
  
  x[seq(2,length(x), 2)] <- x[seq(2,length(x), 2)] + rotation
  df <- data.frame(x = x,
                   y = rep(st_bbox(polygon)[c("ymin", "ymax")], 
                           cellnumber * 2),
                   id = rep(1:cellnumber, each = 2)
  )
  
  line_sf <- df %>% 
    sf::st_as_sf(coords = c("x", "y")) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(do_union = F) %>% 
    sf::st_cast("LINESTRING") %>% 
    sf::st_cast("MULTILINESTRING") %>% 
    sf::st_union() %>% 
    sf::st_sf(parID = 1)
  return(line_sf)
  
}
