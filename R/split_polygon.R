#' Split soil profile
#' 
#' This fuction splits the whole soil profile into individual horizons using lines. 
#' If you do not want a straight line as a horizon transition, you can use this function to create various transition shapes.  
#'
#' @param polygon A simple feature multipolygon, which can been build by the \link[soilprofile2]{sf_polygon} function. The simple feature should be have at least a geom column. In any case, the order of the raws should be the same as the real order of the horizons. The top horizon must appear in the first line. 
#' @param line A simple feature multiline, which can been build by the \link[soilprofile2]{line_mod} function. The simple feature should be have at least a geometry column and a name column with the Id. 
#' In any case, the order of the raws should be the same as the real order of the horizons. The uppermost line (i.e. the first line) represents the surface of the earth. Each additional line represents the upper horizon limit. 
#' 
#' 
#' @return A new simple feature with one line for each horizont and the columns for the atttributes. 
#' @examples 
#' library(tidyverse)
#'## Example data
#'data_example <- data.frame(name = c(1, 2),
#'                           from1 = c(0,20),
#'                           to1 = c(20, 40)
#')
#'  
#'  ##Coordination setting
#'  cord_example <- cord_setting(data_example, plot_width = 3)
#'  
#'  ## build simple feature
#'  poly_example <- sf_polygon(df_geom = cord_example)
#'  
#'  ## Line attributes data frame:
#'  lattri_example <- data.frame(name= unique(cord_example$name),
#'                               numberX = c(2, 10),
#'                               sd = c(1,1),
#'                               sm = c(T, T)
#'  )
#'  
#'  ## Apply the line_mod fuction
#'  line_example <- line_mod(df = cord_example, line_attri = lattri_example)
#'  
#'  ## Split the Profile with the new line shape:
#'  example_profile <- split_polygon(polygon = poly_example,
#'                                   line = line_example) %>%
#'    #join by the id (name) user-defined properties
#'    left_join(data.frame(name = c(1,2),
#'                         rgb_col = c("#6F5F4CFF", "#947650FF")
#'    ), by = "name")
#'  
#'  ## Plot simple feature geometrysf_example %>%
#'  example_profile %>%
#'    ggplot() +
#'    geom_sf(fill = example_profile$rgb_col) +
#'    geom_text(label = c("Ah", "Bv"),
#'              x = c(10, 10), y = c(-10,-30)) +
#'    theme(axis.title.x=element_blank(),
#'          axis.text.x=element_blank(),
#'          axis.ticks.x=element_blank(),
#'          panel.background = element_blank())
#'
#'
#' @export

split_polygon <- function(polygon, line){
  split2 <- sf::st_union(polygon) %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_union(line$geometry[1]) %>%
    sf::st_convex_hull()
  
  for (i in 1:nrow(line)-1){
    split2 <- lwgeom::st_split(x = split2[1], y = line$geometry[i+1]) %>% 
      sf::st_collection_extract("POLYGON")
    polygon$geometry[i] <- split2[2]
  }
  polygon$geometry[nrow(line)] <- split2[1]
  return(polygon)
}

