#' Convert dataframe to an simple features object
#'
#' merge coordinats with the input data frame by name
#' 
#' @param df_geom A long table with the columns named "name", "x" and "y" (Id and coordinates) for each horizont. For example created by the \link[soilprofile2]{cord_setting} function 
#' 
#' @param df_attri The original table with all variables
#'
#' @return This function returns an set of polygons as simple feature. Each polygon represents a horizon defined by the name
#'
#' @examples 
#' ##Example1
#' library(dplyr)
#' data_example <- data.frame(name = c(1, 2),
#' from1 = c(0,20),
#' to1 = c(20, 40)
#' )
#' 
#' ##Coordination setting
#' cord_example <- cord_setting(data_example, plot_width = 3)
#' 
#' ##Plot simple features
#' plot(sf_polygon(df_geom = cord_example)$geometry)
#' 
#' ##Example2
#' 
#' ##example data
#' geom_example <- data.frame(name = c(1, 2),
#' from1 = c(0,20),
#' to1 = c(20, 40)
#' ) %>% 
#' cord_setting(plot_width = 3)
#' attri_example <- data.frame(name = c(1, 2),
#' rgb_col = c("#6F5F4CFF", "#947650FF"),
#' nameC = c("Ah", "Bv")
#' )
#' 
#' ##
#' sf_example <- sf_polygon(df_geom = geom_example, df_attri = attri_example)
#' 
#' ##Plot with ggplot
#' library(ggplot2)
#' sf_example %>% 
#' ggplot() +
#' geom_sf(fill = sf_example$rgb_col) +
#' geom_text(label = sf_example$nameC, x = c(10, 10), y = c(-10,-30)) +
#' theme(axis.title.x=element_blank(),
#' axis.text.x=element_blank(),
#' axis.ticks.x=element_blank(),
#' panel.background = element_blank())
#' @export
sf_polygon <- function(df_geom, df_attri = NULL){
  shape_areas <- df_geom %>%
    sf::st_as_sf(coords = c("x", "y")) %>%
    dplyr::group_by_(~name) %>%
    dplyr::summarise(do_union = F) %>%
    sf::st_cast("POLYGON") %>%
    sf::st_cast("MULTIPOLYGON") %>%
    dplyr::mutate_(area = ~ sf::st_area(geometry)) %>% 
    dplyr::ungroup() %>% 
    sf::st_sf()
  
  if(!is.null(df_attri)){
    shape_areas <- shape_areas %>% 
      dplyr::left_join(df_attri, by = "name") 
  }
  return(shape_areas)
}
