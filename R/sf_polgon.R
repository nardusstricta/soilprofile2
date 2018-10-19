#' sf polygon builder
#'
#' merge coordinats with the input dataframe by name
#'
#' @param database The original table with all variables
#' @param df_polygon A long table with the columns name, x coordinate and y coordinate for each horizont. Build by the function \link[soilprofile2]{cord_setting}
#'
#' @return This function returns an set of polygons as simple features. Each polygon represents a horizon defined by the name
#'
#'
#' @export
sf_polgon <- function(database, df_polygon){
  #Erstellen von rechtwickligen Horizonten mit geraden Linien
  shape_areas <- df_polygon %>%
    st_as_sf(coords = c("x", "y")) %>%
    group_by(name) %>%
    summarise(do_union = F) %>%
    st_cast("POLYGON") %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(area = st_area(geometry)) %>% 
    left_join(database, by = "name") %>% 
    ungroup()
  return(shape_areas)
}
