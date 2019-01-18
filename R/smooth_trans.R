#' Horizons transition  
#'
#' This funktion modifide th horizons transitions. With this function you get waves, tongues and points as transition between the horizons. 
#'
#' @param Line The line layer, which has been build by the \link[soilprofile2]{line_mod} function.
#' @param shape_mod The table modified by new lines  \link[soilprofile2]{split_polygon}
#'
#' @return A point layer with a polygons for the color transition 
#'
#' @export

smooth_trans <- function(lmod, shape_mod, shape = 15, seed = 33){
 # benchmark("data_mod1" = {
  df <- shape_mod
  
  sf::st_geometry(df) <- NULL
  
  shape_mod <- shape_mod %>%
    dplyr::select(name)
  
  df_line <- lmod %>% 
    dplyr::select(name)
  ##
  #Polygone erstelln um einen schwammigen Ünbergang zu gestalten:
  ##
  
  #1 Buffer:
  df_buffer <- df_line %>% 
    dplyr::right_join(df, by = "name") %>% 
    dplyr::group_by(name) %>% 
    sf::st_buffer(dist = df$buffer_size, endCapStyle = "FLAT") %>% 
    dplyr::filter(buffer_size != 0) %>%
    sf::st_intersection(shape_mod) %>% 
    dplyr::select(name, nSides, rate, buffer_number) 
  
  #Punkte auf dem Buffer verteilen:
  set.seed(seed)
  df_sample  <- sf::st_sample(df_buffer, size= df_buffer$buffer_number)
  df_inter <-  sf::st_intersection(df_buffer, df_sample) #nur um die attribiutes zu behalten
  
  #Erstellen der Polygone: 
  #1.Varible Größe der Fläche und deren Verteilung 
  #2. Form der Fläche 
  temp0 <- df_inter %>% 
    dplyr::group_by(name) %>% 
    dplyr::mutate(area_size = rgamma(n(), shape = shape, rate = max(rate))) %>% 
    dplyr::ungroup()
  #}, "point_to_polygon" = {
  temp1 <- point_2_polygon(temp0)
 # }, "smooth" = {
  temp1 <- smoothr::smooth(temp1)#, method = "ksmooth")

 # }, "clean0" = {
  tf <- sf::st_intersects(temp1, shape_mod$geometry,  sparse = F )
  tf1 <- cbind(tf, temp1$name)
  
  for(i in 1:5){
    for(j in 1:nrow(tf1)){
      if(tf1[j,i] == 1){
        if(i == tf1[j,6]){
          tf1[j,6] <- tf1[j,6] -1
        }
      }
    }
  }
  
  temp1$name <- tf1[,6] #Namen überschreiben

    #clean cut 
    temp_int <- temp1 %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(int = if_else(any(st_intersects(geometry, df_line, sparse = F)) == T, T, F)) %>%
      sf::st_sf() %>% 
      dplyr::select(name, int)
 # }, "clip1" = {   
    #shape_mod Polygon verändern: 
    #1. Alle die auf dem Übergang liegen:
    temp2 <- temp_int %>% 
      dplyr::filter(int == T) %>% 
      dplyr::select(name) %>%
      dplyr::group_by(name) %>% 
      dplyr::summarise(do_union = F) %>% 
      sf::st_buffer(0.0) %>% 
      sf::st_intersection(st_union(shape_mod)) 
    
    shape_mod1 <- sf::st_difference(shape_mod, st_combine(temp2)) %>% 
      dplyr::select(name) %>% 
      rbind(temp2) %>%   
      dplyr::group_by(name) %>% 
      dplyr::summarise(do_union = T) 
#  }, "clip2" = {   
#2. Alle weiteren
    temp3 <- temp_int %>% 
      dplyr::filter(int == F) %>% 
      dplyr::select(name) %>%
      dplyr::group_by(name) %>% 
      dplyr::summarise(do_union = F) %>% 
      sf::st_buffer(0.0) %>% 
      sf::st_intersection(st_union(shape_mod1))

    shape_mod2 <- st_difference(shape_mod1, st_combine(temp3)) %>% 
      dplyr::select(name) %>% 
      rbind(temp3) %>%   
      dplyr::group_by(name) %>% 
      dplyr::summarise(do_union = FALSE)
    
 # }, replications = 1)   
    stopifnot("sf" %in% class(shape_mod2))

    return(shape_mod2)
    
}
