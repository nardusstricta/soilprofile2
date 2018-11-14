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

smooth_trans <- function(Line, shape_mod, shape = 5, seed = 33, clean_cut = T){
  df <- shape_mod
  st_geometry(df) <- NULL
  
  df_line <- Line
  ##
  #Polygone erstelln um einen schwammigen Ünbergang zu gestalten:
  ##
  
  #1 Buffer:
  df_buffer <- df_line %>% 
    right_join(df, by = "name") %>% 
    group_by(name) %>% 
    st_buffer(dist = df$buffer_size, endCapStyle = "FLAT") %>% 
    filter(buffer_size != 0) %>%
    st_intersection(shape_mod) %>% 
    dplyr::select(name, nSides, rate, buffer_number) 
  
  #Punkte auf dem Buffer verteilen:
  set.seed(seed)
  df_sample  <- st_sample(df_buffer, size= df_buffer$buffer_number)
  df_inter <-  st_intersection(df_buffer, df_sample) #nur um die attribiutes zu behalten
  
  #Erstellen der Polygone: 
  #1.Varible Größe der Fläche und deren Verteilung 
  #2. Form der Fläche 
  temp0 <- df_inter %>% 
    group_by(name) %>% 
    mutate(area_size = rgamma(n(), shape = shape, rate = max(rate))) %>% 
    ungroup()
  
  point_2_polygon <- df_inter %>%  #Koordinaten für die Funktion extrahieren:
    st_coordinates()
  
  var <- as.data.frame(point_2_polygon)
  temp1 <- temp0
  
  #Geometrien Verändern: 
  for(i in 1 : nrow(var)){
    st_geometry(temp1)[i] <- convex_poly(nSides = temp0$nSides[i],
                                         area = temp0$area_size[i], 
                                         xstart = var$X[i], 
                                         ystart = var$Y[i])
  }
  temp2 <- temp1 
  tf <- st_intersects(temp1, shape_mod$geometry,  sparse = F )
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
  
  temp2$name <- tf1[,6] #Namen überschreinen
  if(clean_cut == T){
    #clean cut 
    temp_int <- temp2 %>% 
      rowwise() %>% 
      mutate(int = if_else(any(st_intersects(geometry, Line, sparse = F)) == T, T, F)) %>%
      filter(int == F) %>% 
      st_sf()
    
    temp3 <- temp_int %>% 
      select(name, geometry) %>%
      group_by(name) %>% 
      summarise(do_union = F) %>%
      st_buffer(0.0)  %>% 
      st_intersection(st_union(shape_mod)) 
    
    erg <- shape_mod %>%
      #filter(name %in% temp3$name) %>%
      #rowwise() %>% 
      mutate(geometry = ifelse(name %in% temp3$name, temp3$geometry, geometry))
    erg <- erg %>% 
      st_sf(geometry = erg$geometry)
       
      
      
    return(erg)
  }else{
    return(temp2)
  }
  
}
