#' soil skeleton 
#'
#' generates new layer for the soil skeleton
#'
#' @param shape_mod A numeric vector that giving the start and end of the horizont line
#' @param point_shape Logical value specifying whether to include an exclamation
#'    point after the text
#'
#' @return This function returns 12 X-value. 10 of this are new generatet by the \link[base]{runif}  Funktion 
#' @usage This function is used internally by the \link[soilprofile2]{line_mod} function. The corresponding Y values are created with the similar function \link[soilprofile2]{genY_fun}.

#' @export


skeleton <- function(shape_mod, skeleton_mat){
shape_s  <- shape_mod %>%
  select(name, skel_ab, skel_dim_from, skel_dim_to, area) %>%
  group_by(name) %>% 
  mutate(area_comp = area * skel_ab) %>% #absoluter Skelettanteil
  mutate(skel_dim_mean = mean(c(skel_dim_from, skel_dim_to), rm.na = T)) %>%
  mutate(skel_number = area_comp/(skel_dim_mean * skel_dim_mean))  %>%
  #Die
  #Anzahl der Steine hängt von den Korngroße ab. D.h. je kleiner die Korngröße
  #ist desto mehr Steine habe ich. Dies ist abhängig von allen angegeben Korngroßen.
  replace_na(list(skel_number = 0))  %>% 
  left_join(skeleton_mat, by = "name") %>% 
  ungroup()

for(i in which(shape_s$strat == T)){
  shape_s$geometry[i] <-
    basic_line(shape_s$geometry[i], 
               cellnumber = shape_s$cellnumber[i],
               rotation = shape_s$rotation[i]) %>% 
    st_sample(round(shape_s$skel_number[i])) %>% 
    st_union()
}
  
for(i in which(shape_s$strat == F & shape_s$skel_number != 0)){
    shape_s$geometry[i] <- st_sample(shape_s$geometry[i], 
                                     shape_s$skel_number[i]) %>% 
      st_union()
    
}
  
  spoint <-  st_intersection(shape_s,  shape_mod$geometry) %>% 
    st_collection_extract(type = "POINT") %>%  
    st_cast("POINT") %>% 
    group_by(name) %>% 
    mutate(random_part = runif(n(), min = skel_dim_from, max = skel_dim_to)) %>% 
    mutate(random_part = random_part/sum(random_part)) %>% 
    mutate(area_size = area_comp * random_part) %>% 
    ungroup()

  xy_cord <- spoint %>%  #Koordinaten für die Funktion extrahieren:
    st_coordinates() 

  xy_cord <- as.data.frame(xy_cord)
  #spoint3 <- left_join(spoint2, xy_cord, by = c("name"="L1"))

  spoint4 <- spoint %>% 
    dplyr::bind_cols(xy_cord)
  
  for(i in 1 : nrow(xy_cord)){
    st_geometry(spoint4)[i] <- convex_poly(data_skel = spoint4[i,])
  }
  
  spoint_clean <-  spoint4 %>%
    st_intersection(shape_mod$geometry)

   spoint_clean[which(spoint_clean$smooth == T),] <- 
     smooth(spoint_clean[which(spoint_clean$smooth == T),], method = "ksmooth")
   
   
   test <- st_union(spoint_clean[which(spoint_clean$union == T),])
   test2 <- spoint_clean$geometry[which(spoint_clean$union == F)]
   spoint_clean1 <- c(test, test2)

  return(spoint_clean1)
}


