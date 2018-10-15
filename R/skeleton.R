#' soil skeleton 
#'
#' generates new layer for the soil skeleton
#'
#' @param X A numeric vector that giving the start and end of the horizont line
#' @param excited Logical value specifying whether to include an exclamation
#'    point after the text
#'
#' @return This function returns 12 X-value. 10 of this are new generatet by the \link[base]{runif}  Funktion 
#' @usage This function is used internally by the \link[soilprofile2]{line_mod} function. The corresponding Y values are created with the similar function \link[soilprofile2]{genY_fun}.
#' @import tidyverse
#' @import sf
#' @export


skeleton <- function(shape_mod, point_shape = c(0,0,4,15,33), jitter = F, union = T, smooth= T){
shape_s  <- shape_mod %>%
  select(name, skel_ab, skel_dim, area) %>%
  mutate(point_shape = point_shape) %>%
  na.omit()  %>%
  mutate(area_comp = area * skel_ab) %>%
  mutate(skel_number = 100 - skel_dim/sum(skel_dim)*100)
 

  sammple_point <- st_sample(shape_s, round(shape_s$skel_number/2))
  spoint <-  st_intersection(shape_s,  sammple_point) %>% 
    group_by(name) %>% 
    mutate(random_part = rgamma(n(),5, 1)) %>% 
    mutate(random_part = random_part/sum(random_part)) %>% 
    mutate(area_size = area_comp * random_part) %>% 
    ungroup()
  

  xy_cord <- spoint %>%  #Koordinaten für die Funktion extrahieren:
    st_coordinates()
 
 
  xy_cord <- as.data.frame(xy_cord)
  for(i in 1 : nrow(xy_cord)){
    st_geometry(spoint)[i] <- convex_poly(nSides = spoint$point_shape[i],
                                         area = spoint$area_size[i], 
                                         xstart = xy_cord$X[i], 
                                         ystart = xy_cord$Y[i])
  }
  
  spoint_clean <-  spoint %>%
    select(name, geometry) %>%
    st_buffer(0.0) %>% 
    st_intersection(shape_mod) %>% 
    ungroup()
  
  if(smooth == T){
    spoint_clean <- smooth(spoint_clean, method = "ksmooth")
  }

  
  

  
  if(jitter == T){
    jitter <- spoint_clean %>% 
      st_jitter(factor = 0.1) %>% 
      st_intersection(shape_mod)
    return(jitter)
  }
  
  if(union == T){
    spoint_clean <-  spoint_clean %>%
      group_by(name) %>% 
      summarise(do_union = F) %>%
      st_buffer(0.0) %>% 
     # st_union(shape_mod) %>% 
      ungroup()
    return(spoint_clean)
  }
  return(spoint_clean)
}

