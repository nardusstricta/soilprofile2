#' soil skeleton 
#'
#' generates new layer for the soil skeleton
#'
#' @param shape_mod A simple feature class object with the base geometry. At least the following column names must be present in the attributes: name (horizont id), skel_ab (abundanz), skel_dim_from (dimension from), skel_dim_to (dimension to), area (area). 
#' 
#' @param skeleton_mat A data frame with the following parameters (as column names) for specifying the rock form:  "name", "nSides", "smooth", "union", "phi", "strat", "cellnumber", "rotation"
#'
#' @return This function returns 12 X-value. 10 of this are new generatet by the \link[stats]{runif}  Funktion. If a parameter is unknown set 0 or NA in the cell.  
#' @usage 

#' @export


skeleton <- function(shape_mod, skeleton_mat){
  
  stopifnot(c("sf", "data.frame") %in% class(shape_mod)  &
              class(skeleton_mat) == "data.frame")
  
  stopifnot(c("name", "skel_ab", "skel_dim_from", "skel_dim_to", "area") %in%
              colnames(shape_mod))
  
  stopifnot(c("name","nSides","smooth", "union", 
              "phi", "strat", "cellnumber", "rotation") %in% 
              colnames(skeleton_mat))


  shape_s  <- shape_mod %>%
    #select the default parameter: 
    dplyr::select_(~name, ~skel_ab, ~skel_dim_from, ~skel_dim_to, ~area) %>%
    dplyr::group_by_(~name) %>% 
    #calculate the absolute skeleton content
    dplyr::mutate_(area_comp = ~ (area * skel_ab)) %>% 
    #calculate the mean of skeleton dimension
    dplyr::mutate_(skel_dim_mean = ~ mean(c(skel_dim_from, skel_dim_to), rm.na = T)) %>% 
    #calculate the number of skeleton
    dplyr::mutate_(skel_number = ~ (area_comp/(skel_dim_mean * skel_dim_mean)))  %>%
    tidyr::replace_na(list(skel_number = 0))  %>% 
    #join with skeleton parameter:
    dplyr::left_join(skeleton_mat, by = "name") %>% 
    dplyr::ungroup()
  
  #Calculating the coordinates for the stratified rock
  for(i in which(shape_s$strat == T)){
    shape_s$geometry[i] <-
      basic_line(shape_s$geometry[i], 
                 cellnumber = shape_s$cellnumber[i],
                 rotation = shape_s$rotation[i]) %>% 
      sf::st_sample(round(shape_s$skel_number[i])) %>% 
      sf::st_union()
  }

  #Calculating the coordinates for the random rock
  for(i in which(shape_s$strat == F & shape_s$skel_number != 0)){
    shape_s$geometry[i] <- sf::st_sample(shape_s$geometry[i], 
                                     shape_s$skel_number[i]) %>% 
      sf::st_union()
    
  }

  #extract the point geometries (the coordinates in which the rock is placed)
  spoint <-  sf::st_intersection(shape_s,  shape_mod$geometry) %>% 
    sf::st_collection_extract(type = "POINT") %>%  
    sf::st_cast("POINT") %>% 
    dplyr::group_by_(~name) %>% 
    #inserting a random parameter of the rock surface 
    dplyr::mutate_(random_part = ~ runif(n(), min = skel_dim_from, max = skel_dim_to)) %>% 
    dplyr::mutate_(random_part = ~ random_part/sum(random_part)) %>% 
    dplyr::mutate_(area_size = ~ area_comp * random_part) %>% 
    dplyr::ungroup()

  spoint_poly <- point_2_polygon(sf_point = spoint)
  
  #clip the rockshape with the origen geometry
  spoint_clean <-  spoint_poly %>%
    sf::st_intersection(shape_mod$geometry)

  #smoth the rock geometry
   spoint_clean[which(spoint_clean$smooth == T),] <- 
     smoothr::smooth(spoint_clean[which(spoint_clean$smooth == T),])#,
                     #method = "ksmooth")
   
   #union the geometrys
   poly1 <- spoint_clean %>% 
     dplyr::select_(~name, ~ union) %>% 
     dplyr::filter(union == TRUE) %>% 
     dplyr::group_by(~ name) %>% 
     dplyr::summarise()   

   poly2 <- spoint_clean %>%
     dplyr::filter(union == FALSE) %>% 
     dplyr::select_(~name) 
   
   erg <- rbind(poly1, poly2)

  return(erg)
}

