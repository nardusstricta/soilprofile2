#' soil skeleton 
#'
#' generates layer for the soil skeleton
#'
#' @param shape_mod A simple feature class object with the base geometry of horizons. At least the following column names must be present in the attributes: horizont id (name), abundanz (skel_ab), minimum dimension in cm (skel_dim_to_from), maximum dimension in cm (skel_dim_to), area of the horizont (area). 
#' 
#' @param skeleton_mat A data frame with the following parameters (as column names) for specifying the rock form: "name" the horizon id as numeric; "nSides" number of sides; \link[smoothr]{smooth} "smooth" logical if should be smooth by; "union" logical value, should the individual stones be connected or each one gets its own outline?; "strat" logical value if the stones are stratified; "cellnumber" numerical value if the stones are stratified in how many horizontal strata should it be; "rotation" Angle of horizontal stratification; "phi" numerical value between 0 and 1. If greater than 0, each stone is drawn in the shape of an ellipse. As the value increases, the ellipse becomes flatter.
#' @param seed set seed for reproducible results
#'
#' @return a simple feature with one row for each horizont. A column with the horizontal ID ("Name") and the geometry column with the skeleton content.
#' @examples  
#' #create an example dataset and modify the color and depths 
#' library(dplyr) 
#' library(ggplot2)
#' df_example <-  data.frame(name = c("Ah", "Bv", "C"),
#'                           from = c(0, 15, 43.4),
#'                           to = c(15, 43.4, 70),
#'                           col = c("7.5YR 2/1","10YR 4/3", "2.5Y 5/3"),
#'                           skel_dim = c(".1-.5","1-2", "2-3"),
#'                           skel_ab = c(0.1, 0.4, .8)) %>%
#'   data_mod()
#' 
#' #Set coordinates, four points on each horizon
#' cord_example  <-  cord_setting(df_example, plot_width = 2)
#' 
#' #create a simple feature: Each line represents a horizon
#' #with one polygon as geometry.
#' sf_example <- sf_polygon(df_geom = cord_example,
#'                          df_attri = df_example)
#' #data frame with specific parameters for the soil skeleton content
#' 
#' skeleton_mat <- data.frame(
#'   name = c(1,2, 3),
#'   nSides = c(4,13, 20),
#'   smooth = c(TRUE, TRUE, TRUE),
#'   union = c(TRUE, FALSE, TRUE),
#'   strat = c(FALSE, TRUE, FALSE),
#'   cellnumber = c(0, 18, 0),
#'   rotation = c(0,20, 0),
#'   phi = c(0, 0, 0)
#' )
#' spoint <- skeleton(shape_mod = sf_example,
#'                    skeleton_mat = skeleton_mat)
#' #Plot the result:
#' sf_example %>%
#'   ggplot() +
#'   geom_sf(fill = sf_example$rgb_col) +
#'   geom_sf(data = spoint) +
#'   soil_theme()
#' @export


skeleton <- function(shape_mod, skeleton_mat, seed = 34){
  
  stopifnot(c("sf", "data.frame") %in% class(shape_mod)  &
              class(skeleton_mat) == "data.frame")
  
  stopifnot(c("name", "skel_ab", "skel_dim_from", "skel_dim_to", "area") %in%
              colnames(shape_mod))
  
  stopifnot(c("name","nSides","smooth", "union", 
              "phi", "strat", "cellnumber", "rotation") %in% 
              colnames(skeleton_mat))

  set.seed(seed)
  shape_s  <- shape_mod %>%
    #select the default parameter: 
    dplyr::select_(~ name, ~ skel_ab, ~ skel_dim_from, ~ skel_dim_to, ~ area) %>% 
    #dplyr::rowwise() %>% 
    dplyr::right_join(skeleton_mat, by = "name") %>% 
    dplyr::mutate_(skel_ab = ~ ifelse(strat == FALSE, 
                                      skel_ab * 0.41149 + 2.02873 * skel_ab ^ 2 + 0.04067, 
                                      skel_ab
                                      )) %>% 
    #calculate the absolute skeleton content
    dplyr::rowwise() %>% 
    dplyr::mutate_(area_comp = ~ (area * skel_ab)) %>% 
    #calculate the mean of skeleton dimension
    dplyr::mutate_(skel_dim_mean = ~ mean(c(skel_dim_from, skel_dim_to), rm.na = T)) %>% 
    #calculate the number of skeleton
    dplyr::mutate_(skel_number = ~ round((area_comp/(skel_dim_mean * skel_dim_mean))))  %>%
    tidyr::replace_na(list(skel_number = 0))  %>% 
    #join with skeleton parameter:
    dplyr::ungroup() %>% 
    sf::st_sf()
  
  #Calculating the coordinates for the stratified rock
  for(i in which(shape_s$strat == T)){ 
    rows <- shape_s$cellnumber[i] 
    shape_s$geometry[i] <- 
      basic_regular_point(shape_s$geometry[i],
                 cellnumber = c(rows, shape_s$skel_number[i]/rows),
                 rotation = shape_s$rotation[i]) %>% 
      sf::st_geometry()
  }

  #Calculating the coordinates for the random rock
  for(i in which(shape_s$strat == F)){
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
     dplyr::select_(~ name, ~ union) %>% 
     dplyr::filter_(~ union == TRUE) %>% 
     dplyr::group_by_(~ name) %>% 
     dplyr::summarise()   

   poly2 <- spoint_clean %>%
     dplyr::filter_(~ union == FALSE) %>% 
     dplyr::select_(~ name) 
   
   erg <- rbind(poly1, poly2) #%>% 
     # dplyr::group_by_(~name) %>% 
     # dplyr::summarise(do_union = F) %>% 
     # dplyr::ungroup()
       

  return(erg)
}

