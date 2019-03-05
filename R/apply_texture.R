#' This is a wrapper function which applies the texture function 
#' 
#'  A set of functions is stored in the package.
#'  These functions represent specific patterns for each horizon 
#'  and are named according to the horizons. Pay attention! don't write functions with the names that already exist in R for example "C".
#'  
#'  df_par_wide is a tibble with the graphical parameters in wide format (saved in the sys.data). 
#'  The key to join the tables is the "name" column and the "par_Id" column. 
#'  The "par_Id" column starts with one for the background layer and then ascends to the top layer. 
#'  An example tibble, which can be extended with the function \link[soilprofile2]{save_par_setting},
#'  is stored in the package (sysdata.rda)
#'  
#'  The actual task is to link the horizon rows to the corresponding functions and 
#'  then apply these functions
#'  
#'  If a horizonname do not matched it 
#'  will be used the \link[soilprofile2]{random_line_pattern} function. If the information about the soil texture is available, the texture function \link[soilprofile2]{fun_grain_size} is preferred. 
#'
#'   
#' 
#' @param shape A simple features file with one row for each hoizon. At least with one geometry column and one "nameC" column ("C" for character). The variables clay silt and sand are optional and will be specified in the respective proportions. The sum must be one. if at least one value isn't zero, a point grid is created for the texture. With the function par_dafault the grain size can be changed. 
#' @param buffer Usually a negative value that indicates the distance between the pattern and the horizon boundary. 
#' @param dist_size distance between texture points
#' @param background whether the points should have a background (only if grain_size information exists). Either a string with 0 and 1 or the default "random", then the values are selected randomly. 
#' 
#' @return This function returns a new Simple Features which
#'  contains all informations (columns) of the Input Simple Features (shape).
#'  Columns with the graphic parameters are added. 
#'  Adding patterns to the given layer can result in multiple geometries.  (e.g. points and lines). 
#'  To save this information, rows are added to the dataset.  
#' @examples 
#'
#' #create an example dataset and modify the color and depths
#' #The columns grain_size and grain_sd specify the texture. 
#' #If the value is not zero or NA, a point grid is created for the texture.
#' #With the function par_dafault the grain size can be changed. 
#' #Only for the "AhBv" horizontal there is a function in the package.
#' #For the other horizons the texture is drawn.
#' 
#' library(dplyr) 
#' library(ggplot2)
#' df_example <-  data.frame(name = c("Bt", "AhBv", "Cve"),
#'                           from = c(0, 15, 43.4),
#'                           to = c(15, 43.4, 70),
#'                           col = c("7.5YR 2/1","10YR 4/3", "2.5Y 5/3"),
#'                           skel_dim = c(".5-1","1-2", "2-3"),
#'                           skel_ab = c(0.2, 0.6, 1.9),
#'                           clay = c(.7, 0, 0),
#'                           silt = c(.2, 0, 0), 
#'                           sand = c(.1, 0, 1)) %>%
#'   data_mod()
#' 
#' #Set coordinates, four points on each horizon
#' cord_example  <-  cord_setting(df_example, plot_width = 2)
#' 
#' #create a simple feature: Each line represents a horizon
#' #with one polygon as geometry.
#' sf_example <- sf_polygon(df_geom = cord_example,
#'                          df_attri = df_example)
#' 
#' ## Creation of the patterns
#' texture_example <- apply_texture(shape = sf_example,
#'                                  buffer = -1.3)
#' 
#' ## Plotting Data
#' library(ggplot2)
#' texture_example %>%
#'   ggplot() +
#'   geom_sf() +
#'   soil_theme()
#' 
#' @export
#' @author Gabriel Holz


apply_texture <- function(shape, buffer = -1, dist_size = 1, background = "random"){
  
  stopifnot("sf" %in% class(shape))
  stopifnot("nameC" %in% colnames(shape))
  
  attr_shape <- data.frame(
    rgb_col = shape$rgb_col, 
    nameC = shape$nameC, 
    clay = ifelse(rep("clay" %in% names(shape), nrow(shape)), shape$clay, 0), 
    silt = ifelse(rep("silt" %in% names(shape), nrow(shape)), shape$silt, 0),
    sand = ifelse(rep("sand" %in% names(shape), nrow(shape)), shape$sand, 0)
  )
  
  #to get the maximum of grain:
  help_mat <- matrix(NA, nrow(attr_shape), 3)
  help_mat <- attr_shape[,3:5]
  t1 <- numeric(nrow(attr_shape))
  for (i in 1:nrow(help_mat)){
    if(max(help_mat[i,]) == 0){
      t1[i] <- 0
    }else{
      t1[i] <- max(which(help_mat[i,]!= 0))
    }
    
  }
  t1 <- t1 * dist_size  
  texture_sf <- shape %>% 
    dplyr::mutate_(background = ~ ifelse(background == "random",
                                         sample(0:1, nrow(.),
                                                replace = TRUE), background)) %>%
    dplyr::mutate_(grain_size = ~t1) %>%
    dplyr::rowwise() %>% 
    dplyr::mutate_(grain_size = ~ ifelse(!any(c("clay", "silt", "sand")) %in%
                                         names(.), grain_size, 0)) %>% 
    dplyr::mutate_(fun = ~ if_else(grain_size != 0, "fun_grain_size", 
                                "random_line_pattern")) %>%
    dplyr::mutate_(fun = ~ ifelse(exists(as.character(nameC)),
                                   c(get(as.character(nameC))),
                                   c(get(as.character(fun))))
                   ) %>%
    dplyr::ungroup()
  
  
  temp_geom <- do.call(
    rbind, lapply(
      1:nrow(texture_sf), function(i){
        texture(shape = texture_sf[i,],
                fun_horizont = texture_sf$fun[[i]],
                buffer = buffer,
                if(texture_sf$grain_size[i] != 0){
                  par_size = texture_sf$grain_size[i]
                } else NA,
                if(texture_sf$grain_size[i] != 0){
                  background = texture_sf$background[i] 
                } else NA
        )
      }
    )
  )


  ##
  #add graphic pars:
  ##
  
  erg <- temp_geom %>% 
    dplyr::left_join(df_par_wide, 
                     by = c("nameC", "par_ID")) %>% 
    dplyr::left_join(attr_shape, by = "nameC")

  
  return(erg)
}

