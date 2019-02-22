#' creates a line as simple feature
#'
#' This is a simple function that creates a simple feature line from the horizont coordinates. 
#' 
#' @param df_geom A long table with the columns named "name", "x" and "y" (Id and coordinates) for each horizont. For example created by the \link[soilprofile2]{cord_setting} function 
#' @param line_attri A datan frame with the information about the lines which divide the horizons. 
#' The following columns must exist:
#' \enumerate{
#'   \item The horizon Id must be given in a numeric column named "name"
#'   \item The number of X points, which should deviate the line from a straight line, must be given in a numeric column named: "numberX"
#'   \item The distance between the X points and the straight line must be given in a numeric column named: "sd". 
#'   The values are calculated using the \link[stats]{rnorm} function. The mean value is given by the X-value of the straight horizon line. And the standard deviation can be selected individually. 
#'   \item The column named "sm" can be used as a logical value to specify whether the horizon transition should be smoothed. This is done with the function \link[smoothr]{smooth}.
#' } 
#' @param seed For replicable examples a seed can be set here 
#' 
#'
#' @return This function returns a linestring simple feature. Each line represents the upper horizontal limit of the corresponding id.
#' 
#' @examples 
#' 
#' ## Example data
#' data_example <- data.frame(name = c(1, 2),
#'                           from1 = c(0,20),
#'                           to1 = c(20, 40)
#')
#'
#'##Coordination setting
#'cord_example <- cord_setting(data_example, plot_width = 3)
#'
#'## Line attributes data frame:
#'lattri_example <- data.frame(name= unique(cord_example$name),
#'                             numberX = c(2, 10), 
#'                             sd = c(1,1),
#'                             sm = c(TRUE, TRUE)
#')
#'
#'## Apply the line_mod fuction
#'line_example <- line_mod(df_geom = cord_example, line_attri = lattri_example)
#'
#'## Plot simple feature geometry
#'plot(line_example$geometry)
#'
#' @export

line_mod <- function(df_geom, line_attri, seed = 33){
  y = NULL #filter_ not working
  tempX <- df_geom %>% 
    dplyr::group_by_(~ name) %>% 
    dplyr::filter(y == max(y)) %>% 
    dplyr::left_join(line_attri, by = "name") %>% 
    dplyr::ungroup()
  
  #generate new X values, depends of line_attri number X for each group
  set.seed(seed)

  new_df_geom  <- data.frame(name = expand_x(numberX = line_attri$numberX,
                                        name = line_attri$name)) %>% 
    dplyr::left_join(tempX, by = "name") %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate_(x = ~ sample(seq(min(x),max(x), .1), n())) %>% 
    dplyr::mutate_(y = ~ rnorm(n(), mean = max(y), sd = sd)) %>% 
    dplyr::union_all(tempX) %>% 
    dplyr::arrange_(~ x) %>% 
    dplyr::select_(~ x, ~ y, ~ name, ~ sm) 

  
  #newdatat Line:
  sf_line <- new_df_geom %>% 
    sf::st_as_sf(coords = c("x", "y")) %>%
    dplyr::group_by_(~ name, ~ sm) %>%
    dplyr::summarise(do_union = FALSE) %>% 
    sf::st_cast("LINESTRING") 
  
  sf_line[which(sf_line$sm == T),] <- smoothr::smooth(sf_line[which(sf_line$sm == T),], method = "ksmooth")
  
  return(sf_line)
}

#' new X-values 
#'
#' generates new X-values for the upper horizon limit 
#'
#' @param numberX A numeric vector that giving the number of points which should be added to the horizont line
#' @param name the name of horizonts
#'
#' @return This function returns a numeric id vector. 


expand_x <- function(numberX, name){
  exp_x <- NULL
  for(i in 1:length(numberX)){
    exp_x <- c(exp_x, rep(name[i], each= numberX[i]))
  }
  return(exp_x)
}
