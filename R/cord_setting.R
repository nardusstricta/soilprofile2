#' setting coordinates 
#'
#' This function sets four coordinates (x and y) for each horizon. This makes it easy to calculate a polygon. e.g. as Simple Features
#'
#' @param df A dataframe, which was normaly build by the function \link[soilprofile2]{data_mod}. The rows represent the horizons and the columns the properties.
#' The data frame should contain at least one column with an id ("name"). One column with the mininal depth ("from1") and one with the maximum depth ("to1")
#' @param plot_width A numeric value that sets the relative width of the plot. The default is 3  
#'
#' @return This function returns a dataframe with four coordinates for each horizon. Additionally a column with the ID, which can be joind to the source data 
#'
#' @examples 
#' data_example <- data.frame(name = c(1, 2),
#' from1 = c(0,20),
#' to1 = c(20, 40)
#' )
#' 
#' cord_example <- cord_setting(data_example, plot_width = 3)
#' print(cord_example)
#' 
#' @export
cord_setting <- function(df, plot_width = 3){
  broad  <- max(df$to1)/plot_width #Setting the width 
  df_select <- df %>% 
    dplyr::select(from1, to1, name) %>% 
    dplyr::mutate(x1 = 0) %>% 
    dplyr::mutate(y1 = -from1) %>% 
    dplyr::mutate(x2 = broad) %>% 
    dplyr::mutate(y2 = -from1) %>% 
    dplyr::mutate(x3 = broad) %>% 
    dplyr::mutate(y3 = -to1) %>% 
    dplyr::mutate(x4 = 0) %>% 
    dplyr::mutate(y4 = -to1) %>% 
    dplyr::mutate(x5 = 0) %>% 
    dplyr::mutate(y5 = -from1) %>% 
    dplyr::select(-from1, -to1)
  
  df_gatherY <- df_select %>% 
    dplyr::select(-seq(2,10,2)) %>% 
    tidyr::gather("Wert", "y", 2:6) 
  
  df_gatherX <- df_select %>% 
    dplyr::select(-seq(3,11,2)) %>%
    tidyr::gather("Wert", "x", 2:6)
  
  
  df_polygon <- data.frame(x = df_gatherX$x,
                           y = df_gatherY$y, 
                           name = df_gatherX$name)
  return(df_polygon)
  
}
