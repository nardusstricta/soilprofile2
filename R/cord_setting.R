#' "Coordinate creator"
#'
#' This is a simple function that, build coordinates for each horizont. So 
#' With this you can easily build sf files 
#'
#' @param database A dataframe, which was build by the function \link[soilprofile2]{data_mod} 
#' @param plot_width A numeric value that determines the relative width of the  representation.  
#'
#' @return This function returns a dataframe with four coordinates for each horizon. Additionally a column with the ID, which can be joind to the source data 
#'
#' @examples
#' cord_setting(database = df, plot_width = 3)
#'
#' @export
cord_setting <- function(database = df, plot_width = 3){
  df <- database
  broad  <- max(df$to1)/plot_width #Einstellung der Breite
  df_select <- df %>% 
    dplyr::select(from1, to1, name) %>% 
    mutate(x1 = 0) %>% 
    mutate(y1 = -from1) %>% 
    mutate(x2 = broad) %>% 
    mutate(y2 = -from1) %>% 
    mutate(x3 = broad) %>% 
    mutate(y3 = -to1) %>% 
    mutate(x4 = 0) %>% 
    mutate(y4 = -to1) %>% 
    mutate(x5 = 0) %>% 
    mutate(y5 = -from1) %>% 
    select(-from1, -to1)
  
  df_gatherY <- df_select %>% 
    dplyr::select(-seq(2,10,2)) %>% 
    gather("Wert", "y", 2:6) 
  
  df_gatherX <- df_select %>% 
    dplyr::select(-seq(3,11,2)) %>%
    gather("Wert", "x", 2:6)
  
  
  df_polygon <- data.frame(x = df_gatherX$x,
                           y = df_gatherY$y, 
                           name = df_gatherX$name)
  return(df_polygon)
  
}
