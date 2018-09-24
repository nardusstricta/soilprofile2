#' creates a sf line 
#'
#' This is a simple function that creates from the horizont coordinates a sf line. 
#' In zukunft müssen die Parameter für die Form der Linie hier einstellbar sein
#' 
#' @param df_polygon A character string giving the text the function will print
#' 
#'
#' @return This function returns a sf-file, which one line for each horizont
#' @import tidyverse
#' @import sf
#'
#' @export
line_mod <- function(df_polygon){
  tempX <- df_polygon %>% 
    group_by(name) %>% 
    filter(y == max(y)) %>% 
    select(x, y, name)
  
  #generate new X
  new_gatherX <- tempX %>% 
    group_by(name) %>% 
    do(genX_fun(.$x,.$name)) %>% 
    gather("Nummer", "x", 2:13)
  
  #generate new Y
  new_gatherY <- tempX %>% 
    group_by(name) %>% 
    do(genY_fun(.$y)) %>%
    gather("Nummer", "y", 2:13)
  
  new.df <- data.frame(x = new_gatherX$x,
                       y = new_gatherY$y, 
                       name = new_gatherY$name)
  
  new.df <- new.df %>% 
    na.omit()
  
  #newdatat Line:
  df.line <- new.df %>% 
    st_as_sf(coords = c("x", "y")) %>%
    group_by(name) %>%
    summarise(do_union = F) %>% 
    st_cast("LINESTRING")
  return(df.line)
}
