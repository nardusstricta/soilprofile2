#' creates a sf line 
#'
#' This is a simple function that creates from the horizont coordinates a sf line. 
#' In zukunft müssen die Parameter für die Form der Linie hier einstellbar sein
#' 
#' @param df_polygon A character string giving the text the function will print
#' @param mat_line A character string giving the text the function will print
#' @param sm A character string giving the text the function will print
#' 
#'
#' @return This function returns a sf-file, which one line for each horizont
#' @import tidyverse
#' @import sf
#' @import smoothr
#'
#' @export

line_mod <- function(df_polygon, mat_line, sm = T, seed = 33){
  tempX <- df_polygon %>% 
    group_by(name) %>% 
    filter(y == max(y)) %>% 
    select(x, y, name) %>% 
    left_join(mat_line, by = "name") %>% 
    ungroup()
  
  #generate new X values, depends of mat_line number X for each group
  set.seed(seed)

  new.df  <- data.frame(name = expand_x(numberX = mat_line$numberX, name = mat_line$name)) %>% 
    left_join(tempX, by = "name") %>% 
    group_by(name) %>% 
    mutate(x = sample(seq(min(x),max(x), .1), n(), replace = T)) %>% 
    mutate(y = rnorm(n(), mean = max(y), sd = sd)) %>% 
    union_all(tempX) %>% 
    arrange(x) %>% 
    dplyr::select(x, y, name) 

  
  #newdatat Line:
  df_line <- new.df %>% 
    st_as_sf(coords = c("x", "y")) %>%
    group_by(name) %>%
    summarise(do_union = F) %>% 
    st_cast("LINESTRING") 
  
  if(sm == T) {
      df_line <- smooth(df_line, method = "ksmooth")
    }
  
  return(df_line)
}
