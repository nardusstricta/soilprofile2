#' Add buffer properties to soil horizonts
#' Funktion to modifide the transition of Soil horizon, by adding the Buffer- size, -number, and Shape of the Polygons, wich will be lay in the other horizont. 
#' @param df.test Dataframe with horizts as row and properties as colum
#' @return new dataframe with add properities
#' @author Gabriel Holz
#' @import tidyverse
#' @import aqp
#' @export

data_mod <- function(df.test, 
                     buffer_size = c(0, 0, 0, 2, 3), 
                     buffer_number = c(0,0,0,10,19),
                     nSides = c(0,0,0,3,25)){
  if(length(buffer_size) != nrow(df.test) | length(buffer_number) != nrow(df.test)|
     length(nSides) != nrow(df.test)){
    return(warning("The lenth of one variable is not equal to df.test"))
  }else{
    df <- df.test %>% 
      separate(depth, c("from1", "to1"), "-") %>% 
      mutate(nameC = name) %>%
      mutate(name = 1:nrow(df.test)) %>%
      mutate(from1 = as.numeric(from1)) %>% 
      mutate(to1 = as.numeric(to1)) %>% 
      separate(col, c("hue_col", "temp"), " ") %>% 
      separate(temp, c("value_col", "chroma_col"), "/") %>% 
      mutate(value_col = as.numeric(value_col)) %>% 
      mutate(chroma_col = as.numeric(chroma_col)) %>% 
      mutate(rgb_col = aqp::munsell2rgb(hue_col, value_col, chroma_col)) %>% 
      mutate(buffer_size = buffer_size) %>% #Auzahl 
      mutate(buffer_number = buffer_number) %>% #Anzahl der Punkte
      mutate(nSides = nSides)
  }
  return(df)
}
