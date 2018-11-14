#' wrapper function for shape_mod
#'
#' This function draws a Ah pattern, with the following  helper fuction \link[soilprofile2]{basic_point}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' 
#' @return A unit sf layer with an parID column
#'
#' @export

sf_soil <- function(data, plot_width = 3, line_sm = T, Line = F){
  
  df <- data_mod(data)
  
  df_polygon <- cord_setting(database = df, plot_width = plot_width)
  
  shape_areas <- sf_polgon(database = df, df_polygon = df_polygon)
  
  mat_line <- data.frame(name= unique(df$name), numberX = df$numberX, 
                         sd = df$sd)
  
  df_line <- line_mod(df_polygon = df_polygon, mat_line = mat_line, sm = line_sm) 
  
  shape_mod  <- split_polygon(Polygon = shape_areas, Line = df_line)
  
  if(Line == T){
    return(df_line)
  }else{
    return(shape_mod)
  }
  
  
}
