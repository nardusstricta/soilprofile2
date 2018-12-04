#' wrapper function for shape_mod
#'
#' This function draws a Ah pattern, with the following  helper fuction \link[soilprofile2]{basic_point}
#'
#' @param data A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param plot_widt The width of the plot
#' @return A unit sf layer with an parID column
#'
#' @export

sf_soil <- function(data, plot_width = 3, line_sm = T, Line = F, sdC, ...){
  
  df <- data_mod(data)
  
  df_polygon <- cord_setting(database = df, plot_width = plot_width)
  
  shape_areas <- sf_polgon(database = df, df_polygon = df_polygon)
  
  if(any(colnames(df) %in% "name") && 
     any(colnames(df) %in% "numberX") &&
     any(colnames(df) %in% "sd")){
    mat_line <- reactive(data.frame(name= unique(df$name), numberX = df$numberX, sd = c(0,0,0,1, 1)))
  }else{
    mat_line <- ...
  }
  
  
  df_line <- line_mod(df_polygon = df_polygon, mat_line = mat_line(), sm = line_sm) 
 
  
  shape_mod  <- split_polygon(Polygon = shape_areas, Line = df_line)
  
  if(Line == T){
    return(df_line)
  }else{
    return(shape_mod)
  }
  
  return(shape_areas)
}

