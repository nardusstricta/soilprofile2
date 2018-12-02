#' wrapper function for shape_mod
#'
#' This function draws a Ah pattern, with the following  helper fuction \link[soilprofile2]{basic_point}
#'
#' @param data A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param plot_widt The width of the plot
#' @return A unit sf layer with an parID column
#'
#' @export

sf_soil <- function(data, plot_width = 3){
  
  df <- data_mod(data)
  
  df_polygon <- cord_setting(database = df, plot_width = plot_width)
  
  shape_areas <- sf_polgon(database = df, df_polygon = df_polygon)
  
  return(shape_areas)
}

