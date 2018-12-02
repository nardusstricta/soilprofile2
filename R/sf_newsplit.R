#' wrapper function for shape_mod
#'
#' This function draws a Ah pattern, with the following  helper fuction \link[soilprofile2]{basic_point}
#'
#' @param shape_areas A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param mat_line
#' @param line_sm
#' @param Line
#' @return A unit sf layer with an parID column
#'
#' @export

sf_newsplit <- function(shape_areas, mat_line, line_sm = T, Line = F){ #line_sm muss noch vectorisiert werden!!
  df_line <- line_mod(df_polygon = shape_areas, mat_line = mat_line, sm = line_sm) 
  
  shape_mod  <- split_polygon(Polygon = shape_areas, Line = df_line)
  
  if(Line == T){
    return(df_line)
  }else{
    return(shape_mod)
  }
  
}
