#' new X-values 
#'
#' generates new X-values for the upper horizon limit 
#'
#' @param numberX A numeric vector that giving the number of points which should be added to the horizont line
#' @param name the name of horizonts
#'
#' @return This function returns a numeric id vector. 
#' @usage This function is used internally by the \link[soilprofile2]{line_mod} function
#' 

expand_x <- function(numberX, name){
  exp_x <- NULL
  for(i in 1:length(numberX)){
    exp_x <- c(exp_x, rep(name[i], each= numberX[i]))
  }
  return(exp_x)
}