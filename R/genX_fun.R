#' new X-values 
#'
#' generates new X-values for the upper horizon limit 
#'
#' @param X A numeric vector that giving the start and end of the horizont line
#' @param excited Logical value specifying whether to include an exclamation
#'    point after the text
#'
#' @return This function returns 12 X-value. 10 of this are new generatet by the \link[base]{runif}  Funktion 
#' @usage This function is used internally by the \link[soilprofile2]{line_mod} function. The corresponding Y values are created with the similar function \link[soilprofile2]{genY_fun}.
#' @export

genX_fun <- function(X, name){
  df_temp1 <- as_data_frame(setNames(as.list(c(min(X), sort(runif(10, min(X), max(X))), max(X))), c(1:12)))
  #df_temp1[,round(runif(max(name), 1,10))] <- NA
  return(df_temp1)
}