#' new Y-values 
#'
#' generates new Y-values for the upper horizon limit 
#'
#' @param Y A numeric vector that giving the start and end of the horizont line
#' @param excited Logical value specifying whether to include an exclamation
#'    point after the text
#'
#' @return This function returns 12 Y-value. 10 of this are new generatet by the \link[base]{rnorm}  Funktion 
#' @usage This function is used internally by the \link[soilprofile2]{line_mod} function. The corresponding Y values are created with the similar function \link[soilprofile2]{genX_fun}.
#' @export

genY_fun <- function(Y){
df_temp2 <- as_data_frame(setNames(as.list(rnorm(12, mean = Y[1], sd = 0.3)), c(1:12)))
}