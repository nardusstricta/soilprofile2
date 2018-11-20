#' edit_horizont_fun
#'
#' With this fuction you can edit and save an existing horizont function
#'
#' @param name the name of the function same to the horizontname
#'
#' @export

edit_horizont_fun <- function(name){
  data("fun_list")
  char_name <- name
  index <- which(fun_list$nameC==name)
  #name <- assign(fun_list$nameC[index], fun_list$fun[[index]], envir=.GlobalEnv)
  fun_edit <- fun_list$fun[[index]]
  fun_edit <- fix(fun_edit)
  save_horizont_fun(fun = fun_edit, name = name)
}
