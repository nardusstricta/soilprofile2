#' save_horizont_fun
#'
#' This function save the new horizont function in the global list
#'
#' @param fun A function 
#' @param name the name of the function same to the horizontname
#' 
#' @return A unit sf layer with an parID column
#'
#' @export

save_horizont_fun <- function(fun, name){
  fun_list <- tibble("fun" = c(fun),
                          "nameC" = name) %>% 
    bind_rows(fun_list)
    
  devtools::use_data(fun_list, overwrite = TRUE)
}



