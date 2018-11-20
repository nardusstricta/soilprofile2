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
  data("fun_list")
  if(name %in% fun_list$nameC){
    index <- which(fun_list$nameC == name)
    fun_list$fun[[index]] <- fun
    usethis::use_data(fun_list, overwrite = TRUE)
    
  }else{
    fun_list <- tibble("fun" = c(fun),
                       "nameC" = name) %>% 
      bind_rows(fun_list)
    
    usethis::use_data(fun_list, overwrite = TRUE)
  }
  
}



