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


save_par_setting <- function(name, variable, value, par_ID){
  df_par <- data_frame("nameC" = name,
                       "variable" = variable, 
                       "value" = value,
                       "par_ID" = par_ID) 
  
  df_par_wide_temp <- spread(df_par, variable, value, convert = T)
  df_par_wide_temp$par_ID <- as.numeric(df_par_wide_temp$par_ID)
  
  df_par_wide <- df_par_wide_temp %>% 
    bind_rows(df_par_wide)
  
  usethis::use_data(df_par_wide, overwrite = TRUE)
}




