#' save_horizont_fun
#'
#' This function save the new horizont function in the global list
#'
#' @param name name of the function same to the horizontname
#' @param variable  par variable e.g col
#' @param value value of variable e.g. blue
#' @param par_ID the "id" which defines the layer 
#' @return A unit sf layer with an parID column
#'
#' @export


save_par_setting <- function(name, variable, value, par_ID){
  df_par <- data.frame("nameC" = name,
                       "variable" = variable, 
                       "value" = value,
                       "par_ID" = par_ID) 
  
  df_par_wide_temp <- tidyr::spread_(df_par, "variable", "value", convert = T)
  df_par_wide_temp$par_ID <- as.numeric(df_par_wide_temp$par_ID)
  
  df_par_wide <- df_par_wide_temp %>% 
    dplyr::bind_rows(df_par_wide)
  
  usethis::use_data(df_par_wide, overwrite = TRUE)
}




