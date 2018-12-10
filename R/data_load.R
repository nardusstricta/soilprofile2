#' Load example profildata
#' 
#' The package contains example profile data which can be loaded with this function. 
#' @param Profile The name of the Profile. One of the following names can be selected: "P1", "SG2", "SG5", "SG7"
#' @return A dataframe
#' @export

data_load <- function(Profile = "SG2"){
  tmp <- example
  class(tmp) <- 'data.frame'
  df <- tmp[which(tmp$Profile == Profile),] 
  if(nrow(df)==0){
    return(warning("This name is not in the example Data"))
  }else{
    return(df)
  }
}

