#' Funktion to load example Profils from 
#' @author Gabriel Holz
#' @export


data_load <- function(Profile = "SG2"){
  tmp <- example
  class(tmp) <- 'data.frame'
  df.test <- tmp[which(tmp$Profile == Profile),] 
  if(nrow(df.test)==0){
    return(warning("This name is not in the example Data"))
  }else{
    return(df.test)
  }
}

