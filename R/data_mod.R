#' Transforms the raw data (depth and color) 
#' 
#' This function transforms the raw profiledata. 
#' Funktion to modifide the transition of Soil horizon, by adding the Buffer- size, -number, and Shape of the Polygons, wich will be lay in the other horizont. 
#' @param df_org dataframe with horizts as row and properties as column. At least the following columns should be in the dataframe (as.character):
#' 
#' \enumerate{
#'   \item The horizon name must be given in a column named "name"
#'   \item The depth must be given in a column named "depth" in the following format: "0-20" This example shows a horizon extension from 0 cm to minus 20 cm.
#'   \item The color must be specified in a column named "col" according to the system of Munsell.
#'   e.g. 10YR 5/4 (Important is the blank between the Hue and value and the backslash between the value and the Chroma).
#'   \item The rock content must be indicated in the following form: "2-3". The column must be called skel_dim. 
#' }
#' @return A data frame with the following additional columns:
#' depth: ("from1", "to1"), "value_col", "chroma_col", "rgb_col" (calculated using the function \link[aqp]{munsell2rgb}, nameC (A new name column, the old one was replaced with an numeric id). skel_dim_from and skel_dim_to (seperate the skel dimension column))
#' @examples 
#' data_example <- data.frame(name = c("Ah", "Bv"),
#'                            from = c(0, 22),
#'                            to = c(22, 33.434),
#'                            col = c("10YR 4/3", "5Y 5/3"),
#'                            skel_dim= c("3-30","45-50"))
#' 
#' print(data_mod(data_example))
#' @export


data_mod <- function(df_org){
  #depth = to1 = from1 = NULL
  stopifnot("data.frame" %in% class(df_org))
  stopifnot(c("from", "to", "name", "skel_dim", "col") %in% colnames(df_org))
  
  df_org %>% 
    #tidyr::separate_("depth", c("from1", "to1"), " - ") %>% 
    dplyr::mutate_(nameC = ~ name) %>%
    dplyr::mutate_(name = ~ 1:nrow(df_org)) %>%
    dplyr::mutate_(from1 = ~ as.numeric(from)) %>% 
    dplyr::mutate_(to1 = ~ as.numeric(to)) %>%
    tidyr::separate_("skel_dim",
                     c("skel_dim_from", "skel_dim_to"), "-", convert = T) %>% 
    tidyr::separate_("col", c("hue_col", "temp"), " ") %>% 
    tidyr::separate_("temp", c("value_col", "chroma_col"), "/") %>% 
    ##munsell::mnsl("5Y 5/3") not run error: some colours have chromas that are not multiples of two:
    dplyr::mutate_(value_col = ~ as.numeric(value_col)) %>% 
    dplyr::mutate_(chroma_col = ~ as.numeric(chroma_col)) %>% 
    dplyr::mutate_(rgb_col = ~ aqp::munsell2rgb(hue_col, value_col, chroma_col))
  
}
