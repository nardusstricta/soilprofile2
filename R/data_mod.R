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
#' }
#' @return A data frame with the following additional columns:
#' depth: ("from1", "to1"), "value_col", "chroma_col", "rgb_col" (calculated using the function \link[aqp]{munsell2rgb}, nameC (A new name column, the old one was replaced with an numeric id))
#' @examples 
#' data_example <- data.frame(name = c("Ah", "Bv"),
#' depth = c("0-22", "22-33.434"), 
#' col = c("10YR 4/3", "5Y 5/3"))
#' print(data_mod(data_example))
#' @export
#' @importFrom tidyr separate gather
#' @importFrom aqp munsell2rgb
#' @importFrom magrittr "%>%"


data_mod <- function(df_org){
  df_org %>% 
    tidyr::separate(depth, c("from1", "to1"), "-") %>% 
    dplyr::mutate(nameC = name) %>%
    dplyr::mutate(name = 1:nrow(df_org)) %>%
    dplyr::mutate(from1 = as.numeric(from1)) %>% 
    dplyr::mutate(to1 = as.numeric(to1)) %>%
    tidyr::separate(col, c("hue_col", "temp"), " ") %>% 
    tidyr::separate(temp, c("value_col", "chroma_col"), "/") %>% 
    dplyr::mutate(value_col = as.numeric(value_col)) %>% 
    dplyr::mutate(chroma_col = as.numeric(chroma_col)) %>% 
    dplyr::mutate(rgb_col = aqp::munsell2rgb(hue_col, value_col, chroma_col))
}
