#' par_default 
#'
#' This function draws a set of randompolygons.
#'
#' @param polygon A simple feature file with plot information see \link[soilprofile2]{apply_texture}
#' @param size default size
#' @param outline_col dafault outline color of the horizont
#' @return A unit multipolygon (sf) with one parID column
#'
#' @export


par_default <-  function(polygon, size = 0.5, outline_col = "grey"){
  
  #Füllen der Standardeinstellungen:
  polygon <- polygon %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate_(col = 
                     ~ ifelse(par_ID != 1 & is.na(col),
                    "grey", 
                    col
             ),
           col = 
             ~  ifelse(par_ID == 1,
                       outline_col, 
                       col
             ),
           bgc = 
             ~ ifelse(sf::st_geometry_type(geometry) == "MULTIPOLYGON" & is.na(bgc),
                      "grey40", 
                      bgc
             ),
           bgc = ~ ifelse(sf::st_geometry_type(geometry) == "POLYGON", 
                          as.character(rgb_col),
                          bgc
           ),
           linetype = 
             ~ ifelse(sf::st_geometry_type(geometry) == "MULTILINESTRING" & is.na(linetype) & par_ID != 1,
                    as.integer(sample(1:6, 1)), 
                    1
             ),
           pch = ~ ifelse(sf::st_geometry_type(geometry) == "MULTIPOINT" & is.na(linetype) & par_ID != 1,
                        as.integer(sample(25, 1)), 
                        19
           ), 
           size = ~ ifelse(sf::st_geometry_type(geometry) == "POINT",
                           rgamma(1, shape = as.numeric(grain_size), rate = as.numeric(grain_sd)), 
                           size
           )
           
    ) %>% 
    sf::st_sf()
  
  return(polygon)
}

#' soil_theme 
#'
#' a function to call the theme settings of ggplot. The most important is the absence of the X-axis. 
#'
#' @return ggplot themes
#'
#' @export

soil_theme <- function(){
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank())
  
}

#' soil_legend 
#'
#' a simple function to visualize the legend in an own "ggplot" plot. 
#' @param df_legend A data frame with aesthetics for the legend. In the simplest case the table ("process_symbols") stored in the folder data. 
#'
#' @return ggplot
#' @examples 
#' data("process_symbols")
#' soil_legend(process_symbols)
#'
#' @export
soil_legend <- function(df_legend){
  ggplot2::ggplot() + 
    ggplot2::scale_y_discrete(name = "", position = "right") +
    ggplot2::scale_x_continuous(limits=c(0,10), breaks=NULL, name= "") +
    ggplot2::scale_shape_discrete(solid=T, guide=FALSE) +
    ggplot2::geom_point(data = df_legend,
                        mapping = ggplot2::aes_(x = ~ 5, y = ~ name),
                        shape = df_legend$shape,
                        fill = df_legend$fill, 
                        col = df_legend$col, 
                        stroke = df_legend$stroke,
                        size=4) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size=13),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank()) +
    ggplot2::ggtitle("Legend")
  
}

