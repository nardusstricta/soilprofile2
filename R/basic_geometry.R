#' basic random polygon 
#'
#' This function draws a set of randompolygons.
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param size Percentage area of input polygon
#' @param number Number of polygons
#' @param nSides number of polygon sides
#' @param sm smooth by the \link[smoothr]{smooth} function with the methode "ksmooth"
#' 
#' @return A unit multipolygon (sf) with one parID column
#'
#' @export

basic_random_polygon <- function(polygon, size, number, nSides, sm = T){
  stopifnot("sfc_POLYGON" %in% class(polygon))
  
  #eventuell noch zufällige Werte einbauen
  # if(length(size) > 1){
  #   size <- sample(size[1]:size[2], number)
  # }
  # 
  area_size <- sf::st_area(polygon) * size/number
  
  point_samp <- sf::st_sample(polygon, number) %>% 
    st_sf(nSides = nSides, number = number,
          area_size = area_size)

  sf_polygon <- point_2_polygon(point_samp)

  sf::st_is_valid(sf_polygon)
  
  erg <- sf_polygon %>% 
    sf::st_intersection(polygon) %>% 
    sf::st_union() %>% 
    sf::st_sf(parID = 1)
  
  if(sm == T){
    erg  <-  smoothr::smooth(erg)
  }
  
  return(erg)
  
}


#' basic polygon 
#'
#' This function draws a polygon, with the following  helper fuction \link[sf]{st_make_grid}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' 
#' @return A unit polygon (sf) with an parID column
#'
#' @export

basic_polygon <- function(polygon, cellsize, square = F){
  sf::st_make_grid(polygon, what = "polygons", cellsize = cellsize,  square = square) %>% 
    sf::st_union()%>% 
    sf::st_sf(parID = 1)
}

#' basic line 
#'
#' This function draws a line-layer, with the following helper fuction \link[sf]{st_bbox}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param rotation The angle of the line
#' @param cellnumber The distance between the lines
#' @return A unit point layer (sf) with one rown in the parID column
#'
#' @export

basic_line <- function(polygon, cellnumber = 22, rotation = 12){
  x <- sf::st_bbox(polygon)[c("xmin", "xmax")]
  x["xmin"] <- x["xmin"] - rotation
  x["xmax"] <- x["xmax"] + rotation 
  x <- rep(rep(seq(x[1], x[2],
                   length.out = cellnumber),2), each =2)
  
  x[seq(2,length(x), 2)] <- x[seq(2,length(x), 2)] + rotation
  df <- data.frame(x = x,
                   y = rep(st_bbox(polygon)[c("ymin", "ymax")], 
                           cellnumber * 2),
                   id = rep(1:cellnumber, each = 2)
  )
  
  line_sf <- df %>% 
    sf::st_as_sf(coords = c("x", "y")) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(do_union = F) %>% 
    sf::st_cast("LINESTRING") %>% 
    sf::st_cast("MULTILINESTRING") %>% 
    sf::st_union() %>% 
    sf::st_sf(parID = 1)
  return(line_sf)
  
}

#' basic point 
#'
#' This function draws a pointlayer, with the following helper fuction \link[sf]{st_sample} by random points and \link[sf]{st_make_grid}
#'
#' @param polygon A polygon-layer, which has been build by the \link[soilprofile2]{texture} function.
#' @param random if random or not
#' @return A unit point layer (sf) with one rown in the parID column
#'
#' @export

basic_point <- function(polygon, cellsize = 12, random = FALSE){
  if(random == T){
    sf::st_sample(polygon, size = cellsize) %>% 
      sf::st_union() %>% 
      sf::st_sf(parID = 1) #geometry colum named "."
  }else{
    sf::st_make_grid(polygon, cellsize = cellsize,  what = "centers", square = TRUE) %>% 
      sf::st_union() %>% 
      sf::st_sf(parID = 1) 
  }
  
}
