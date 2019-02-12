#' Helper function to build polygons by points 
#'
#' This function builds polygons with given area and number of sides. The starting point is one of the corner points of the polygon. 
#' 
#' @param sf_point A simple feature with at least the following parameter as colnames: 
#'  
#' @return A sf polygon layer with one line for each input point 


point_2_polygon <- function(sf_point){
  
  stopifnot("sf" %in% class(sf_point))
  stopifnot("nSides" %in% colnames(sf_point))
  stopifnot("area_size" %in% colnames(sf_point))
  
  
  xy_cord <- sf_point %>%  #Koordinaten für die Funktion extrahieren:
    sf::st_coordinates()
  
  df_attr <- sf_point 
  sf::st_geometry(df_attr) <- NULL
  

  geom <- do.call(
    rbind, lapply(
      1:nrow(xy_cord), function(i){
        convex_poly(cord = xy_cord[i,], attr_poly = df_attr[i,], poly_id = i)
      }
    )
  )
  
  spoint_poly <- sf::st_sfc(
    lapply(
      1:max(geom[,3]), function(x) sf::st_linestring(geom[geom[,3] == x, 1:2]) %>% 
        sf::st_cast("POLYGON")
    )
  ) %>% 
    sf::st_sf(df_attr, geometry = .) %>% 
    sf::st_buffer(0.0)
}



#' Helper function to extract X and Y coordinates and create ellipses 
#'
#' This function builds polygons with given area and number of sides. The starting point is one of the corner points of the polygon. 
#' 
#' @param cord A numeric vector with the point coordinates specifying the position of the polygon. 
#'  
#' @param attr_poly the parameters described in function \link[soilprofile2]{skeleton}  
#' @param poly_id an id ...
#' @return a matrix with the x and y coordinates and an id for each polygon 


convex_poly <- function(cord, attr_poly, poly_id){
  stopifnot("numeric" == class(cord))
  
  
  if(any(attr_poly$nSides < 3 & attr_poly$nSides > 0)){
    warning("Possibly the polygons are outside the buffer area!")
  }
  
  #checking if parameters are available for stratified rock 
  area <- attr_poly$area_size
  nSides <- attr_poly$nSides
  if(any(c("phi", "vertex") %in% colnames(attr_poly)) == T){
    
    phi <- attr_poly$phi 
    a <- 5  #attr_poly$a
    b <- 3  #attr_poly$b
    
    if(phi == 0){
      points_list1 <-  poly_fun(nSides = nSides, area = area)
    }else{
      t <- seq(0, 2 * pi, length.out = attr_poly$nSides)
      x <- a * cos(t) * cos(phi) - b * sin(t) * sin(phi)
      y <- a * cos(t) * cos(phi) + b * sin(t) * cos(phi)
      y <- rnorm(y, y, .01)
      x <- rnorm(x, x, .1)
      points_list1 <- list(x=NULL, y= NULL)
      points_list1$x <- x
      points_list1$y <- y
      nSides <- length(t)
    }
  }else{
     points_list1 <-  poly_fun(nSides = nSides, area = area)
  }
  
  
  # Find the area of the polygon
  m <- matrix(unlist(points_list1), ncol=2)
  m <- rbind(m, m[1,])
  
  current_area <- 0.5 * (sum(m[1:nSides, 1] * 
                               m[2:(nSides + 1),2]) - 
                           sum(m[1:nSides, 2] * m[2:(nSides + 1), 1]))
  
  points_list1$x <- points_list1$x * sqrt(area/current_area)
  points_list1$y <- points_list1$y * sqrt(area/current_area)

  p <- data.frame(x = points_list1$x + cord[1],
                  y= points_list1$y + cord[2],
                  name = poly_id) 
  
  poo <- rbind(p, p[1,])
  
  return(as.matrix(poo))
  
}



#' Helper function to calculate a polygon by given number of sides and area.  
#'
#' This function builds polygons with given area and number of sides. The starting point is one of the corner points of the polygon. 
#' The area (A) of a convex polygon can be found with the following formula  
#' \eqn{A = 1/2 * [(x1*y2 + x2*y3 + ... + xn*y1) - (y1*x2 + y2*x3 + ... + yn*x1)]}
#' 
#' @param nSides number of sides  
#' @param area the area of the polygon
#' @return A list with the x and y coordinates
#' @export

poly_fun <- function(nSides, area){
  stopifnot(class(nSides) == "numeric" & class(area) == "numeric")
  radius <- sqrt((2 * area) / (nSides * sin((2 * pi) / nSides)))
  angle <- (2 * pi) / nSides
  # Randomize the radii/angles
  radii <- rnorm(nSides, radius, radius/10)
  angles <- rnorm(nSides, angle, angle/10) * 1:nSides
  angles <- sort(angles)
  
  points <- list(x=NULL, y= NULL)
  points$x <- cos(angles) * radii 
  points$y <- sin(angles) * radii
  return(points)
}

