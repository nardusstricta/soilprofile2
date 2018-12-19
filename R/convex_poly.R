#' Helper function from \link[soilprofile2]{smoth_trans} 
#'
#' This function builds polygons with given area and number of sides, with an given start point
#' The area of a convex polygon can be found as: 
#' deqn{ A = 1/2 * [(x1*y2 + x2*y3 + ... + xn*y1) - (y1*x2 + y2*x3 + ... + yn*x1)]}
#' 
#' @param nSides The number of Sides
#' @param area The area size 
#' @param xstart startet X Coordinats,  see \link[soilprofile2]{smoth_trans}
#' @param ystart startet Y Coordinats,  see \link[soilprofile2]{smoth_trans}
#' @return A sl polygon layer with one line for each input point 
#'
#'
#' @export

convex_poly <- function(data_skel){
  if(data_skel$nSides < 3){
    warning("Possibly the polygons are outside the buffer area!")
  }
  
  # Find the radius of the circumscribed circle, 
  # and the angle of each point if this was a regular polygon
poly_fun <- function(nSides, area){
  radius <- sqrt((2*area)/(nSides*sin((2*pi)/nSides)))
  angle <- (2*pi)/nSides
  
  # Randomize the radii/angles
  radii <- rnorm(nSides, radius, radius/10)
  angles <- rnorm(nSides, angle, angle/10) * 1:nSides
  angles <- sort(angles)
  
  points <- list(x=NULL, y= NULL)
  points$x <- cos(angles) * radii 
  points$y <- sin(angles) * radii
  return(points)
}


  area <- data_skel$area_size
  nSides <- data_skel$nSides
  if(any(c("phi", "vertex") %in% colnames(data_skel)) == T){
    
    phi <- data_skel$phi 
    a <- 5#data_skel$a
    b <- 3#data_skel$b
    
    if(phi == 0){
      points_list1 <-  poly_fun(nSides = nSides, area = area)
    }else{
      t <- seq(0, 2 * pi, length.out = data_skel$nSides)
      x <- a*cos(t)* cos(phi) - b * sin(t) * sin(phi)
      y <- a*cos(t)* cos(phi) + b * sin(t) * cos(phi)
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
  current_area <- 0.5 * (sum(m[1:nSides,1] * 
                               m[2:(nSides+1),2]) - 
                           sum(m[1:nSides,2] * m[2:(nSides+1),1]))
  
  points_list1$x <- points_list1$x * sqrt(area/current_area)
  points_list1$y <- points_list1$y * sqrt(area/current_area)
  p <- data.frame(x = points_list1$x + data_skel$X, y= points_list1$y + data_skel$Y)
  poo <- rbind(p, p[1,])
  po2 <- sf::st_linestring(as.matrix(poo)) %>% 
    sf::st_cast('MULTIPOINT') %>% 
    sf::st_cast("POLYGON") %>% 
    sf::st_convex_hull()
  
  return(po2)
  
}
