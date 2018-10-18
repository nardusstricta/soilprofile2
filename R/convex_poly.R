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
#' @examples
#' smooth_trans()
#'
#' @export

convex_poly <- function(nSides, area, xstart, ystart){
  if(nSides < 7){
    warning("Possibly the polygons are outside the buffer area!")
  }
  
  # Find the radius of the circumscribed circle, 
  # and the angle of each point if this was a regular polygon
  radius <- sqrt((2*area)/(nSides*sin((2*pi)/nSides)))
  angle <- (2*pi)/nSides
  
  # Randomize the radii/angles
  radii <- rnorm(nSides, radius, radius/10)
  angles <- rnorm(nSides, angle, angle/10) * 1:nSides
  angles <- sort(angles)
  
  points <- list(x=NULL, y= NULL)
  points$x <- cos(angles) * radii + xstart
  points$y <- sin(angles) * radii + ystart
  
  # Find the area of the polygon
  m <- matrix(unlist(points), ncol=2)
  m <- rbind(m, m[1,])
  current.area <- 0.5 * (sum(m[1:nSides,1] * 
                               m[2:(nSides+1),2]) - 
                           sum(m[1:nSides,2] * m[2:(nSides+1),1]))
  
  points$x <- points$x * sqrt(area/current.area)
  points$y <- points$y * sqrt(area/current.area)
  p <- data.frame(x = points$x, y= points$y)
  poo <- rbind(p, p[1,])
  po2 <- st_linestring(as.matrix(poo)) %>% 
    st_cast('MULTIPOINT') %>% 
    st_cast("POLYGON")
  
  return(po2)
  
}
