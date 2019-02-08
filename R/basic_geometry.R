#' basic random line
#'
#' This function draws a set of random lines. The functions were developed primarily to visualise roots. 
#'
#' @param polygon A Simple Feature or  just a "sfc_MULTIPOLYGON" or "POLYGON" geometry.
#' @param number The number of points from which the lines are to be created (dafault 800)
#' @param line_length The size of the buffer around each point. All points in this area will be combined to one line. This means if the buffer is larger, the lines become longer on average. 
#' @param variation The standard deviation of the random factor for the buffer size. Increasing the value will result in different lengths of the lines.
#' @param smoothness Passed on to \link[smoothr]{smooth}  fuction 
#' @return A Linedataset as Simple Feature with  a parID column
#' @examples 
#' #create an example dataset
#' df_example <-  data.frame(name = c(1, 2),
#'                           from1 = c(0,20),
#'                           to1 = c(20, 40),
#'                           rgb_col = c("#6F5F4CFF", "#947650FF")
#' )
#' 
#' #Set coordinates, four points on each horizon 
#' cord_example  <-  cord_setting(df_example, plot_width = 2)
#' 
#' #create a simple feature: Each line represents a horizon 
#' #with one polygon as geometry.
#' sf_example <- sf_polygon(df_geom = cord_example,
#'                          df_attri = df_example)
#' 
#' root_example <- basic_random_line(polygon = sf_example[1,],
#'                                   number = 100,
#'                                   line_length = 1,
#'                                   variation = 1,
#'                                   smoothness = 5)
#' library(ggplot2)
#' library(dplyr)
#' sf_example %>%
#'   ggplot() +
#'   geom_sf(fill = sf_example$rgb_col) +
#'   geom_sf(data = root_example, 
#'           size = root_example$id/max(root_example$id), 
#'           alpha = .3, col = "white ") +
#'   soil_theme()
#' 
#' 
#' @export


basic_random_line <- function(polygon, 
                              number = 300, 
                              line_length = .8, 
                              variation = 1,
                              smoothness = 5){
  stopifnot("sf" %in% class(polygon) | 
              "sfc_MULTIPOLYGON" %in% class(polygon) |
              "POLYGON" %in% class(polygon))
  
  point_temp <- sf::st_sample(polygon, number)
  
  geom <- do.call(
    rbind, lapply(
      1:length(point_temp), function(i){
        buffer_temp <- sf::st_buffer(point_temp[i],
                                     abs(rnorm(1, mean=line_length, sd = variation)))
        point_int <- sf::st_intersection(point_temp, buffer_temp) %>% 
          sf::st_sf(id = i, geometry = .) 
        return(point_int)
      }
    )
  )
  
  erg <- geom %>% 
    dplyr::group_by(id) %>% 
    dplyr::filter(length(id) >= 2) %>% 
    dplyr::summarise(do_union = T) %>% 
    sf::st_cast("LINESTRING") %>%
    smoothr::smooth(method = "ksmooth", smoothness = smoothness)
  return(erg)
}

#' basic random polygon 
#'
#' This function draws a set of random polygons.
#'
#' @param polygon A Simple Feature polygon or  just a "sfc_MULTIPOLYGON" or "POLYGON" geometry.
#' @param size percentage of output area relative to input area 
#' @param number number of polygons
#' @param nSides number of polygon sides
#' @param sm smooth by the \link[smoothr]{smooth} function
#' @param ... is passed to function \link[smoothr]{smooth} 
#' 
#' @return A unit multipolygon Simple Feature with one parID column
#' 
#' @examples 
#' poly_example <- sf::st_polygon(list(matrix(c(0,0,10,0,10,10,0,10,0,0),
#' ncol=2, byrow=TRUE)))
#' 
#' poly_example1 <- basic_random_polygon(polygon = poly_example, 
#'                                       size = .3, 
#'                                       number = 30,
#'                                       nSides = 5, sm = T)
#' plot(poly_example)
#' plot(poly_example1$geometry, add =T, col = "brown")
#' @export

basic_random_polygon <- function(polygon, size, number, nSides, sm = T, ...){
  
  stopifnot("sf" %in% class(polygon) | 
              "sfc_MULTIPOLYGON" %in% class(polygon) |
              "POLYGON" %in% class(polygon))
  
  area_size <- sf::st_area(polygon) * size/number
  
  point_samp <- sf::st_sample(polygon, number) %>% 
    sf::st_sf(nSides = nSides, number = number,
          area_size = area_size)

  sf_polygon <- point_2_polygon(point_samp)
  
  erg <- sf_polygon %>% 
    sf::st_intersection(polygon) %>% 
    sf::st_union() %>% 
    sf::st_sf(parID = 1, geometry = .)
  
  stopifnot("sf" %in% class(erg))
  
  if(sm == T){
    erg  <-  smoothr::smooth(erg, ...)
  }
  return(erg)
}


#' basic polygon 
#'
#' This function draws a polygon, with the following  helper fuction \link[sf]{st_make_grid}
#'
#' @param polygon polygon A Simple Feature or  just  a "sfc_MULTIPOLYGON" or "POLYGON" geometry.
#' @param cellnumber integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#' @param square logical; if FALSE, create hexagonal grid
#' 
#' @return A multipolygon Simple Feature with an unchanging parID column 
#' @examples 
#' df_example <-  data.frame(name = c(1, 2),
#' from1 = c(0,20),
#' to1 = c(20, 40)
#' )
#' 
#' #Set coordinates, four points on each horizon
#' cord_example  <-  cord_setting(df_example, plot_width = 2)
#' 
#' #create a simple feature: Each line represents a horizon
#' #with one polygon as geometry.
#' sf_example <- sf_polygon(df_geom = cord_example,
#'                          df_attri = df_example)
#' 
#' #Modification of the lines:
#' lattri_example <- data.frame(name= c(1,2),
#'                              numberX = c(2, 5),
#'                              sd = c(1,1),
#'                              sm = c(TRUE, TRUE)
#' )
#' 
#' line_example <- line_mod(df = cord_example,
#'                          line_attri = lattri_example)
#' #splitting the polygon                          
#' 
#' example_profile <- split_polygon(polygon = sf_example,
#'                                  line = line_example)
#' 
#' #Creating a set of smaller polygons in the second horizon 
#' 
#' poly_example1 <- basic_polygon(polygon = example_profile[2,])
#' plot(example_profile$geometry)
#' plot(poly_example1$geometry, add =T, col = "darkblue")
#'
#' @export

basic_polygon <- function(polygon, cellnumber = c(5, 15), square = T){
  stopifnot("sf" %in% class(polygon) | 
              "sfc_MULTIPOLYGON" %in% class(polygon) |
              "POLYGON" %in% class(polygon))
  
  sf::st_make_grid(polygon, what = "polygons", n = cellnumber,  square = square) %>% 
    sf::st_intersection(polygon) %>% 
    sf::st_sf(parID = 1, geometry = .)

}



#' basic line 
#'
#' This function draws a set of regular lines, with the following helper fuction \link[sf]{st_make_grid}
#'
#' @param polygon polygon A Simple Feature or  just  a "sfc_MULTIPOLYGON" or "POLYGON" geometry.
#' @param rotation The angle of the line
#' @param cellnumber integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#' @return A unit multiline Simple Feature  with a parID column
#' @examples 
#' df_example <-  data.frame(name = c(1, 2),
#'                           from1 = c(0,20),
#'                           to1 = c(20, 40)
#' )
#' 
#' #Set coordinates, four points on each horizon
#' cord_example  <-  cord_setting(df_example, plot_width = 2)
#' 
#' #create a simple feature: Each line represents a horizon
#' #with one polygon as geometry.
#' sf_example <- sf_polygon(df_geom = cord_example,
#'                          df_attri = df_example)
#' #Modification of the lines:
#' lattri_example <- data.frame(name= c(1,2),
#'                              numberX = c(2, 5),
#'                              sd = c(1,1),
#'                              sm = c(TRUE, TRUE)
#' )
#' 
#' line_example <- line_mod(df = cord_example,
#'                          line_attri = lattri_example)
#' #splitting the polygon 
#' example_profile <- split_polygon(polygon = sf_example,
#'                                  line = line_example)
#' 
#' #Creating a linegrid in the polygons
#' poly_example1 <- basic_line(polygon = example_profile[2,], cellnumber = c(1, 10))
#' poly_example2 <- basic_line(polygon = example_profile[1,],
#'                             cellnumber = c(10, 10),
#'                             rotation = 10)
#' 
#' #plotting
#' plot(example_profile$geometry)
#' plot(poly_example1$geometry, add =T, col = "darkblue")
#' plot(poly_example2$geometry, add =T, col = "darkred")
#' 
#'
#' @export

 

basic_line <- function(polygon, cellnumber = c(1, 10), rotation = 45){
  
  stopifnot("sf" %in% class(polygon) | 
              "sfc_MULTIPOLYGON" %in% class(polygon) |
              "POLYGON" %in% class(polygon))
  
  rotang <- rotation
  
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  
  tran <- function(geo, ang, center) (geo - center) * rot(ang * pi / 180) + center
  
  inpoly <- polygon %>% 
    sf::st_geometry()
  
  center <- sf::st_centroid(st_union(inpoly))
  
  grd <- sf::st_make_grid(tran(geo = inpoly, ang = -rotang, center = center), 
                          n = cellnumber)
  
  grd_rot <- tran(grd, rotang, center) %>% 
    sf::st_cast("LINESTRING") %>% 
    sf::st_intersection(polygon) %>% 
    sf::st_collection_extract("LINESTRING") %>% 
    sf::st_union() %>% 
    sf::st_sf(parID = 1, geometry = .)
  
  return(grd_rot)
}


#' basic random point 
#'
#' This function draws a random pointlayer, with the following helper fuction \link[sf]{st_sample}
#'
#' @param polygon polygon A Simple Feature or  just  a "sfc_MULTIPOLYGON" or "POLYGON" geometry.
#' @param size  sample size(s) requested; either total size, or a numeric vector with sample sizes for each feature geometry. When sampling polygons, the returned sampling size may differ from the requested size, as the bounding box is sampled, and sampled points intersecting the polygon are returned.
#' @return A unit point Simple Feature with an parID column
#' @examples 
#' df_example <-  data.frame(name = c(1, 2),
#'                           from1 = c(0,20),
#'                           to1 = c(20, 40)
#' )
#' 
#' #Set coordinates, four points on each horizon
#' cord_example  <-  cord_setting(df_example, plot_width = 2)
#' 
#' #create a simple feature: Each line represents a horizon
#' #with one polygon as geometry.
#' sf_example <- sf_polygon(df_geom = cord_example,
#'                          df_attri = df_example)
#' 
#' lattri_example <- data.frame(name= c(1,2),
#'                              numberX = c(2, 5),
#'                              sd = c(1,1),
#'                              sm = c(TRUE, TRUE)
#' )
#' 
#' line_example <- line_mod(df = cord_example,
#'                          line_attri = lattri_example)
#' #splitting the polygon 
#' example_profile <- split_polygon(polygon = sf_example,
#'                                  line = line_example)
#' 
#' #build an random point layer with 80 points
#' poly_example1 <- basic_random_point(polygon = example_profile[2,],  size = 80)
#' 
#' #plotting
#' 
#' plot(example_profile$geometry)
#' plot(poly_example1$geometry, add =T, col = "darkblue")
#' 
#'
#' @export

basic_random_point <- function(polygon, size = 12){
  
  stopifnot("sf" %in% class(polygon) | 
              "sfc_MULTIPOLYGON" %in% class(polygon) |
              "POLYGON" %in% class(polygon))
  
    sf::st_sample(polygon, size = size) %>% 
    sf::st_intersection(polygon) %>% 
      sf::st_union() %>% 
      sf::st_sf(parID = 1, geometry = .) #geometry colum named "."
}

#' basic regular point
#'
#' This function draws a regular pointlayer, with the following helperfuction \link[sf]{st_make_grid}
#'
#' @param polygon polygon A Simple Feature or  just  a "sfc_MULTIPOLYGON" or "POLYGON" geometry.
#' @param rotation the angle of the point
#' @param cellnumber integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#' @return A unit point Simple Feature  with a parID column
#' 
#' @examples 
#' df_example <-  data.frame(name = c(1, 2),
#' from1 = c(0,20),
#' to1 = c(20, 40)
#' )
#' 
#' #Set coordinates, four points on each horizon
#' cord_example  <-  cord_setting(df_example, plot_width = 2)
#' 
#' #create a simple feature: Each line represents a horizon
#' #with one polygon as geometry.
#' sf_example <- sf_polygon(df_geom = cord_example,
#'                          df_attri = df_example)
#' #modification of the lines:
#' lattri_example <- data.frame(name= c(1,2),
#'                              numberX = c(2, 5),
#'                              sd = c(1,1),
#'                              sm = c(TRUE, TRUE)
#' )
#' 
#' line_example <- line_mod(df = cord_example,
#'                          line_attri = lattri_example)
#' #split the polygon by the new lines
#' example_profile <- split_polygon(polygon = sf_example,
#'                                  line = line_example)
#' 
#' #create same regular points with different rotations:
#' poly_example1 <- basic_regular_point(polygon = example_profile[2,], 
#'                                      cellnumber = c(10, 10),
#'                                      rotation = 45)
#' poly_example2 <- basic_regular_point(polygon = example_profile[1,], 
#'                                      cellnumber = c(10, 10),
#'                                      rotation = 0)
#' 
#' 
#' 
#' 
#' plot(example_profile$geometry)
#' plot(poly_example1$geometry, add =T, col = "darkblue")
#' plot(poly_example2$geometry, add =T, col = "darkred")
#' 
#' @export

basic_regular_point <- function(polygon, cellnumber = c(10, 10), rotation = 45){
  
  stopifnot("sf" %in% class(polygon) | 
              "sfc_MULTIPOLYGON" %in% class(polygon) |
              "POLYGON" %in% class(polygon))
  
  rotang <- rotation
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  
  tran <- function(geo, ang, center) (geo - center) * rot(ang * pi / 180) + center
  inpoly <- polygon %>% 
    sf::st_geometry()
  
  center <- sf::st_centroid(st_union(inpoly))
  
  grd <- sf::st_make_grid(tran(geo = inpoly, ang = -rotang, center = center),
                          what = "centers", square = TRUE, n = cellnumber) %>% 
    sf::st_union()
  
  grd_rot <- tran(grd, rotang, center) %>% 
    sf::st_intersection(polygon) %>% 
    sf::st_sf(parID = 1, geometry = .)
  
}
