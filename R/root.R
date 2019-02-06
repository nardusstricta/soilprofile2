#' creates a root layer modelling by a L-System
#'
#' Hier sollten man noch einige Sachen einbauen:
#' kein Fehler bei fehlendem skeleton
#' smooth
#' clip mit dem ursprünglichen layer
#' 
#' @param Plant a numeric value between 1 and 6 calling the stored L-system
#' 
#' @return This function returns a new sf-line geometry with an id column 
#' @export


root <- function(Plant = 1, 
                 axiom="F", 
                 rules=list("F"="FF-[-F+F+F]+[+F-F-F]"),
                 angle=22.5,
                 depth=2){

  
  if(Plant == 2){
    #Plant 2
    axiom="X"
    rules=list("X"="F[+X][-X]FX", "F"="FF")
    angle=25.7
    depth=2
  }
  
  if(Plant == 3){
    #Plant 3
    axiom="X"
    rules=list("X"="F[+X]F[-X]+X", "F"="FF")
    angle=20
    depth=3
  }
  
  if(Plant == 4){
    #Plant 4
    axiom="X"
    rules=list("X"="F-[[X]+X]+F[+FX]-X", "F"="FF")
    angle=22.5
    depth=4
  } 
  
  if(Plant == 5){
    #Plant 5
    axiom="F"
    rules=list("F"="F[+F]F[-F]F")
    angle=25.7
    depth=5
  }
  
  if(Plant == 6){
    #Plant 6
    axiom="F"
    rules=list("F"="F[+F]F[-F][F]")
    angle=20
    depth=5
  }
  start <- c(0,0)

  for (i in 1:depth) axiom = gsubfn::gsubfn(".", rules, axiom)
  
    actions = stringr::str_extract_all(axiom, "\\d*\\+|\\d*\\-|F|L|R|\\[|\\]|\\|") %>%
    unlist
  
  
  status <- data.frame(x=numeric(0), y=numeric(0), alfa=numeric(0))
  points <- data.frame(x1 = start[1], 
                      y1 = start[2], 
                      x2 = NA, 
                      y2 = NA, 
                      alfa=90, 
                      depth=1)
  
  
  
  mat <- matrix(NA, 2, 2)
  
  for (action in actions) {
    if (action=="F"){
      
        x <- points[1, "x1"] + (cos(points[1, "alfa"] * (pi/180)))
        y <- points[1, "y1"] + (sin(points[1, "alfa"] * (-pi/180)))
        
        mat[1,1] <- points$x1[1]
        mat[1,2] <- points$y1[1]
        mat[2,1] <- x
        mat[2,2] <- y
        

          points[1,"x2"] <- x
          points[1,"y2"] <- y 
          points <- data.frame(x1 = x, y1 = y, x2 = NA, y2 = NA, 
                               alfa = points[1, "alfa"],
                               depth = points[1,"depth"]) %>%
            rbind(points)
        
      }
    
    if (action %in% c("+", "-")){
      alfa <- points[1, "alfa"]
      points[1, "alfa"] <-  eval(parse(text=paste0("alfa", action, angle)))
    }
    
    if(action == "["){ 
      status <- data.frame(
        x = points[1, "x1"],
        y = points[1, "y1"], 
        alfa = points[1, "alfa"]
      ) %>% 
        rbind(status) 
      
      points[1, "depth"] = points[1, "depth"] + 1
    }

    if(action == "]"){ 
      depth <- points[1, "depth"]
      points <- points[-1,]
      points <-  data.frame(x1=status[1, "x"], 
                            y1=status[1, "y"], x2 = NA, y2 = NA, 
                            alfa=status[1, "alfa"],
                            depth = depth-1) %>% 
        rbind(points) 
      status <- status[-1,] 
    }

  }
    
  

  sdr <- points %>% 
    select(x1, y1, depth) %>% 
    mutate(id = 1:nrow(points))
  
  names(sdr) <- c("x", "y", "depth", "id")
  
  sdr2 <- points %>% 
    select(x2, y2, depth) %>% 
    mutate(id = 1:nrow(points))
  
  
  names(sdr2) <- c("x", "y", "depth", "id")
  
  
  rsdr <- rbind(sdr[-1,], sdr2[-1,]) 
  

  return(rsdr)
}

#' creates a root layer modelling by a L-System
#'
#' Hier sollten man noch einige Sachen einbauen:
#' kein Fehler bei fehlendem skeleton
#' smooth
#' clip mit dem ursprünglichen layer
#' 
#' @param Plant a numeric value between 1 and 6 calling the stored L-system
#' 
#' @return This function returns a new sf-line geometry with an id column 
#' @export


root_position <- function(root_inp, horizont, spoint, sample_size){
  if(!is.null(spoint)){
    temp <- st_combine(spoint) %>% 
      st_buffer(0.0)
    place <- st_difference(horizont, temp)
  }else{
    temp <- st_combine(spoint) %>% 
      st_buffer(0.0)
    place <- st_difference(horizont, temp)
  }
  
  sample1 <- st_sample(place, sample_size) %>% 
    st_coordinates()
  
   
  geom <- do.call(
    rbind, lapply(
      1:nrow(sample1), function(i){
        x <- sample1[i,1] + root_inp[,1] * rnorm(1, mean = 1, sd = 0.3)
        y <- sample1[i,2] + root_inp[,2] * rnorm(1, mean = 1, sd = 0.3)
        erg <- data.frame(x = x, 
                          y = y, 
                          id = i,
                          id1 = root_inp[,4])
        return(erg)
      }
    )
  )

  line_sf <- geom %>% 
    st_as_sf(coords = c("x", "y")) %>%
    group_by(id, id1) %>%
    summarise(do_union = F) %>% 
    st_cast('LINESTRING') %>% 
    st_intersection(horizont)
  
  lines_merged <- st_cast(st_line_merge(
    st_union(st_cast(line_sf, "MULTILINESTRING"))), "LINESTRING")
  
  ff <- smoothr::smooth(lines_merged, method = "chaikin")
  
  return(ff)
}
