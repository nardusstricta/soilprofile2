#' creates a root layer modelling by a L-System
#'
#' Hier sollten man noch einige Sachen einbauen:
#' kein Fehler bei fehlendem skeleton
#' smooth
#' clip mit dem ursprünglichen layer
#' 
#' @param Plant a numeric value between 1 and 6 calling the stored L-system
#' @param skeleton A layer with the skeleton content as polygon (bei fehlen gibt es noch einen Fehler)
#' @param start The start coordinates (x and y) given in a vector 
#' 
#' @return This function returns a new sf-line geometry with an id column 
#' @import tidyverse
#' @import sf
#' @import gsubfn
#' @import stringr
#' @export


root <- function(Plant = 1, skeleton = spoint, start = c(6, 0)){
  library(gsubfn)
  library(stringr)
  if(Plant == 1){
    #Plant 1
    axiom="F"
    rules=list("F"="FF-[-F+F+F]+[+F-F-F]")
    angle=22.5
    depth=3
  }
  
  if(Plant == 2){
    #Plant 2
    axiom="X"
    rules=list("X"="F[+X][-X]FX", "F"="FF")
    angle=25.7
    depth=7
  }
  
  if(Plant == 3){
    #Plant 3
    axiom="X"
    rules=list("X"="F[+X]F[-X]+X", "F"="FF")
    angle=20
    depth=7
  }
  
  if(Plant == 4){
    #Plant 4
    axiom="X"
    rules=list("X"="F-[[X]+X]+F[+FX]-X", "F"="FF")
    angle=22.5
    depth=5
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
  
  for (i in 1:depth) axiom=gsubfn(".", rules, axiom)
  
  actions = str_extract_all(axiom, "\\d*\\+|\\d*\\-|F|L|R|\\[|\\]|\\|") %>% unlist
  
  
  status = data.frame(x=numeric(0), y=numeric(0), alfa=numeric(0))
  points = data.frame(x1 = start[1], y1 = start[2], x2 = NA, y2 = NA,  alfa=90, depth=1)
  
  
  
  
  
  for (action in actions) {
    if (action=="F"){
      mat <- matrix(NA, 2, 2)
      counter <- 0.8
      repeat{
        counter <-  counter/2
        x <- points[1, "x1"] + (cos(points[1, "alfa"] * (pi/180)) * counter)
        y <- points[1, "y1"] + (sin(points[1, "alfa"] * (-pi/180)) * counter)
        mat[1,1] <- points$x1[1]
        mat[1,2] <- points$y1[1]
        mat[2,1] <- x
        mat[2,2] <- y
        if(any(st_intersects(st_linestring(mat), skeleton$geometry[3],  sparse = F ))== T){
          print(counter)
          if(counter < 0.3){
            
            break
          }
          #if(counter < 0.03){00
          #suf <- sample(c("+", "-"), 1)
          #alfa <- points$alfa[1]
          #points$alfa[1] <- eval(parse(text=paste0("alfa", suf, angle)))
          #print(x)
        }else{
          points[1,"x2"] <- x
          points[1,"y2"] <- y 
          points <- data.frame(x1 = x, y1 = y, x2 = NA, y2 = NA, 
                               alfa = points[1, "alfa"],
                               depth = points[1,"depth"]) %>% rbind(points)
          break
          
        } 
        
        
      }
      
      
      
      
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
                            y1=status[1, "y"], x2=NA, y2=NA, 
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
  
  
  line_sf <- rsdr %>% 
    st_as_sf(coords = c("x", "y")) %>%
    group_by(id) %>%
    summarise(do_union = F) %>% 
    st_cast('LINESTRING') 
  return(line_sf)
  
}
