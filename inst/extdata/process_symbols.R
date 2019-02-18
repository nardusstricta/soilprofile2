#building attriute data.frame 

process_symbols <- data.frame(
  name = c("Toncutane", "Wasser", 
           "Humuseinwaschung",
           "mangan Konkretionen",
           "extrovertierte Redoximorphose ed", 
           "introvertierte Redoximorphose ed"),
  shape = c(9, 126, 35, 15, 21, 21), 
  color = c("black", "darkblue", "black", "black", "darkred", "grey"),
  fill = c(NA, "darkblue", "black", NA, "grey", "darkred"),
  stroke = c(1, 2, 2, 2, 2, 2)
  
)
usethis::use_data(process_symbols, overwrite = TRUE)
