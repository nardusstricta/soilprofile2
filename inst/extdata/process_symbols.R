#building attriute data.frame 

process_symbols <- data.frame(
  name = c("Toncutane", "Wasser", 
           "Humuseinwaschung",
           "mangan Konkretionen",
           "extrovertierte Redoximorphose ed", 
           "introvertierte Redoximorphose ed", 
           "humos", 
           "carbonatarm", 
           "carbonatreich"),
  shape = c(9, 126, 35, 15, 21, 21, 104, 60, 62), 
  color = c("black", "darkblue", "black", "black", "darkred", "grey", "grey", "grey", "grey"),
  fill = c(NA, "darkblue", "black", NA, "grey", "darkred", "black", "black", "black"),
  stroke = c(1, 2, 2, 2, 4, 4, 2, 2, 2)
  
)
usethis::use_data(process_symbols, overwrite = TRUE)
