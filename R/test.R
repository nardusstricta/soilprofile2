library(tidyverse)
library(rbenchmark)


## Example data
data_example <- data.frame(name = c(1, 2),
from1 = c(0,20),
to1 = c(20, 40)
)

##Coordination setting
cord_example <- cord_setting(data_example, plot_width = 3)

## build simple feature 
poly_example <- sf_polygon(df_geom = cord_example)

## Line attributes data frame:
lattri_example <- data.frame(name= unique(cord_example$name),
                           numberX = c(2, 10), 
                       sd = c(1,1),
                       sm = c(T, T)
)

## Apply the line_mod fuction
line_example <- line_mod(df = cord_example, line_attri = lattri_example)

## Split the Profile with the new line shape:
example_profile <- split_polygon(polygon = poly_example,
                                 line = line_example) %>% 
  #join by the id (name) user-defined properties
  left_join(data.frame(name = c(1,2),
                       rgb_col = c("#6F5F4CFF", "#947650FF")
                       ), by = "name")

## Plot simple feature geometrysf_example %>%
example_profile %>% 
  ggplot() + 
  geom_sf(fill = example_profile$rgb_col) + 
  geom_text(label = c("Ah", "Bv"),
            x = c(10, 10), y = c(-10,-30)) + 
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        panel.background = element_blank())

  
