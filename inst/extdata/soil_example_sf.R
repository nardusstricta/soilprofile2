#create an example dataset and modify the color and depths 
df_example <- data.frame(name = c("Ah", "Bvh", "BvCv"),
                         depth = c("0-15", "15-43.4", "43.4-70"),
                         col = c("7.5YR 2/1","10YR 4/3", "2.5Y 5/3"),
                         skel_dim = c(".1-.8","1-2", "2-3"),
                         skel_ab = c(0.2, 0.4, .9),
                         grain_size = c(3, 2, 1),
                         grain_sd = c(3, 10, 4)) %>% 
  data_mod()

#Set coordinates, four points on each horizon 
cord_example  <-  cord_setting(df_example,
                               plot_width = 2)

#create a simple feature: Each line represents a horizon 
#with one polygon as geometry

soil_example_sf <- sf_polygon(df_geom = cord_example,
                              df_attri = df_example)

usethis::use_data(soil_example_sf, overwrite = TRUE)

