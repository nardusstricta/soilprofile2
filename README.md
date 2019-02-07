
<!-- README.md is generated from README.Rmd. Please edit that file -->
soilprofile2 <img src="logo/soilprofile.png" align="right" width=140/>
======================================================================

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

A package for R that provides the graphical representation of [soil horizons](https://en.wikipedia.org/wiki/Soil_horizon), in the simplest case the color. The focus is on morphological properties such as horizon transitions, texture, structure, root density, and rock content. Soil-forming processes can also be represented with uniform symbols, for example different hydromorphic soils characteristics. Some designations are based on the [German soil mapping instructions](https://de.wikipedia.org/wiki/Bodenkundliche_Kartieranleitung). The package was written as part of my master thesis at the [Chair of Soil Ecology](http://www.bodenkunde.uni-freiburg.de/index_html-en?set_language=en) of the University of Freiburg.

Installation
------------

The package is currently only available on Github

``` r
#install the development version from github with
#install.packages("devtools")
#devtools::install_github("nardusstricta/soilprofile2")
```

Usage
-----

This package provides functions to graphically represent soil properties. Morphological data gathered in the field such as horizon boundaries, root abundance and dimensions, skeletal shape, abundance and dimension as well as
meaningful soil color may be represented via the plot function.

### Data pre-processing

``` r
#load packages:
library(dplyr)
library(soilprofile2)
library(ggplot2)

#create an example dataset and modify the color and depths 
df_example <-  data.frame(name = c("Ah", "Bv", "C"), 
                          depth = c("0-15", "15-43.4", "43.4-70"), 
                          col = c("7.5YR 2/1","10YR 4/3", "2.5Y 5/3"),
                          skel_dim= c(".5-1","1-2", "1-2"), 
                          skel_ab = c(0.2, 0.6, 0.7)) %>% 
  data_mod()

#Set coordinates, four points on each horizon 
cord_example  <-  cord_setting(df_example, plot_width = 2)

#create a simple feature: Each line represents a horizon 
#with one polygon as geometry.
sf_example <- sf_polygon(df_geom = cord_example,
                         df_attri = df_example)
```

### Plot

``` r
sf_example %>% 
  ggplot() +
  geom_sf(fill = sf_example$rgb_col) +
  soil_theme()
```

<img src="README-plot_eample-1.png" width="1008" />

### Modification of the horizon geometries

First we create a new line between each horizon and split the entire profile with this line.

``` r
lattri_example <- data.frame(name= c(1,2,3),
                             numberX = c(2, 5, 12),
                             sd = c(1,.5, 1), # change it from c(1,.5, 1) to c(1,3,1)
                             sm = c(TRUE, TRUE, FALSE)
)

line_example <- line_mod(df = cord_example,
                         line_attri = lattri_example)

example_profile <- split_polygon(polygon = sf_example,
                                 line = line_example)

example_profile %>%
  ggplot() +
  geom_sf(fill = example_profile$rgb_col) +
  soil_theme()
```

<img src="README-data_mod-1.png" width="1008" />

Secondly, we can modify the profile so that the horizon polygons mix, i.e. a single horizon consists of a multipolygon

``` r

df_smooth <- data.frame(
  buffer_size = c(5), #from 5 to 4
  buffer_number = c(60),  #from 30 to 300
  nSides = c(10), #from 10 to 50
  rate = c(.6), # from 12 to 40 
  #and with shape  from .1 to .6
  name = c(2)
)

#Applying the function
smooth_profile <- smooth_trans(lmod = line_example,
                               shape_mod = example_profile,
                               attr_df = df_smooth, 
                               smoothness = 3, 
                               shape = 1) #von 12 auf .1
#Plot the result:
smooth_profile %>%
  ggplot() +
  geom_sf(fill = smooth_profile$rgb_col) +
  soil_theme()
```

<img src="README-trasiton-1.png" width="1008" />

Contributing
------------

To contribute to the development of this project please refer to the [guidelines](CONTRIBUTING.md).
