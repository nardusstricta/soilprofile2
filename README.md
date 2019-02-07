
# soilprofile2 <img src="logo/soilprofile.png" align="right" width=140/>

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

A package for R that provides the graphical representation of [soil horizons](https://en.wikipedia.org/wiki/Soil_horizon), in the simplest case the color. The focus is on morphological properties such as horizon transitions, texture, structure, root density, and rock content. Soil-forming processes can also be represented with uniform symbols, for example different hydromorphic soils characteristics. Some designations are based on the [German soil mapping instructions](https://de.wikipedia.org/wiki/Bodenkundliche_Kartieranleitung). The package was written as part of my master thesis at the [Chair of Soil Ecology](http://www.bodenkunde.uni-freiburg.de/index_html-en?set_language=en) of the University of Freiburg.  


## Installation
The package is currently only available on Github 
```{r installation, eval = FALSE}
install the development version from github with
install.packages("devtools")
devtools::install_github("nardusstricta/soilprofile2")
```

## Usage
This package provides functions to graphically represent 
soil properties. Morphological data gathered in the field 
such as horizon boundaries, root abundance and dimensions,
skeletal shape, abundance and dimension as well as	
meaningful soil color may be represented via the plot
function.

```{r smooth-lines, dpi = 144}
#load packages:
library(dplyr)
library(soilprofile2)
library(ggplot2)

#create an example dataset and modify the color and depths 
df_example <-  data.frame(name = c("Ah", "Bv"), 
                          depth = c("0-22", "22-33.434"), 
                          col = c("10YR 4/3", "5Y 5/3"),
                          skel_dim= c(".5-1","1-2"), 
                          skel_ab = c(0.2, 0.6)) %>% 
  data_mod()
print(df_example)
```


## Contributing

To contribute to the development of this project please refer to the [guidelines](CONTRIBUTING.md).


