% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/png_import.R
\name{multiple_png}
\alias{multiple_png}
\title{imports multiple png file an maks to the specific horizont position}
\usage{
multiple_png(shape_temp, path_temp, ...)
}
\arguments{
\item{shape_temp}{a simple feature with the same length of "path_temp"}

\item{path_temp}{a character string with the path of each texture .png}

\item{...}{is passed to function \link[soilprofile2]{png_import}}
}
\description{
imports multiple png file an maks to the specific horizont position
}
\examples{
library(dplyr) 
library(ggplot2)

sf_example <- soilprofile2::soil_example_sf
str_Bvh <- system.file("extdata", "broeckel.png", package = "soilprofile2")
str_BvCv <- system.file("extdata", "prismen.png", package = "soilprofile2")

str_all <- multiple_png(sf_example[c(2,3), ], c(str_Bvh, str_BvCv))

sf_example \%>\% 
  ggplot() +
  geom_sf(fill = sf_example$rgb_col) +
  geom_sf(data = str_all, fill = "black") +
  soil_theme()
}
