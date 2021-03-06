% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_mod.R
\name{data_mod}
\alias{data_mod}
\title{Transforms the raw data (depth and color)}
\usage{
data_mod(df_org)
}
\arguments{
\item{df_org}{dataframe with horizts as row and properties as column. At least the following columns should be in the dataframe (as.character):

\enumerate{
  \item The horizon name must be given in a column named "name"
  \item The depth must be given in a column named "depth" in the following format: "0-20" This example shows a horizon extension from 0 cm to minus 20 cm.
  \item The color must be specified in a column named "col" according to the system of Munsell.
  e.g. 10YR 5/4 (Important is the blank between the Hue and value and the backslash between the value and the Chroma).
  \item The rock content must be indicated in the following form: "2-3". The column must be called skel_dim. 
}}
}
\value{
A data frame with the following additional columns:
depth: ("from1", "to1"), "value_col", "chroma_col", "rgb_col" (calculated using the function \link[aqp]{munsell2rgb}, nameC (A new name column, the old one was replaced with an numeric id). skel_dim_from and skel_dim_to (seperate the skel dimension column))
}
\description{
This function transforms the raw profiledata. 
Funktion to modifide the transition of Soil horizon, by adding the Buffer- size, -number, and Shape of the Polygons, wich will be lay in the other horizont.
}
\examples{
data_example <- data.frame(name = c("Ah", "Bv"),
                           from = c(0, 22),
                           to = c(22, 33.434),
                           col = c("10YR 4/3", "5Y 5/3"),
                           skel_dim= c("3-30","45-50"))

print(data_mod(data_example))
}
