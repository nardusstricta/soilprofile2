% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/special_geometry.R
\name{fun_grain_size}
\alias{fun_grain_size}
\title{build soil_texture}
\usage{
fun_grain_size(polygon, ...)
}
\arguments{
\item{polygon}{polygon A Simple Feature or  just  a "sfc_MULTIPOLYGON" or "sfc_POLYGON" geometry.}

\item{...}{information about texture size and varation.}
}
\value{
This function returns a new Simple File geometry
}
\description{
The actual task to build random pattern if no texture information and no specific horizont function is available and you still want to draw a texture. for example for black and white representations.
}
