% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smooth_trans.R
\name{smooth_trans}
\alias{smooth_trans}
\title{Shape and position of the horizontal boundary}
\usage{
smooth_trans(lmod, shape_mod, attr_df, shape = 10, seed = 33,
  smoothness = 2)
}
\arguments{
\item{lmod}{A simple feature line layer which was created with the \link[soilprofile2]{line_mod} function for example. The horizon borders should be defined by these lines.}

\item{shape_mod}{A simple feature polygon layer with one line for each horizon. For example, created by the function \link[soilprofile2]{sf_polygon}  or \link[soilprofile2]{split_polygon}}

\item{attr_df}{Attribute table with the following parameters for the horizon transitions: "buffer_size" How far should the transition extend into the neighbouring horizon? "buffer_number" How many polygons should lie in the neighboring horizon? "nSides" How many sides should the polygons have? "rate" The area size of the polygons is drawn from a gamma distribution. The "rate" parameter required for this can be set individually for each horizon. "name" The horizon id as number}

\item{shape}{The parameter to define the area for the polygon. The smaller it is, the smaller is the area. In addition, the distribution is skewed.}

\item{seed}{Seed setting for reproducible results}

\item{smoothness}{is passed to function \link[smoothr]{smooth_ksmooth} ; a parameter controlling the bandwidth of the Gaussian kernel, and therefore the smoothness and level of generalization. By default, the bandwidth is chosen as the mean distance between adjacent points. The smoothness parameter is a multiplier of this chosen bandwidth, with values greater than 1 yielding more highly smoothed and generalized features and values less than 1 yielding less smoothed and generalized features.}
}
\value{
A simple feature with one line per horizon and the attributes from the input polygon
}
\description{
This function changes the soil horizon transitions. It creates polygons in the neighboring horizon so that a smooth transition can be mapped.
}
\examples{
#Creat same example data:
library(dplyr)
library(soilprofile2)
library(ggplot2)
geom_example <- data.frame(name = c(1, 2),
                           from1 = c(0,20),
                           to1 = c(20, 40)
) \%>\%
  cord_setting(plot_width = 3)

attri_example <- data.frame(name = c(1, 2),
                            rgb_col = c("#6F5F4CFF", "#947650FF"),
                            nameC = c("Ah", "Bv")
)

##
sf_example <- sf_polygon(df_geom = geom_example, df_attri = attri_example)

## Line attributes data frame:
lattri_example <- data.frame(name= c(1,2),
                             numberX = c(2, 10),
                             sd = c(1,1),
                             sm = c(TRUE, TRUE)
)

## Apply the line_mod fuction
line_example <- line_mod(df = geom_example,
                         line_attri = lattri_example)

## Split the Profile with the new line shape:
example_profile <- split_polygon(polygon = sf_example,
                                 line = line_example)

## Attributes table with the specifications for the transition:
##
df_smooth <- data.frame(
  buffer_size = c(5),
  buffer_number = c(30),
  nSides = c(10),
  rate = c(15),
  name = c(2)
)
#Applying the function
smooth_profile <- smooth_trans(lmod = line_example,
                               shape_mod = example_profile,
                               attr_df = df_smooth)
#Plot the result:

smooth_profile \%>\%
  ggplot() +
  geom_sf(fill = smooth_profile$rgb_col)
}
