% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centerline.R
\name{centerline}
\alias{centerline}
\title{creates a centerline from the raster based corridor mask}
\usage{
centerline(
  mask,
  riverline,
  seg_length = 1000,
  max_width,
  smooth_factor = 2500,
  d = 5000
)
}
\arguments{
\item{mask}{spatRaster of the binary river corridor mask where the river corridor is encoded with 1 and non-corridor is NA;
alternatively, mask can also be a sf object with a polygon representing the river corridor}

\item{riverline}{sf object representing the river line}

\item{seg_length}{length of the desired river segments; choosing the appropriate scale of analysis will depend very much
on the resolution of the input data and the scope of the specific analysis; default is 1 km
#' @param max_width Estimated maximum width of the river corridor used to create an initial boundary of the
disaggregation; will be reduced to the boundaries of "mask" during the processing and thus should cover the
maximum width of the corridor mask}

\item{smooth_factor}{smoothing factor for converting the raster mask to polygon, a higher smooth parameter will result in higher degree of simplification of the polygon}

\item{d}{length to extend the ends of the riverline in order to split the corridor outline; can be adjusted to avoid errors}
}
\value{
a list with 1) the centerline as sf line object and 2) the smoothed polygon as sf object
}
\description{
The centerline function derives the centerline of the river corridor mask
}
\author{
Florian Betz
}
