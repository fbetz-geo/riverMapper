% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_profile.R
\name{make_profile}
\alias{make_profile}
\title{Creates a longitudinal profile using a DEM (or other raster) and a channel line as input}
\usage{
make_profile(channel_line, rast_file, plot = TRUE)
}
\arguments{
\item{channel_line}{sf object containing the channel line}

\item{rast_file}{terra object with the raster values to use for the profile}

\item{plot}{If TRUE, a line plot of the profile will be returned}
}
\value{
a data.frame with the coordinates, the profile distance and the raster value
}
\description{
This function creates longitudinal profiles from a raster (most likely a DEM) and a channel line.
It uses the profile_dist() function to compute distances along the line, extracts the raster values
and adds them to a data.frame with the coordinates and cumulative distances along the line as well as the raster values
}
\author{
Florian Betz
}
