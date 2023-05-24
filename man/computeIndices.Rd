% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeIndices.R
\name{computeIndices}
\alias{computeIndices}
\title{Compute Morphometric Indices}
\usage{
computeIndices(dem, channels, saga_path, indices, out_dir = tempdir())
}
\arguments{
\item{dem}{path to the digital elevation model}

\item{channels}{path to raster of channel network delineated from digital elevation models}

\item{saga_path}{path where the SAGA installation is found (see RSAGA::rsaga.env() for details)}

\item{out_dir}{directory where to store output files}
}
\value{
spatRaster with the morphometric indices
}
\description{
Computes different morphometric indices useful for the fuzzy delineation of river corridors
}
\references{
Betz, F., Lauermann, M., Cyffka, B. (2018): Delineation of riparian zone...
}
\author{
Florian Betz
}