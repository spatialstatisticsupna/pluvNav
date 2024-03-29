% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadStationData.R
\name{downloadStationData}
\alias{downloadStationData}
\title{Download data for a single pluviometric station}
\usage{
downloadStationData(st, years, r.folder, curl, download)
}
\arguments{
\item{st}{a vector containing (station.ID, station.type, station.name) in the
same way as in the elements from \code{getStationList()}}

\item{years}{vector of years to download}

\item{r.folder}{path to the folder to store the downloaded csv files}

\item{curl}{curl handle}

\item{download}{logical argument. If \code{TRUE}, downloads all data, overwriting
previous files in \code{r.folder}}
}
\description{
Download data for a single pluviometric station
}
