% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ChangeGridforEVfiles.R
\name{ChangeGridforEVfiles}
\alias{ChangeGridforEVfiles}
\title{Modify acoustic variable grid height and distance for both
Sv and single target variables.}
\usage{
ChangeGridforEVfiles(
  EVFile,
  SvacoVarName,
  TSacoVarName,
  timeDistanceGridType,
  depthGridType,
  timeDistanceGridDistance,
  depthGridDistance
)
}
\arguments{
\item{EVFile}{EV file to be altered. In this function it will generally be one of many and the user
will have this entered by code.}

\item{SvacoVarName}{Character. The name for the Sv variable.}

\item{TSacoVarName}{Character. The name for the single target variable.}

\item{timeDistanceGridType}{(numeric) specifying the along track grid type. 0 = no grid, 1 = time (minutes), 2 = GPS distance (NMi), 3 = Vessel Log Distance (NMi), 4 = Pings, 5 = GPS distance (m), 6 = Vessel Log Distance (m).}

\item{depthGridType}{(numeric) 0 = no grid, 1 = depth grid, 2 = use reference line.}

\item{timeDistanceGridDistance}{(numeric) vertical grid line spacing. Not needed if verticalType = 0.}

\item{depthGridDistance}{(numeric) horizontal grid line spacing. Ignored if horizontalType = 0.}
}
\value{
Modified EV files.
}
\description{
Modify acoustic variable grid height and distance for both
Sv and single target variables.
}
