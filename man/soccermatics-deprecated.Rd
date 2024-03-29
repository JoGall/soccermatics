% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soccerPitchBG.R, R/soccermatics-deprecated.R
\name{soccerPitchBG}
\alias{soccerPitchBG}
\alias{soccermatics-deprecated}
\title{Plot a full soccer pitch}
\usage{
soccerPitchBG(
  lengthPitch = 105,
  widthPitch = 68,
  arrow = c("none", "r", "l"),
  title = NULL,
  subtitle = NULL,
  theme = c("light", "dark", "grey", "grass"),
  data = NULL
)
}
\arguments{
\item{lengthPitch, widthPitch}{length and width of pitch in metres}

\item{arrow}{adds team direction of play arrow as right (\code{'r'}) or left (\code{'l'}); \code{'none'} by default}

\item{title, subtitle}{adds title and subtitle to plot; NULL by default}

\item{theme}{palette of pitch background and lines, either \code{light} (default), \code{dark}, \code{grey}, or \code{grass}}

\item{data}{a default dataset for plotting in subsequent layers; NULL by default}

\item{fillPitch, colPitch}{pitch fill and line colour}
}
\value{
a ggplot object
}
\description{
Draws a soccer pitch as a ggplot object for the purpose of adding layers such as player positions, player trajectories, etc..

The functions listed below are deprecated and will be defunct in
  the near future. When possible, alternative functions with similar
  functionality are also mentioned. Help pages for deprecated functions are
  available at \code{help("soccermatics-deprecated")}.
}
\seealso{
\code{\link{soccermatics-deprecated}}
}
\keyword{internal}
