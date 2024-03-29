% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soccerHeatmap.R
\name{soccerHeatmap}
\alias{soccerHeatmap}
\title{Draw a heatmap on a soccer pitch using any event or tracking data.}
\usage{
soccerHeatmap(
  df,
  lengthPitch = 105,
  widthPitch = 68,
  xBins = 10,
  yBins = NULL,
  kde = FALSE,
  arrow = c("none", "r", "l"),
  colLow = "white",
  colHigh = "red",
  title = NULL,
  subtitle = NULL,
  x = "x",
  y = "y"
)
}
\arguments{
\item{df}{dataframe containing x,y-coordinates of player position}

\item{lengthPitch, widthPitch}{numeric, length and width of pitch in metres.}

\item{xBins, yBins}{integer, the number of horizontal (length-wise) and vertical (width-wise) bins the soccer pitch is to be divided up into. If no value for \code{yBins} is provided, it will take the value of \code{xBins}.}

\item{kde}{use kernel density estimates for a smoother heatmap; FALSE by default}

\item{arrow}{adds team direction of play arrow as right (\code{'r'}) or left (\code{'l'}); \code{'none'} by default}

\item{colLow, colHigh}{character, colours for the low and high ends of the heatmap gradient; white and red respectively by default}

\item{title, subtitle}{adds title and subtitle to plot; NULL by default}

\item{x, y}{name of variables containing x,y-coordinates}
}
\value{
a ggplot object of a heatmap on a soccer pitch.
}
\description{
Draws a heatmap showing player position frequency in each area of the pitch and adds soccer pitch outlines.
}
\details{
uses \code{ggplot2::geom_bin2d} to map 2D bin counts
}
\examples{
library(dplyr)

# tracking data heatmap with 21x5 zones(~5x5m)
data(tromso)
tromso \%>\% 
  filter(id == 8) \%>\% 
  soccerHeatmap(xBins = 10)

# transform x,y-coords, filter only France pressure events,
# heatmap with 6x3 zones
data(statsbomb)
statsbomb \%>\%
  soccerTransform(method='statsbomb') \%>\% 
  filter(type.name == "Pressure" & team.name == "France") \%>\% 
  soccerHeatmap(x = "location.x", y = "location.y",
                xBins = 6, yBins = 3, arrow = "r", 
                title = "France (vs Argentina, 30th June 2016)", 
                subtitle = "Defensive pressure heatmap")

# transform x,y-coords, standardise column names,
# filter player defensive actions, plot kernel density estimate heatmap
statsbomb \%>\%
  soccerTransform(method='statsbomb') \%>\% 
  soccerStandardiseCols() \%>\% 
  filter(event_name \%in\% c("Duel", "Interception", "Clearance", "Block") &
         player_name == "Samuel Yves Umtiti") \%>\%
  soccerHeatmap(kde = TRUE, arrow = "r",
                title = "Umtiti (vs Argentina, 30th June 2016)",
                subtitle = "Defensive actions heatmap")
                
}
