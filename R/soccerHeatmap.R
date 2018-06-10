#' @include soccerPitchFG.R
#' @import ggplot2
#' @import dplyr
NULL
#' Draw a heatmap on a soccer pitch.
#' @description Draws a heatmap showing player position frequency in each area of the pitch and adds soccer pitch outlines.
#' 
#' @param df dataframe containing x,y-coordinates of player position in columns named \code{'x'} and \code{'y'}.
#' @param xBins,yBins integer, the number of horizontal (length-wise) and vertical (width-wise) bins the soccer pitch is to be divided up into. If no value for \code{yBins} is provided, it will take the value of \code{xBins}.
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param colLow,colHigh character, colours for the low and high ends of the heatmap gradient.
#' @return a ggplot object of a heatmap on a soccer pitch.
#' @details uses \code{ggplot2::geom_bin2d} to map 2D bin counts
#' @examples
#' data(tromso)
#' # simple heatmap of player #9's position
#' soccerHeatmap(subset(tromso, id == 8), xBins = 10)
#' 
#' # draw heatmap with approximately 5m x 5m bins (pitchLength / 5 = 21, pitchWidth / 5 = 13.6)
#' soccerHeatmap(subset(tromso, id == 8), xBins = 21, yBins = 14)
#' 
#' @seealso \code{\link{soccerPitchBG}} for a background soccer pitch for the purpose of drawing position maps, player trajectories, etc..
#' @export
soccerHeatmap <- function(df, xBins, lengthPitch = 105, widthPitch = 68, yBins = NULL, colLow = "white", colHigh = "red") {
  # check value for vertical bins and match to horizontal bins if NULL
  if(is.null(yBins)) yBins <- xBins
  
  # filter invalid values outside pitch limits
  df <- df[df$x > 0 & df$x < lengthPitch & df$y > 0 & df$y < widthPitch,]
  
  # define bin ranges
  x.range <- seq(0, lengthPitch, length.out = xBins+1)
  y.range <- seq(0, widthPitch, length.out = yBins+1)
  
  # plot
  p <- ggplot() +
    geom_bin2d(data = df, aes(x, y), binwidth = c(diff(x.range)[1], diff(y.range)[1])) +
    scale_fill_gradient(low = colLow, high = colHigh) +
    guides(fill=FALSE)
  
  soccerPitchFG(p, lengthPitch = lengthPitch, widthPitch = widthPitch)
}
