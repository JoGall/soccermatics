#' @include soccerPitchFG.R
NULL
#' Draw a heatmap on a soccer pitch.
#' @description Draws a heatmap showing player position frequency in each area of the pitch and adds soccer pitch outlines.
#' 
#' @param df dataframe containing x,y-coordinates of player position in columns named \code{'x'} and \code{'y'}.
#' @param bins integer, the number of horizontal bins (length-wise) the soccer pitch is to be divided up into. If no value for \code{yBins} is provided, this value will also be used for the number of vertical (width-wise) bins.
#' @param yBins integer, the number of vertical bins (width-wise) the soccer patch is to be divided up into. If \code{NULL}, the same value is used as for \code{bins}.
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param colLow,colHigh character, colours for the low and high ends of the heatmap gradient.
#' @return a ggplot object of a heatmap on a soccer pitch.
#' @details uses \code{ggplot2::geom_bin2d} to map 2D bin counts
#' @examples
#' data(tromso)
#' # draw heatmap of player #9's position
#' soccerHeatmap(subset(tromso, id == 8), bins = 15)
#' 
#' @seealso \code{\link{soccerPitchBG}} for a background soccer pitch for the purpose of drawing position maps, player trajectories, etc..
#' @export
soccerHeatmap <- function(df, bins = 5, lengthPitch = 105, widthPitch = 68, yBins = NULL, colLow = "white", colHigh = "red") {
  # check value for vertical bins and match to horizontal bins if NULL
  if(is.null(yBins)) yBins <- bins
  
  # filter invalid values outside pitch limits
  df <- df[df$x > 0 & df$x < lengthPitch & df$y > 0 & df$y < widthPitch,]
  
  # define bin ranges
  x.range <- seq(0, lengthPitch, length.out = bins+1)
  y.range <- seq(0, widthPitch, length.out = yBins+1)
  
  # plot
  p <- ggplot() +
    geom_bin2d(data = df, aes(x, y), binwidth = c(diff(x.range)[1], diff(y.range)[1])) +
    scale_fill_gradient(low = colLow, high = colHigh) +
    guides(fill=FALSE)
  
  soccerPitchFG(p, lengthPitch = lengthPitch, widthPitch = widthPitch)
}
