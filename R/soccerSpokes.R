#' @include soccerPitchBG.R
#' @include soccerHeatmap.R
#' @import ggplot2
#' @import dplyr
NULL
#' Visualise movement direction on a soccer pitch.
#' @description Draws spokes showing the direction of x,y-movements made in each sector of the pitch.
#' 
#' @param plot plot of soccer pitch returned by \code{\link{soccerPitchBG}} to add spokes to
#' @param df dataframe containing x,y-coordinates of player position in columns named \code{x} and \code{y} and angular information (in radians, ranging between \code{-pi} and \code{pi}) in a column \code{direction}.
#' @param lengthPitch,widthPitch length and width of pitch in metres.
#' @param xBins,yBins integer, the number of horizontal (length-wise) and vertical (width-wise) bins the soccer pitch is to be divided up into. If no value for \code{yBins} is provided, it will take the value of \code{xBins}.
#' @param angleBins integer, the number of angle bins movement directions are divided up into. For example, a value of 4 clusters directions in each bin into north, east, south and west.
#' @param lwd thickness of arrow lines
#' @param minLength numeric, ratio between size of shortest arrow and longest arrow depending on number of events.
#' @param legend if \code{TRUE}, adds legend showing relationship between arrow transparency and number of events

#' @return a ggplot object
#' @examples
#' data(tromso_extra)
#' # resample movement dataset to plot 100 movement directions 
#' # (in absence of pass / shot event data as yet)
#' id8 <- tromso_extra %>%
#'   dplyr::filter(id == 8) %>%
#'   dplyr::sample_n(100)
#' # 5x5 x,y-bins, 16 angle-bins, blank pitch
#' soccerPitchBG(pitchLength, pitchWidth) %>% 
#'   soccerSpokes(id8, xBins = 5, angleBins = 16, minLength = 0.4)
#' # 10x10 x,y-bins, 8 angle-bins, grass pitch
#' soccerPitchBG(pitchLength, pitchWidth, grass = T) %>% 
#'   soccerSpokes(id8, xBins = 10, angleBins = 8, minLength = 0.2, lwd = 1)
#' # draw spokes over player heatmap w/ 5x5 x,y-bins, 8 angle-bins
#' soccerHeatmap(id8, xBins = 5) %>% 
#'   soccerSpokes(id8, xBins = 5, angleBins = 8, lwd = 1)
#' 
#' @seealso \code{\link{soccerPitchBG}} for drawing a heatmap of player position, \code{\link{soccerHeatmap}} for drawing a heatmap of player position
#' @export
soccerSpokes <- function(plot, df, lengthPitch = 105, widthPitch = 68, xBins, yBins = NULL, angleBins = 16, lwd = 0.5, minLength = 0.6, minAlpha = 0.4, legend = TRUE) {
  
  # check value for vertical bins and match to horizontal bins if NULL
  if(is.null(yBins)) yBins <- xBins
  
  x.range <- seq(0, lengthPitch, length.out = xBins+1)
  y.range <- seq(0, widthPitch, length.out = yBins+1)
  
  angle.bin <- seq(-pi, pi, length.out = angleBins+1)
  
  # bin plot values
  x.bin.coords <- data.frame(x.bin = 1:xBins, 
                             x.bin.coord = (x.range + (lengthPitch / (xBins) / 2))[1:xBins])
  y.bin.coords <- data.frame(y.bin = 1:yBins,
                             y.bin.coord = (y.range + (widthPitch / (yBins) / 2))[1:yBins])
  
  angle.bin.theta <- data.frame(angle.bin = 1:angleBins,
                                angle.theta = angle.bin[1:angleBins])
  
  # bin by x,y bins
  df <- df %>%
    rowwise() %>%
    mutate(x.bin = max(which(x > x.range)),
           y.bin = max(which(y > y.range)),
           bin = paste(x.bin, y.bin, sep = "_")) %>%
    ungroup()
  
  # bin by angle bins
  df <- df %>%
    group_by(bin) %>%
    rowwise() %>%
    mutate(angle.bin = max(which(direction > angle.bin))) %>%
    ungroup()
  
  # count number of events in each angle bin
  df <- df %>%
    group_by(bin, angle.bin) %>%
    summarise(n.angles = n(), x.bin = x.bin[1], y.bin = y.bin[1])
  
  # join x,y-coords and theta to each bin
  df <- left_join(df, x.bin.coords, by = "x.bin")
  df <- left_join(df, y.bin.coords, by = "y.bin")
  df <- left_join(df, angle.bin.theta, by = "angle.bin")
  
  # df$alpha <- minAlpha + ((1-minAlpha) / max(df$n.angles) * df$n.angles)
  df$radius <- (minLength * widthPitch / (yBins+5)) + ((1-minLength) * (df$n.angles / max(df$n.angles)) * widthPitch / (yBins+5))
  
  # plot
  p <- plot +
    geom_point(data = df, aes(x = x.bin.coord, y = y.bin.coord)) +
    geom_spoke(data = df, aes(x = x.bin.coord, y = y.bin.coord, angle = angle.theta, radius = radius), size = lwd, arrow=arrow(length = unit(0.15,"cm"))) +
    scale_radius(name="Movements", range = c(0.3, 1))
  
  # add legend
  if(!legend) {
    p + guides(radius=FALSE)
  } else {
    p
  }
}
