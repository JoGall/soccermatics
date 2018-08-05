#' @include soccerHeatmap.R
#' @include soccerSpokes.R
#' @import ggplot2
#' @import dplyr
NULL
#' Draw a flow field on a soccer pitch.
#' @description Draws a flow field showing the mean direction of movement made in each sector of the pitch and adds pitch outlines. Note: This function is prototypical and intended to eventually visualise pass and shot event data, but there are no open-source samples of such data available as yet.
#' 
#' @param df dataframe containing x,y-coordinates of player position in columns named \code{'x'} and \code{'y'} and angular information (in radians, ranging between -pi and pi) in a column \code{'direction'}.
#' @param xBins,yBins integer, the number of horizontal (length-wise) and vertical (width-wise) bins the soccer pitch is to be divided up into. If no value for \code{yBins} is provided, it will take the value of \code{xBins}.
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param grass if TRUE, draws pitch background in green and lines in white. If FALSE, draws pitch background in white and lines in black.
#' @param line_col colour of pitch lines.
#' @param lwd thickness of arrow lines.
#' @param plot optional, adds wagon wheels to an existing ggplot object if provided
#' @return a ggplot object of a heatmap on a soccer pitch.
#' @examples
#' data(tromso_extra)
#' # draw flow field showing mean direction of player #8's movement
#' soccerFlow(subset(tromso_extra, id == 8), xBins = 5, grass = TRUE)
#' # draw flow field over player heatmap
#' p <- soccerHeatmap(subset(tromso_extra, id == 8), xBins = 5)
#' soccerFlow(subset(tromso_extra, id == 8), xBins = 5, plot = p)
#'
#' @seealso \code{\link{soccerHeatmap}} for drawing a heatmap of player position, or \code{\link{soccerSpokes}} for drawing spokes to show all directions in each area of the pitch.
#' @export
soccerFlow <- function(df, lengthPitch = 105, widthPitch = 68, xBins = 5, yBins = NULL, fillPitch = "white", colPitch = "grey60", grass = FALSE, lwd = 0.5, border = c(4, 4, 4, 4), plot = NULL) {
  
  # check value for vertical bins and match to horizontal bins if NULL
  if(is.null(yBins)) yBins <- xBins

  # adjust range and n bins
  x.range <- seq(0, lengthPitch, length.out = xBins+1)
  y.range <- seq(0, widthPitch, length.out = yBins+1)
  
  # bin plot values
  x.bin.coords <- data.frame(x.bin = 1:xBins, 
                             x.bin.coord = (x.range + (lengthPitch / (xBins) / 2))[1:xBins])
  y.bin.coords <- data.frame(y.bin = 1:yBins,
                             y.bin.coord = (y.range + (widthPitch / (yBins) / 2))[1:yBins])

  # bin data
  df <- df %>%
    rowwise() %>%
    mutate(x.bin = max(which(x > x.range)),
           y.bin = max(which(y > y.range)),
           bin = paste(x.bin, y.bin, sep = "_")) %>%
    ungroup()
  
  # summarise direction for each bin
  df <- df %>%
    group_by(bin) %>%
    select(id, bin, x.bin, y.bin, id, x, y, direction) %>%
    mutate(x.mean = mean(x),
           y.mean = mean(y),
           angle.mean = mean(direction, na.rm=T)) %>%
    ungroup()
  
  # add x,y-coords for bin centres
  df <- left_join(df, x.bin.coords, by = "x.bin")
  df <- left_join(df, y.bin.coords, by = "y.bin")
  
  if(missing(plot)) {
    soccerPitchBG(lengthPitch = lengthPitch, widthPitch = widthPitch, fillPitch = fillPitch, colPitch = colPitch, grass = grass) +
      geom_spoke(data = df, aes(x = x.bin.coord, y = y.bin.coord, angle = angle.mean), radius = widthPitch / (yBins+2), size = lwd, arrow=arrow(length = unit(0.2,"cm"))) +
      guides(fill=FALSE)
  } else {
    plot +
      geom_spoke(data = df, aes(x = x.bin.coord, y = y.bin.coord, angle = angle.mean), radius = widthPitch / (yBins+2), size = lwd, arrow=arrow(length = unit(0.2,"cm"))) +
      guides(fill=FALSE)
  }
}
