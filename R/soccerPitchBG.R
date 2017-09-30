#' @include soccerPitchFG.R
NULL
#' Draw a soccer pitch.
#'
#' @description Draws a soccer pitch as a ggplot object for the purpose of adding layers such as player positions, player trajectories, etc..
#' 
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param grass if TRUE, draws pitch background in green and lines in white. If FALSE, draws pitch background in white and lines in black.
#' @return a ggplot object
#' @examples
#' # get x,y-coords of player #8 during first 10 minutes
#' data(tromso)
#' dd <- subset(tromso, id == 9)[1:1200,]
#' # draw player path on pitch
#' soccerPitchBG(lengthPitch = 105, widthPitch = 68, grass = TRUE) + 
#'   geom_path(data = dd, aes(x, y), lwd = 2)
#' 
#' @seealso \code{\link{soccerPitchFG}} for drawing a soccer pitch as foreground over an existing ggplot object
#' @export
soccerPitchBG <- function(lengthPitch = 105, widthPitch = 68, grass = FALSE) {
  # set draw colours
  if(grass) {
    bg_col = "#008000"
    fg_col = "white"
  } else {
    bg_col = "white"
    fg_col = "black"
  }
  
  #draw
  ggplot() +
    # pitch
    geom_rect(aes(xmin = -4, xmax = lengthPitch + 4, ymin = -4, ymax = widthPitch + 4), fill = bg_col) +
    # outer lines
    geom_rect(aes(xmin = 0, xmax = lengthPitch, ymin = 0, ymax = widthPitch), fill = bg_col, col = fg_col, lwd = 1.2) +
    # centre circle
    geom_circle(aes(x0 = lengthPitch / 2, y0 = widthPitch / 2, r = 9.15), fill = bg_col, col = fg_col, lwd = 1.2) +
    # kick off spot
    geom_circle(aes(x0 = lengthPitch / 2, y0 = widthPitch / 2, r = 0.5), fill = fg_col, col = fg_col, lwd = 1.2) +
    # halfway line
    geom_segment(aes(x = lengthPitch / 2, y = 0, xend = lengthPitch / 2, yend = widthPitch), col = fg_col, lwd = 1.2) +
    # penalty arcs
    geom_arc(aes(x0= 11, y0 = widthPitch / 2, r = 9.15, start = 0.65, end = 2.49), col= fg_col, lwd = 1.2) +
    geom_arc(aes(x0 = lengthPitch - 11, y0 = widthPitch / 2, r = 9.15, start = 3.79, end = 5.63), col= fg_col, lwd = 1.2) +
    # penalty areas
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = widthPitch / 2 - (40.3 / 2), ymax = widthPitch / 2 + (40.3 / 2)), fill = bg_col, col = fg_col, lwd = 1.2) +
    geom_rect(aes(xmin = lengthPitch - 16.5, xmax = lengthPitch, ymin = widthPitch / 2 - (40.3 / 2), ymax = widthPitch / 2 + (40.3 / 2)), fill = bg_col, col = fg_col, lwd = 1.2) +
    # penalty spots
    geom_circle(aes(x0 = 11, y0 = widthPitch / 2, r = 0.5), fill = fg_col, col = fg_col, lwd = 1.2) +
    geom_circle(aes(x0 = lengthPitch - 11, y0 = widthPitch / 2, r = 0.5), fill = fg_col, col = fg_col, lwd = 1.2) +
    # six yard boxes
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = (widthPitch / 2) - 9.16, ymax = (widthPitch / 2) + 9.16), fill = bg_col, col = fg_col, lwd = 1.2) +
    geom_rect(aes(xmin = lengthPitch - 5.5, xmax = lengthPitch, ymin = (widthPitch / 2) - 9.16, ymax = (widthPitch / 2) + 9.16), fill = bg_col, col = fg_col, lwd = 1.2) +
    # goals
    geom_rect(aes(xmin = -2, xmax = 0, ymin = (widthPitch / 2) - 3.66, ymax = (widthPitch / 2) + 3.66), fill = bg_col, col = fg_col, lwd = 1.2) +
    geom_rect(aes(xmin = lengthPitch, xmax = lengthPitch + 2, ymin = (widthPitch / 2) - 3.66, ymax = (widthPitch / 2) + 3.66), fill = bg_col, col = fg_col, lwd = 1.2) +
    coord_fixed() +
    xlab("") +
    ylab("") +
    theme(rect = element_blank(), 
          line = element_blank(),
          axis.text = element_blank())
}
