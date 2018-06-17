#' @include soccerPitchFG.R
#' @import ggplot2
#' @importFrom ggforce geom_arc geom_circle
NULL
#' Draw a soccer pitch.
#'
#' @description Draws a soccer pitch as a ggplot object for the purpose of adding layers such as player positions, player trajectories, etc..
#' 
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param fillPitch pitch fill colour
#' @param colPitch pitch line colour
#' @param grass if \code{TRUE}, draws a more realistic looking pitch
#' @param lwd pitch line width
#' @param border size of border drawn around pitch perimeter (t,r,b,l)
#' @return a ggplot object
#' @examples
#' # get x,y-coords of player #8 during first 10 minutes
#' data(tromso)
#' dd <- subset(tromso, id == 9)[1:1200,]
#' # draw player path on pitch
#' soccerPitchBG(lengthPitch = 105, widthPitch = 68, grass = TRUE) + 
#'   geom_path(data = dd, aes(x, y))
#' 
#' @seealso \code{\link{soccerPitchFG}} for drawing a soccer pitch as foreground over an existing ggplot object
#' @export
soccerPitchBG <- function(lengthPitch = 105, widthPitch = 68, fillPitch = "white", colPitch = "grey60", grass = FALSE, lwd = 0.5, border = c(4, 4, 4, 4), direction = c("none", "r", "l"), SB = FALSE) {

  if(grass) {
    fill1 <- "#008000"
    fill2 <- "#328422"
    colPitch <- "grey85"
  } else {
    fill1 <- fillPitch
    fill2 <- fillPitch
  }
  
  if(SB) {
    lengthPitch <- 106
    widthPitch <- 70.4
  }
  
  lines <- (lengthPitch + border[2] + border[1]) / 13
  boxes <- data.frame(start = lines * 0:12 - border[1], end = lines * 1:13 - border[2])[seq(2, 12, 2),]
  
  p <- ggplot() +
    # background
    geom_rect(aes(xmin = -border[1], xmax = lengthPitch + border[2], ymin = -border[3], ymax = widthPitch + border[4]), fill = fill1) +
    # mowed pitch lines
    geom_rect(data = boxes, aes(xmin = start, xmax = end, ymin = -border[3], ymax = widthPitch + border[4]), fill = fill2) +
    # perimeter line
    geom_rect(aes(xmin = 0, xmax = lengthPitch, ymin = 0, ymax = widthPitch), fill = NA, col = colPitch, lwd = lwd) +
    # centre circle
    geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 9.15), col = colPitch, lwd = lwd) +
    # kick off spot
    geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # halfway line
    geom_segment(aes(x = lengthPitch/2, y = 0, xend = lengthPitch/2, yend = widthPitch), col = colPitch, lwd = lwd) +
    # penalty arcs
    geom_arc(aes(x0= 11, y0 = widthPitch/2, r = 9.15, start = 0.65, end = 2.49), col = colPitch, lwd = lwd) +
    geom_arc(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 9.15, start = 3.79, end = 5.63), col = colPitch, lwd = lwd) +
    # penalty areas
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = widthPitch/2 - 20.15, ymax = widthPitch/2 + 20.15), fill = NA, col = colPitch, lwd = lwd) +
    geom_rect(aes(xmin = lengthPitch - 16.5, xmax = lengthPitch, ymin = widthPitch/2 - 20.15, ymax = widthPitch/2 + 20.15), fill = NA, col = colPitch, lwd = lwd) +
    # penalty spots
    geom_circle(aes(x0 = 11, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    geom_circle(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # six yard boxes
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = (widthPitch/2) - 9.16, ymax = (widthPitch/2) + 9.16), fill = NA, col = colPitch, lwd = lwd) +
    geom_rect(aes(xmin = lengthPitch - 5.5, xmax = lengthPitch, ymin = (widthPitch/2) - 9.16, ymax = (widthPitch/2) + 9.16), fill = NA, col = colPitch, lwd = lwd) +
    # goals
    geom_rect(aes(xmin = -2, xmax = 0, ymin = (widthPitch/2) - 3.66, ymax = (widthPitch/2) + 3.66), fill = NA, col = colPitch, lwd = lwd) +
    geom_rect(aes(xmin = lengthPitch, xmax = lengthPitch + 2, ymin = (widthPitch/2) - 3.66, ymax = (widthPitch/2) + 3.66), fill = NA, col = colPitch, lwd = lwd) +
    coord_fixed() +
    xlab("") +
    ylab("") +
    theme(rect = element_blank(), 
          line = element_blank(),
          axis.text = element_blank())
  
  if(direction[1] == "r") {
    p <- p + geom_segment(aes(x = 0, y = -5, xend = 30, yend = -5), colour = "#435366", size = 1.5, arrow = arrow(length = unit(0.2, "cm"), type="closed")) + 
      annotate("text", x = 0, y = -2, label = "Direction of play", colour = "#435366", fontface=2, size = 4, hjust = 0)
  } else if(direction[1] == "l") {
    p <- p + geom_segment(aes(x = lengthPitch, y = -5, xend = lengthPitch - 30, yend = -5), colour = "#435366", size = 1.5, arrow = arrow(length = unit(0.2, "cm"), type="closed")) + 
      annotate("text", x = lengthPitch, y = -2, label = "Direction of play", colour = "#435366", fontface=2, size = 4, hjust = 1)
  }
  
  p
  
}
