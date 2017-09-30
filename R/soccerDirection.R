#' @include soccerPitchBG.R
#' @include soccerPitchFG.R
NULL
#' Add an arrow showing the direction of play to a soccer pitch ggplot.
#' @description Draws an arrow showing the direction of play at the top of an existing soccer pitch ggplot.
#' 
#' @param plot an existing ggplot object to add arrow to.
#' @param direction character, direction of arrow (\code{"right"} or \code{"left"}).
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param arrow_col character, colour of arrow (defaults to \code{"black"}).
#' @param grass if TRUE, draws pitch background in green and lines in white. If FALSE, draws pitch background in white and lines in black.
#' @return a ggplot object
#' @examples
#' data(tromso)
#' # draw heatmap of player #9's position
#' p <- soccerHeatmap(subset(tromso, id == 9), bins = 15, lengthPitch = 105, widthPitch = 68)
#' # add arrow showing direction of play to the right
#' soccerDirection(p, "right", lengthPitch = 105, widthPitch = 68)
#' 
#' @seealso \code{\link{soccerPitchBG}} and \code{\link{soccerPitchFG}} for drawing a soccer pitch
#' @export
soccerDirection <- function(plot, direction = c("right", "left"), lengthPitch = 105, widthPitch = 68, arrow_col = "black", grass = FALSE) {
  
  bg_col <- ifelse(grass, "#008000", "white")
  direction <- match.arg(direction)
  
  # plot
  p <- plot +
    geom_rect(aes(xmin = -4, xmax = lengthPitch + 4, ymin = widthPitch + 4, ymax = widthPitch + 9), fill = bg_col)
  
  if(direction == "right") {
    p + 
      geom_segment(aes(x = lengthPitch * 1/4, xend = lengthPitch * 3/4, y = widthPitch + 4.5, yend = widthPitch + 4.5), size = 3, arrow = arrow(angle=20, type="closed", ends="last", length=unit(1,"cm")), col = arrow_col)
  } else {
    p + 
      geom_segment(aes(x = lengthPitch * 3/4, xend = lengthPitch * 1/4, y = widthPitch + 4.5, yend = widthPitch + 4.5), size = 3, arrow = arrow(angle=20, type="closed", ends="last", length=unit(1,"cm")), col = arrow_col)
  }
}
