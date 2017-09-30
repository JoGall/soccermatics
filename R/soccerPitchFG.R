#' @include soccerPitchFG.R
NULL
#' Add soccer pitch outlines to an existing ggplot
#'
#' @description Draws soccer pitch outlines (with transparent fill) over an existing ggplot object to provide context for heatmaps, passing maps, etc..
#' 
#' @param plot an existing ggplot object to add layers to.
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @return a ggplot object
#' @examples
#' data(tromso)
#' # draw heatmap of player #9's position
#' p <- soccerHeatmap(subset(tromso, id == 8), bins = 15, lengthPitch = 105, widthPitch = 68)
#' # add pitch lines to plot
#' soccerPitchFG(p, lengthPitch = 105, widthPitch = 68)
#' 
#' @seealso \code{\link{soccerPitchBG}} for a background soccer pitch for the purpose of drawing position maps, player trajectories, etc..
#' @export
soccerPitchFG <- function(plot, lengthPitch = 105, widthPitch = 68) {
  fg_col <- "black"
  
  plot +
    geom_rect(aes(xmin = -4, xmax = lengthPitch + 4, ymin = -4, ymax = widthPitch + 4), fill = "NA") +
    # outer lines
    geom_rect(aes(xmin = 0, xmax = lengthPitch, ymin = 0, ymax = widthPitch), fill = "NA", col = fg_col, lwd = 1.2) +
    # centre circle
    geom_circle(aes(x0 = lengthPitch / 2, y0 = widthPitch / 2, r = 9.15), fill = "NA", col = fg_col, lwd = 1.2) +
    # kick off spot
    geom_circle(aes(x0 = lengthPitch / 2, y0 = widthPitch / 2, r = 0.5), fill = fg_col, col = fg_col, lwd = 1.2) +
    # halfway line
    geom_segment(aes(x = lengthPitch / 2, y = 0, xend = lengthPitch / 2, yend = widthPitch), col = fg_col, lwd = 1.2) +
    # penalty areas
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = widthPitch / 2 - (40.3 / 2), ymax = widthPitch / 2 + (40.3 / 2)), fill = "NA", col = fg_col, lwd = 1.2) +
    geom_rect(aes(xmin = lengthPitch - 16.5, xmax = lengthPitch, ymin = widthPitch / 2 - (40.3 / 2), ymax = widthPitch / 2 + (40.3 / 2)), fill = "NA", col = fg_col, lwd = 1.2) +
    # penalty spots
    geom_circle(aes(x0 = 11, y0 = widthPitch / 2, r = 0.5), fill = fg_col, col = fg_col, lwd = 1.2) +
    geom_circle(aes(x0 = lengthPitch - 11, y0 = widthPitch / 2, r = 0.5), fill = fg_col, col = fg_col, lwd = 1.2) +
    # penalty arcs
    geom_arc(aes(x0= 11, y0 = widthPitch / 2, r = 9.15, start = 0.65, end = 2.49), col = fg_col, lwd = 1.2) +
    geom_arc(aes(x0 = lengthPitch - 11, y0 = widthPitch / 2, r = 9.15, start = 3.79, end = 5.63), col = fg_col, lwd = 1.2) +
    # six yard boxes
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = (widthPitch / 2) - 9.16, ymax = (widthPitch / 2) + 9.16), fill = "NA", col = fg_col, lwd = 1.2) +
    geom_rect(aes(xmin = lengthPitch - 5.5, xmax = lengthPitch, ymin = (widthPitch / 2) - 9.16, ymax = (widthPitch / 2) + 9.16), fill = "NA", col = fg_col, lwd = 1.2) +
    # goals
    geom_rect(aes(xmin = -2, xmax = 0, ymin = (widthPitch / 2) - 3.66, ymax = (widthPitch / 2) + 3.66), fill = "NA", col = fg_col, lwd = 1.2) +
    geom_rect(aes(xmin = lengthPitch, xmax = lengthPitch + 2, ymin = (widthPitch / 2) - 3.66, ymax = (widthPitch / 2) + 3.66), fill = "NA", col = fg_col, lwd = 1.2) +
    coord_fixed() +
    theme(rect = element_blank(), 
          line = element_blank(),
          text = element_blank())
}
