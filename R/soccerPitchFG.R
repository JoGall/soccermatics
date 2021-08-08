#' @include soccerPitchFG.R
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom ggforce geom_arc geom_circle
NULL
#' Helper function to draw soccer pitch outlines over an existing ggplot object
#'
#' @description Adds soccer pitch outlines (with transparent fill) to an existing ggplot object (e.g. heatmaps, passing maps, etc..)
#' 
#' @param plot an existing ggplot object to add pitch lines layer to
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param colPitch colour of pitch markings
#' @param arrow adds team direction of play arrow as right (\code{'r'}) or left (\code{'l'}); \code{'none'} by default
#' @param title,subtitle adds title and subtitle to plot; NULL by default
#' @return a ggplot object
#' 
#' @seealso \code{\link{soccerPitch}} for plotting a soccer pitch as background layer
#' @export
soccerPitchFG <- function(plot, lengthPitch = 105, widthPitch = 68, colPitch = "black", arrow = c("none", "r", "l"), title = NULL, subtitle = NULL) {
  
  lwd <- 0.5
  
  p <- plot +
    geom_rect(aes(xmin = -4, xmax = lengthPitch + 4, ymin = -4, ymax = widthPitch + 4), fill = "NA") +
    # outer lines
    geom_rect(aes(xmin = 0, xmax = lengthPitch, ymin = 0, ymax = widthPitch), fill = "NA", col = colPitch, lwd = lwd) +
    # centre circle
    geom_circle(aes(x0 = lengthPitch / 2, y0 = widthPitch / 2, r = 9.15), fill = "NA", col = colPitch, lwd = lwd) +
    # kick off spot
    geom_circle(aes(x0 = lengthPitch / 2, y0 = widthPitch / 2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # halfway line
    geom_segment(aes(x = lengthPitch / 2, y = 0, xend = lengthPitch / 2, yend = widthPitch), col = colPitch, lwd = lwd) +
    # penalty areas
    geom_rect(aes(xmin = 0, xmax = 16.5, ymin = widthPitch / 2 - (40.3 / 2), ymax = widthPitch / 2 + (40.3 / 2)), fill = "NA", col = colPitch, lwd = lwd) +
    geom_rect(aes(xmin = lengthPitch - 16.5, xmax = lengthPitch, ymin = widthPitch / 2 - (40.3 / 2), ymax = widthPitch / 2 + (40.3 / 2)), fill = "NA", col = colPitch, lwd = lwd) +
    # penalty spots
    geom_circle(aes(x0 = 11, y0 = widthPitch / 2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    geom_circle(aes(x0 = lengthPitch - 11, y0 = widthPitch / 2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # penalty arcs
    geom_arc(aes(x0= 11, y0 = widthPitch/2, r = 9.15, start = pi/2 + 0.9259284, end = pi/2 - 0.9259284), col = colPitch, lwd = lwd) +
    geom_arc(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 9.15, start = pi/2*3 - 0.9259284, end = pi/2*3 + 0.9259284), col = colPitch, lwd = lwd) +
    # six yard boxes
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = (widthPitch / 2) - 9.16, ymax = (widthPitch / 2) + 9.16), fill = "NA", col = colPitch, lwd = lwd) +
    geom_rect(aes(xmin = lengthPitch - 5.5, xmax = lengthPitch, ymin = (widthPitch / 2) - 9.16, ymax = (widthPitch / 2) + 9.16), fill = "NA", col = colPitch, lwd = lwd) +
    # goals
    geom_rect(aes(xmin = -2, xmax = 0, ymin = (widthPitch / 2) - 3.66, ymax = (widthPitch / 2) + 3.66), fill = "NA", col = colPitch, lwd = lwd) +
    geom_rect(aes(xmin = lengthPitch, xmax = lengthPitch + 2, ymin = (widthPitch / 2) - 3.66, ymax = (widthPitch / 2) + 3.66), fill = "NA", col = colPitch, lwd = lwd) +
    theme(rect = element_blank(), 
          line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  # add title and/or subtitle
  if(title & subtitle) {
    p <- p +
      theme(plot.margin = unit(c(-0.525,-0.9,-0.7,-0.9), "cm"))
  } else if(title & !subtitle) {
    p <- p +
      theme(plot.margin = unit(c(-0.9,-0.9,-0.7,-0.9), "cm"))
  } else if(!title & subtitle) {
    p <- p +
      theme(plot.margin = unit(c(-0.9,-0.9,-0.7,-0.9), "cm"))
  } else if(!title & !subtitle) {
    p <- p +
      theme(plot.margin = unit(c(-1.2,-0.9,-0.7,-0.9), "cm"))
  }
  
  return(p)
  
}
