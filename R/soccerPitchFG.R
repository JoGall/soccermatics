#' @include soccerPitchFG.R
#' @import ggplot2
#' @importFrom ggforce geom_arc geom_circle
NULL
#' Helper function to add soccer pitch outlines to an existing ggplot object
#'
#' @description Adds soccer pitch outlines (with transparent fill) to an existing ggplot object (e.g. heatmaps, passing maps, etc..)
#' 
#' @param plot an existing ggplot object to add pitch lines to
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param colPitch pitch fill and line colour
#' @param arrow optional, adds arrow showing team attack direction as right (\code{'r'}) or left (\code{'l'})
#' @param arrow_col colour of attack direction arrow
#' @param lwd numeric, pitch line width
#' @param title,subtitle optional, adds title and subtitle to plot
#' @return a ggplot object
#' 
#' @seealso \code{\link{soccerPitch}} for plotting a soccer pitch for the purpose of drawing over event data, average position, player trajectories, etc..
#' @export
soccerPitchFG <- function(plot, lengthPitch = 105, widthPitch = 68, colPitch = "black", arrow = c("none", "r", "l"), arrow_col = "black", lwd = 0.5, title = NULL, subtitle = NULL) {
  
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
    geom_arc(aes(x0= 11, y0 = widthPitch / 2, r = 9.15, start = 0.65, end = 2.49), col = colPitch, lwd = lwd) +
    geom_arc(aes(x0 = lengthPitch - 11, y0 = widthPitch / 2, r = 9.15, start = 3.79, end = 5.63), col = colPitch, lwd = lwd) +
    # six yard boxes
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = (widthPitch / 2) - 9.16, ymax = (widthPitch / 2) + 9.16), fill = "NA", col = colPitch, lwd = lwd) +
    geom_rect(aes(xmin = lengthPitch - 5.5, xmax = lengthPitch, ymin = (widthPitch / 2) - 9.16, ymax = (widthPitch / 2) + 9.16), fill = "NA", col = colPitch, lwd = lwd) +
    # goals
    geom_rect(aes(xmin = -2, xmax = 0, ymin = (widthPitch / 2) - 3.66, ymax = (widthPitch / 2) + 3.66), fill = "NA", col = colPitch, lwd = lwd) +
    geom_rect(aes(xmin = lengthPitch, xmax = lengthPitch + 2, ymin = (widthPitch / 2) - 3.66, ymax = (widthPitch / 2) + 3.66), fill = "NA", col = colPitch, lwd = lwd) +
    coord_fixed() +
    theme(rect = element_blank(), 
          line = element_blank(),
          text = element_blank())
  
  # add arrow
  if(arrow[1] == "r") {
    p <- p + 
      geom_segment(aes(x = 0, y = -3, xend = lengthPitch / 3, yend = -3), colour = arrow_col, size = 1.5, arrow = arrow(length = unit(0.2, "cm"), type="closed"), linejoin='mitre')
  } else if(arrow[1] == "l") {
    p <- p + 
      geom_segment(aes(x = lengthPitch, y = -3, xend = lengthPitch / 3 * 2, yend = -3), colour = arrow_col, size = 1.5, arrow = arrow(length = unit(0.2, "cm"), type="closed"), linejoin='mitre')
  }
  
  # add title
  if(!is.null(title)) {
    title_buffer <- ifelse(is.null(subtitle), 4, 8)
    
    p <- p +
      cowplot::draw_text(title, 
                         x = 0, y = widthPitch + title_buffer, hjust = 0, vjust = 1,
                         size = 14, fontface = 'bold')
  }
  
  # add subtitle
  if(!is.null(subtitle)) {
    p <- p +
      cowplot::draw_text(subtitle, 
                         x = 0, y = widthPitch + 4, hjust = 0, vjust = 1,
                         size = 12)
  }
  
  return(p)
  
}
