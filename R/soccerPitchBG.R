#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @importFrom ggforce geom_arc geom_circle
#' @importFrom cowplot draw_text
NULL
#' Plot a full soccer pitch
#'
#' @description Draws a soccer pitch as a ggplot object for the purpose of adding layers such as player positions, player trajectories, etc..
#' 
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param fillPitch,colPitch pitch fill and line colour
#' @param arrow adds team direction of play arrow as right (\code{'r'}) or left (\code{'l'}); \code{'none'} by default
#' @param title,subtitle adds title and subtitle to plot; NULL by default
#' @param theme palette of pitch background and lines, either \code{light} (default), \code{dark}, \code{grey}, or \code{grass}
#' @param data a default dataset for plotting in subsequent layers; NULL by default
#' @return a ggplot object
#' @seealso \code{\link{soccermatics-deprecated}}
#' @keywords internal
#' @rdname soccermatics-deprecated
#' @export
soccerPitchBG <- function(lengthPitch = 105, widthPitch = 68, arrow = c("none", "r", "l"), title = NULL, subtitle = NULL, theme = c("light", "dark", "grey", "grass"), data = NULL) {
  .Deprecated("soccerPitch")
  start<-end<-NULL
  
  # define colours by theme
  if(theme[1] == "grass") {
    fill1 <- "#008000"
    fill2 <- "#328422"
    colPitch <- "grey85"
    arrowCol <- "white"
    colText <- "white"
  } else if(theme[1] == "light") {
    fill1 <- "grey98"
    fill2 <- "grey98"
    colPitch <- "grey60"
    arrowCol = "black"
    colText <- "black"
  } else if(theme[1] %in% c("grey", "gray")) {    
    fill1 <- "#A3A1A3"
    fill2 <- "#A3A1A3"
    colPitch <- "white"
    arrowCol <- "white"
    colText <- "black"
  } else if(theme[1] == "dark") {  
    fill1 <- "#1C1F26"
    fill2 <- "#1C1F26"
    colPitch <- "white"
    arrowCol <- "white"
    colText <- "white"
  } else if(theme[1] == "blank") {
    fill1 <- "white"
    fill2 <- "white"
    colPitch <- "white"
    arrowCol <- "black"
    colText <- "black"
  }
  lwd <- 0.5
  
  # outer border (t,r,b,l)
  border <- c(10, 6, 5, 6)
  
  # mowed grass lines
  lines <- (lengthPitch + border[2] + border[4]) / 13
  boxes <- data.frame(start = lines * 0:12 - border[4], end = lines * 1:13 - border[2])[seq(2, 12, 2),]
  
  # draw pitch
  p <- ggplot(data) +
    # background
    geom_rect(aes(xmin = -border[4], xmax = lengthPitch + border[2], ymin = -border[3], ymax = widthPitch + border[1]), fill = fill1) +
    # mowed pitch lines
    geom_rect(data = boxes, aes(xmin = start, xmax = end, ymin = -border[3], ymax = widthPitch + border[1]), fill = fill2) +
    # perimeter line
    geom_rect(aes(xmin = 0, xmax = lengthPitch, ymin = 0, ymax = widthPitch), fill = NA, col = colPitch, lwd = lwd) +
    # centre circle
    geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 9.15), col = colPitch, lwd = lwd) +
    # kick off spot
    geom_circle(aes(x0 = lengthPitch/2, y0 = widthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # halfway line
    geom_segment(aes(x = lengthPitch/2, y = 0, xend = lengthPitch/2, yend = widthPitch), col = colPitch, lwd = lwd) +
    # penalty arcs
    geom_arc(aes(x0= 11, y0 = widthPitch/2, r = 9.15, start = pi/2 + 0.9259284, end = pi/2 - 0.9259284), col = colPitch, lwd = lwd) +
    geom_arc(aes(x0 = lengthPitch - 11, y0 = widthPitch/2, r = 9.15, start = pi/2*3 - 0.9259284, end = pi/2*3 + 0.9259284), col = colPitch, lwd = lwd) +
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
    theme(rect = element_blank(), 
          line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  # add arrow
  if(arrow[1] == "r") {
    p <- p + 
      geom_segment(aes(x = 0, y = -2, xend = lengthPitch / 3, yend = -2), colour = arrowCol, size = 1.5, arrow = arrow(length = unit(0.2, "cm"), type="closed"), linejoin='mitre')
  } else if(arrow[1] == "l") {
    p <- p + 
      geom_segment(aes(x = lengthPitch, y = -2, xend = lengthPitch / 3 * 2, yend = -2), colour = arrowCol, size = 1.5, arrow = arrow(length = unit(0.2, "cm"), type="closed"), linejoin='mitre')
  }
  
  # add title and/or subtitle
  theme_buffer <- ifelse(theme[1] == "light", 0, 4)
  if(!is.null(title) & !is.null(subtitle)) {
    p <- p +
      draw_text(title, 
                         x = 0, y = widthPitch + 9, hjust = 0, vjust = 1,
                         size = 15, fontface = 'bold', col = colText) +
      draw_text(subtitle, 
                         x = 0, y = widthPitch + 4.5, hjust = 0, vjust = 1,
                         size = 13, col = colText) +
      theme(plot.margin = unit(c(-0.525,-0.9,-0.7,-0.9), "cm"))
  } else if(!is.null(title) & is.null(subtitle)) {
    p <- p +
      draw_text(title, 
                         x = 0, y = widthPitch + 4.5, hjust = 0, vjust = 1,
                         size = 15, fontface = 'bold', col = colText) +
      theme(plot.margin = unit(c(-0.9,-0.9,-0.7,-0.9), "cm"))
  } else if(is.null(title) & !is.null(subtitle)) {
    p <- p +
      draw_text(subtitle, 
                         x = 0, y = widthPitch + 4.5, hjust = 0, vjust = 1,
                         size = 13, col = colText) +
      theme(plot.margin = unit(c(-0.9,-0.9,-0.7,-0.9), "cm"))
  } else if(is.null(title) & is.null(subtitle)){
    p <- p +
      theme(plot.margin = unit(c(-1.2,-0.9,-0.7,-0.9), "cm"))
  }
  

  return(p)
  
}
