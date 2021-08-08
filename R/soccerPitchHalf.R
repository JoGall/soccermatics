#' @include soccerPitch.R
#' @include soccerShotmap.R
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom ggforce geom_arc geom_circle
#' @importFrom cowplot draw_text
NULL
#' Draws a vertical half soccer pitch for the purpose of plotting shotmaps
#'
#' @description Adds soccer pitch outlines (with transparent fill) to an existing ggplot object (e.g. heatmaps, passing maps, etc..)
#' 
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param arrow adds team direction of play arrow as right (\code{'r'}) or left (\code{'l'}); \code{'none'} by default
#' @param theme palette of pitch background and lines, either \code{light} (default), \code{dark}, \code{grey}, or \code{grass}; 
#' @param title,subtitle adds title and subtitle to plot; NULL by default
#' @param data a default dataset for plotting in subsequent layers; NULL by default
#' @return a ggplot object
#' @seealso \code{\link{soccerShotmap}} for plotting a shotmap on a half pitch for a single player or \code{\link{soccerPitch}} for drawing a full size soccer pitch
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' data(statsbomb)
#' 
#' # normalise data, get non-penalty shots for France, 
#' # add boolean variable 'goal' for plotting
#' my_df <- statsbomb %>%
#'   soccerTransform(method = 'statsbomb') %>% 
#'   filter(team.name == "France" &
#'          type.name == "Shot" &
#'          shot.type.name != 'penalty') %>%
#'   mutate(goal = as.factor(if_else(shot.outcome.name == "Goal", 1, 0)))
#'   
#' soccerPitchHalf(data = my_df, theme = 'light') +
#'   geom_point(aes(x = location.y, y = location.x,
#'                  size = shot.statsbomb_xg, colour = goal),
#'              alpha = 0.7)
#'             
#' @export
soccerPitchHalf <- function(lengthPitch = 105, widthPitch = 68, arrow = c("none", "r", "l"), theme = c("light", "dark", "grey", "grass"), title = NULL, subtitle = NULL, data = NULL) {
  start<-end<-NULL
  
  # define colours by theme
  if(theme[1] == "grass") {
    fill1 <- "#008000"
    fill2 <- "#328422"
    colPitch <- "grey85"
    arrowCol <- "white"
    colText <- "white"
  } else if(theme[1] == "light") {
    fill1 <- "white"
    fill2 <- "white"
    colPitch <- "grey60"
    arrowCol = "black"
    colText <- "black"
  } else if(theme[1] %in% c("grey", "gray")) {    
    fill1 <- "#A3A1A3"
    fill2 <- "#A3A1A3"
    colPitch <- "white"
    arrowCol <- "white"
    colText <- "black"
  } else {
    fill1 <- "#1a1e2c"
    fill2 <- "#1a1e2c"
    colPitch <- "#F0F0F0"
    arrowCol <- "#F0F0F0"
    colText <- "#F0F0F0"
  }
  lwd <- 0.5
  
  # outer border (t,r,b,l)
  border <- c(12, 6, 1, 6)
  
  # mowed grass lines
  lines <- (lengthPitch + border[2] + border[4]) / 13
  boxes <- data.frame(start = lines * 0:12 - border[4], end = lines * 1:13 - border[2])[seq(2, 12, 2),]
  
  # draw pitch
  p <- ggplot(data) +
    # background
    geom_rect(aes(xmin = -border[4], xmax = widthPitch + border[2], ymin = lengthPitch/2 - border[3], ymax = lengthPitch + border[1]), fill = fill1) +
    # mowed pitch lines
    geom_rect(data = boxes, aes(ymin = start, ymax = end, xmin = -border[4], xmax = widthPitch + border[2]), fill = fill2) +
    # perimeter line
    geom_rect(aes(xmin = 0, xmax = widthPitch, ymin = lengthPitch/2, ymax = lengthPitch), fill = NA, col = colPitch, lwd = lwd) +
    # centre circle
    geom_arc(aes(x0 = widthPitch/2, y0 = lengthPitch/2, r = 9.15, start = pi/2, end = -pi/2), col = colPitch, lwd = lwd) +
    # kick off spot
    geom_circle(aes(x0 = widthPitch/2, y0 = lengthPitch/2, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # halfway line
    geom_segment(aes(x = 0, y = lengthPitch/2, xend = widthPitch, yend = lengthPitch/2), col = colPitch, lwd = lwd) +
    # penalty arc
    geom_arc(aes(x0 = widthPitch/2, y0 = lengthPitch - 11, r = 9.15, start = pi * 0.705, end = 1.295 * pi), col = colPitch, lwd = lwd) +
    # penalty area
    geom_rect(aes(xmin = widthPitch/2 - 20.15, xmax = widthPitch/2 + 20.15, ymin = lengthPitch - 16.5, ymax = lengthPitch), fill = NA, col = colPitch, lwd = lwd) +
    # penalty spot
    geom_circle(aes(x0 = widthPitch/2, y0 = lengthPitch - 11, r = 0.25), fill = colPitch, col = colPitch, lwd = lwd) +
    # six yard box
    geom_rect(aes(xmin = (widthPitch/2) - 9.16, xmax = (widthPitch/2) + 9.16, ymin = lengthPitch - 5.5, ymax = lengthPitch), fill = NA, col = colPitch, lwd = lwd) +
    # goal
    geom_rect(aes(xmin = (widthPitch/2) - 3.66, xmax = (widthPitch/2) + 3.66, ymin = lengthPitch, ymax = lengthPitch + 2), fill = NA, col = colPitch, lwd = lwd) +
    coord_fixed(ylim = c(lengthPitch/2 - border[3], lengthPitch + border[1])) +
    theme(rect = element_blank(), 
          line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  # add title and/or subtitle
  theme_buffer <- ifelse(theme[1] == "light", 0, 4)
  if(!is.null(title) & !is.null(subtitle)) {
    p <- p +
      draw_text(title, 
                         x = widthPitch/2, y = lengthPitch + 10, hjust = 0.5, vjust = 1,
                         size = 15, fontface = 'bold', col = colText) +
      draw_text(subtitle, 
                         x = widthPitch/2, y = lengthPitch + 6.5, hjust = 0.5, vjust = 1,
                         size = 13, col = colText) +
      theme(plot.margin = unit(c(-0.7,-1.4,-0.7,-1.4), "cm"))
  } else if(!is.null(title) & is.null(subtitle)) {
    p <- p +
      draw_text(title, 
                         x = widthPitch/2, y = lengthPitch + 6.5, hjust = 0.5, vjust = 1,
                         size = 15, fontface = 'bold', col = colText) +
      theme(plot.margin = unit(c(-1.2,-1.4,-0.7,-1.4), "cm"))
  } else if(is.null(title) & !is.null(subtitle)) {
    p <- p +
      draw_text(subtitle, 
                         x = widthPitch/2, y = lengthPitch + 6.5, hjust = 0.5, vjust = 1,
                         size = 13, col = colText) +
      theme(plot.margin = unit(c(-1.2,-1.4,-0.7,-1.4), "cm"))
  } else if(is.null(title) & is.null(subtitle)){
    p <- p +
      theme(plot.margin = unit(c(-1.85,-1.4,-0.7,-1.4), "cm"))
  }
  
  
  return(p)
  
}
