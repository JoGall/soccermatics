#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr "%>%"
NULL
#' Draw a path of player trajectory on a soccer pitch using any tracking data
#'
#' @description Draws a path connecting consecutive x,y-coordinates of a player on a soccer pitch. 
#' 
#' @param df dataframe containing x,y-coordinates of player position
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param col colour of path if no \code{'id'} is provided; if an \code{'id'} is present, uses ColorBrewer's 'Paired' palette by default
#' @param arrow adds team direction of play arrow as right (\code{'r'}) or left (\code{'l'}); \code{'none'} by default
#' @param theme draws a \code{light}, \code{dark}, \code{grey}, or \code{grass} coloured pitch
#' @param lwd player path thickness
#' @param title,subtitle adds title and subtitle to plot; NULL by default
#' @param legend boolean, include legend
#' @param x,y name of variables containing x,y-coordinates
#' @param id character, the name of the column containing player identity (only required if \code{'df'} contains multiple players)
#' @param plot base plot to add path layer to; NULL by default
#' @return a ggplot object
#' @examples
#' library(dplyr)
#' data(tromso)
#' 
#' # draw path of Tromso #8 over first 3 minutes (1800 frames)
#' tromso %>% 
#'   filter(id == 8) %>% 
#'   top_n(1800) %>%
#'   soccerPath(col = "red", theme = "grass", arrow = "r")
#'   
#' # draw path of all Tromso players over first minute (600 frames)
#' tromso %>%
#'   group_by(id) %>%
#'   slice(1:1200) %>%
#'   soccerPath(id = "id", theme = "light")
#' 
#' @export
soccerPath <- function(df, lengthPitch = 105, widthPitch = 68, col = "black", arrow = c("none", "r", "l"), theme = c("light", "dark", "grey", "grass"), lwd = 1, title = NULL, subtitle = NULL, legend = FALSE, x = "x", y = "y", id = NULL, plot = NULL) {
  
  # ensure input is dataframe
  df <- as.data.frame(df)
  
  if(is.null(id)) {
    # one player
    if(missing(plot)) {
      p <- soccerPitch(lengthPitch, widthPitch, arrow = arrow, theme = theme[1], title = title, subtitle = subtitle) + 
        geom_path(data = df, aes(x, y), col = col, lwd = lwd)
    } else {
      p <- plot +
        geom_path(data = df, aes(x, y), col = col, lwd = lwd)
    }
  } else {
    # multiple players
    if(missing(plot)) {
      p <- soccerPitch(lengthPitch, widthPitch, arrow = arrow, theme = theme[1], title = title, subtitle = subtitle) + 
        geom_path(data = df, aes_string("x", "y", group = id, colour = id), lwd = lwd) +
        scale_colour_brewer(type = "seq", palette = "Paired", labels = 1:12)
    } else {
      p <- plot + 
        geom_path(data = df, aes_string("x", "y", group = id, colour = id), lwd = lwd) +
        scale_colour_brewer(type = "seq", palette = "Paired", labels = 1:12)
    }
    
    #legend
    if(legend == FALSE) {
      p <- p +
        guides(colour="none")
    }
  }
  
  return(p)
  
}
