#' @import ggplot2
#' @import dplyr
NULL
#' Draw a path of player trajectory on a soccer pitch.
#'
#' @description Draws a path connecting consecutive x,y-coordinates of a player on a soccer pitch. 
#' 
#' @param dat dataframe containing x,y-coordinates of player position
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param id character, the name of the column containing player identity. Only required if \code{'dat'} contains multiple players
#' @param col colour of path if no \code{'id'} is provided. If an \code{'id'} is present, colours from ColorBrewer's 'Paired' palette are used
#' @param fillPitch,colPitch pitch background and line colour
#' @param grass if \code{TRUE}, uses a more realistic pitch
#' @param arrow optional, adds arrow showing team attack direction as right (\code{'r'}) or left (\code{'l'})
#' @param lwd player path thickness
#' @param title,subtitle optional, adds title and subtitle to plot
#' @param legend boolean, include legend
#' @param x,y = name of variables containing x,y-coordinates
#' @param plot plot to add path to, if desired
#' @return a ggplot object
#' @examples
#' data(tromso)
#' # draw path of player #8 over first 1200 frames
#' subset(tromso, id == 8)[1:1200,] %>%
#'   soccerPath(col = "red", grass = TRUE)
#' # draw path of all players over first 1200 frames
#' tromso %>%
#'   dplyr::group_by(id) %>%
#'   dplyr::slice(1:1200) %>%
#'   soccerPath(id = "id")
#' 
#' @export
soccerPath <- function(dat, lengthPitch = 105, widthPitch = 68, id = NULL, col = "black", colPitch = "grey60", fillPitch = "white", grass = FALSE, arrow = c("none", "r", "l"), lwd = 1, legend = TRUE, x = "x", y = "y", plot = NULL) {
  
  if(is.null(id)) {
    if(missing(plot)) {
      # one player
      p <- soccerPitch(lengthPitch = lengthPitch, widthPitch = widthPitch, fillPitch = fillPitch, colPitch = colPitch, grass = grass) + 
        geom_path(data = dat, aes(x, y), col = col, lwd = lwd)
    } else {
      p <- plot +
        geom_path(data = dat, aes(x, y), col = col, lwd = lwd)
    }
  } else {
    # multiple players
    if(missing(plot)) {
      p <- soccerPitch(lengthPitch = lengthPitch, widthPitch = widthPitch, fillPitch = fillPitch, colPitch = colPitch, grass = grass) + 
        geom_path(data = dat, aes_string("x", "y", group = id, colour = id), lwd = lwd) +
        scale_colour_brewer(type = "seq", palette = "Paired", labels = 1:12)
    } else {
      p <- plot + 
        geom_path(data = dat, aes_string("x", "y", group = id, colour = id), lwd = lwd) +
        scale_colour_brewer(type = "seq", palette = "Paired", labels = 1:12)
    }
    
    #legend
    if(legend == FALSE) {
      p +
        guides(colour=FALSE)
    }
  }
  
  return(p)
  
}
