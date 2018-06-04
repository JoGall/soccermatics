#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
NULL
#' Draw a path of player trajectory on a soccer pitch.
#'
#' @description Draws a path connecting consecutive x,y-coordinates of a player on a soccer pitch. 
#' 
#' @param dat dataframe containing x,y-coordinates of player position in columns named \code{'x'} and \code{'y'}
#' @param id_var character, the name of the column containing player identity. Only required if \code{'dat'} contains multiple players
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param grass  if TRUE, draws a more realistic looking pitch
#' @param col colour of path if no \code{'id_var'} is provided. If an \code{'id_var'} is present, colours from ColorBrewer's 'Paired' palette are used
#' @param lwd player path thickness
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
#'   soccerPath("id")
#' 
#' @export
soccerPath <- function(dat, id_var = NULL, lengthPitch = 105, widthPitch = 68, col = "black", fillPitch = "white", colPitch = "grey60", grass = FALSE, lwd = 1, legend = TRUE, plot = NULL) {
  
  if(is.null(id_var)) {
    if(missing(plot)) {
      # one player
      p <- soccerPitchBG(lengthPitch = lengthPitch, widthPitch = widthPitch, fillPitch = fillPitch, colPitch = colPitch, grass = grass) + 
        geom_path(data = dat, aes(x, y), col = col, lwd = lwd)
    } else {
      p <- plot +
        geom_path(data = dat, aes(x, y), col = col, lwd = lwd)
    }
  } else {
    # multiple players
    if(missing(plot)) {
      p <- soccerPitchBG(lengthPitch = lengthPitch, widthPitch = widthPitch, fillPitch = fillPitch, colPitch = colPitch, grass = grass) + 
        geom_path(data = dat, aes_string("x", "y", group = id_var, colour = id_var), lwd = lwd) +
        scale_colour_brewer(type = "seq", palette = "Paired", labels = 1:12)
    } else {
      p <- plot + 
        geom_path(data = dat, aes_string("x", "y", group = id_var, colour = id_var), lwd = lwd) +
        scale_colour_brewer(type = "seq", palette = "Paired", labels = 1:12)
    }
    
    #legend
    if(legend == FALSE) {
      p +
        guides(colour=FALSE)
    }
  }
  p
}
