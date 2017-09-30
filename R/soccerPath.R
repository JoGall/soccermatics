NULL
#' Draw a path of player trajectory on a soccer pitch.
#'
#' @description Draws a path connecting consecutive x,y-coordinates of a player on a soccer pitch. 
#' 
#' @param df dataframe containing x,y-coordinates of player position in columns named \code{'x'} and \code{'y'}.
#' @param id_var character, the name of the column containing player identity. Only required if \code{'df'} contains multiple players.
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param grass if TRUE, draws pitch background in green and lines in white. If FALSE, draws pitch background in white and lines in black.
#' @param col colour of path if no \code{'id_var'} is provided. If an \code{'id_var'} is present, colours from ColorBrewer's 'Paired' palette are used.
#' @param lwd thickness of path
#' @return a ggplot object
#' @examples
#' data(tromso)
#' # draw path of player #8 over first 1200 frames
#' subset(tromso, id == 8)[1:1200,] %>%
#'   soccerPath(col = "red", grass = TRUE)
#' # draw path of all players over first 1200 frames
#' tromso %>%
#'   group_by(id) %>%
#'   slice(1:1200) %>%
#'   soccerPath("id")
#' 
#' @export
soccerPath <- function(df, id_var = NULL, lengthPitch = 105, widthPitch = 68, grass = FALSE, col = "black", lwd = 1, legend = TRUE) {
  
  if(is.null(id_var)) {
    # one player
    soccerPitchBG(lengthPitch = 105, widthPitch = 68, grass = grass) + 
    geom_path(data = df, aes(x, y), colour = col, lwd = lwd)
  } else {
    # multiple players
    if(legend) {
      soccerPitchBG(lengthPitch = 105, widthPitch = 68, grass = grass) + 
        geom_path(data = df, aes_string("x", "y", group = id_var, colour = id_var), lwd = lwd) +
        scale_colour_brewer(type = "seq", palette = "Paired", labels = 1:12)
    } else {
      soccerPitchBG(lengthPitch = 105, widthPitch = 68, grass = grass) + 
        geom_path(data = df, aes_string("x", "y", group = id_var, colour = id_var), lwd = lwd) +
        scale_colour_brewer(type = "seq", palette = "Paired", labels = 1:12) +
        guides(colour=FALSE)
    }
  }
}
