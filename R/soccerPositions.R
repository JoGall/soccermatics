#' @include soccerPitchFG.R
#' @import ggplot2
#' @import dplyr
NULL
#' Plot average player position on a soccer pitch. Note: Function soon to be deprecated in favour of `soccerPositionMap`.
#' @description Draws the average x,y-positions of all players in a dataframe and plots over
#' a soccer pitch.
#' 
#' @param df dataframe containing x,y-coordinates of player position in columns named \code{'x'} and \code{'y'}.
#' @param id_var character, the name of the column containing player identity. Defaults to \code{'id'}.
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param col1 character, fill colour of position points.
#' @param col2 character, border colour of position points.
#' @param size numeric, size of position points and text.
#' @param grass if TRUE, draws pitch background in green and lines in white. If FALSE, draws pitch background in white and lines in black.
#' @examples
#' data(tromso)
#' # draw average player position of players
#' p <- soccerPositions(tromso, lengthPitch = 105, widthPitch = 68, grass = TRUE)
#' # draw arrow showing direction of play
#' soccerDirection(p, "right", lengthPitch = 105, widthPitch = 68, grass = TRUE)
#' 
#' @seealso \code{\link{soccerPitchBG}} for a background soccer pitch for the purpose of drawing position maps, player trajectories, etc..
#' @export
soccerPositions <- function(df, id_var = "id", lengthPitch = 105, widthPitch = 68, col1 = "red", col2 = "white", size = 8, grass = FALSE, plot = NULL) {
  # get average position of 11 players with most detected frames
  pos <- df %>%
    filter(x > 0 & x < lengthPitch & y > 0 & y < widthPitch) %>%
    group_by_(id_var) %>%
    summarise(x.mean = mean(x), y.mean = mean(y), n = n())
    # arrange(desc(n)) %>%
    # head(11)
  
  if(missing(plot)) {
    soccerPitchBG(lengthPitch = lengthPitch, widthPitch = widthPitch, grass = grass) +
      geom_point(data = pos, aes(x.mean, y.mean), shape = 16, size = size, colour = col1, stroke = 1.5) +
      geom_text(data = pos, aes(x.mean, y.mean, label=id), hjust=0.5, vjust=0.5, colour = col2, fontface = "bold", size = size - 4)
  } else {
    plot + 
      geom_point(data = pos, aes(x.mean, y.mean), shape = 16, size = size, colour = col1) +
      geom_text(data = pos, aes(x.mean, y.mean, label=id), hjust=0.5, vjust=0.5, colour = col2, fontface = "bold", size = size - 4)
  }
}
