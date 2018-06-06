#' @include soccerPitchFG.R
#' @import ggplot2
#' @import dplyr
NULL
#' Plot average player position on a soccer pitch.
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
soccerPositionMap <- function(df, id_var = "id", group_var = NULL, lengthPitch = 105, widthPitch = 68, fill1 = "red", col1 = "white", fill2 = "blue", col2 = "white", node_size = 6, label_size = 3, label = TRUE, fillPitch = "white", colPitch = "grey60", grass = FALSE) {
  
  if(!is.null(group_var)) {
    # get average position of 11 players with most detected frames for both teams
    pos <- df %>%
      dplyr::filter(x > 0 & x < lengthPitch & y > 0 & y < widthPitch) %>%
      dplyr::group_by_(group_var, id_var) %>%
      dplyr::summarise(x.mean = mean(x), y.mean = mean(y), n = n()) %>% 
      ungroup() %>% 
      mutate_(team = group_var, id = id_var) %>% 
      mutate(team = as.factor(team), id = as.factor(id))
    
    bp <- soccerPitchBG(fillPitch = fillPitch, colPitch = colPitch, grass = grass) +
      geom_point(aes(x.mean, y.mean, group = team, fill = team, colour = team), data = pos, shape = 21, size = 6, stroke = 1.3) +
      scale_colour_manual(values = c(col1, col2)) +
      scale_fill_manual(values = c(fill1, fill2)) +
      guides(colour = FALSE, fill = FALSE)
    
  } else {
    # get average position of 11 players with most detected frames for one team
    pos <- df %>%
      dplyr::filter(x > 0 & x < lengthPitch & y > 0 & y < widthPitch) %>%
      dplyr::group_by_(id_var) %>%
      dplyr::summarise(x.mean = mean(x), y.mean = mean(y), n = n()) %>% 
      ungroup() %>% 
      mutate_(id = id_var) %>% 
      mutate(id = as.factor(id))
    
    bp <- soccerPitchBG(fillPitch = fillPitch, colPitch = colPitch, grass = grass) +
      geom_point(aes(x.mean, y.mean), data = pos, col = col1, fill = fill1, shape = 21, size = node_size, stroke = 1.3)
  }
  
  if(label) {
    if(!is.null(group_var)) {
      bp <- bp +
        geom_text(aes(x.mean, y.mean, label = id, group = team, colour = team), data = pos, hjust=0.5, vjust=0.5, fontface = "bold", size = label_size)
    } else {
      bp <- bp +
        geom_text(aes(x.mean, y.mean, label = id), data = pos, hjust=0.5, vjust=0.5, fontface = "bold", size = label_size)
    }
  }
  
  return(bp)
}
