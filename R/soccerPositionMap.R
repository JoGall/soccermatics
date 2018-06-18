#' @include soccerPitchFG.R
#' @import ggplot2
#' @import dplyr
NULL
#' Plot average player position
#' @description Draws the average x,y-positions of each player from one or both teams on a soccer pitch.
#' 
#' @param df dataframe containing x,y-coordinates of player position in columns named \code{'x'} and \code{'y'}
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres
#' @param id_var character, the name of the column containing player identity. Defaults to \code{'id'}
#' @param group_var character, the name of the column containing team identity. Optional, defaults to \code{'NULL'}
#' @param fill1,fill2 character, fill colour of position points for team one (and team two if `group_var` provided)
#' @param col1,col2 character, border colour of position points for team one (and team two if `group_var` provided)
#' @param node_size numeric, size of position points
#' @param label_size numeric, size of label names
#' @param label boolean, draw labels or not
#' @param fillPitch pitch fill colour
#' @param colPitch pitch line colour
#' @param grass if \code{TRUE}, draws a more realistic looking pitch
#' @param lwd pitch line width
#' @param border size of border drawn around pitch perimeter (t,r,b,l)
#' @examples
#' data(tromso)
#' # draw average player position of players
#' p <- soccerPositionMap(tromso, lengthPitch = 105, widthPitch = 68, grass = TRUE)
#' # draw arrow showing direction of play
#' soccerDirection(p, "right", lengthPitch = 105, widthPitch = 68, grass = TRUE)
#' 
#' @seealso \code{\link{soccerPitchBG}} for a background soccer pitch for the purpose of drawing position maps, player trajectories, etc...
#' @export
soccerPositionMap <- function(df, lengthPitch = 105, widthPitch = 68, id_var = "id", group_var = NULL, x_var = "x", y_var = "y", fill1 = "red", col1 = "white", fill2 = "blue", col2 = "white", node_size = 6, label_size = 3, label = TRUE, fillPitch = "white", colPitch = "grey60", lwd = 0.5, grass = FALSE) {
  
  if(!is.null(group_var)) {
    # get average position for both teams
    pos <- df %>%
      group_by_(group_var, id_var) %>%
      summarise(x.mean = mean(!!sym(x_var)), y.mean = mean(!!sym(y_var))) %>% 
      ungroup() %>% 
      mutate_(team = group_var, id = id_var) %>% 
      mutate(team = as.factor(team), id = as.factor(id))
    
    bp <- soccerPitchBG(fillPitch = fillPitch, colPitch = colPitch, grass = grass) +
      geom_point(aes(x.mean, y.mean, group = team, fill = team, colour = team), data = pos, shape = 21, size = 6, stroke = 1.3) +
      scale_colour_manual(values = c(col1, col2)) +
      scale_fill_manual(values = c(fill1, fill2)) +
      guides(colour = FALSE, fill = FALSE)
    
  } else {
    # get average position for one team
    pos <- df %>%
      group_by_(id_var) %>%
      summarise(x.mean = mean(!!sym(x_var)), y.mean = mean(!!sym(y_var))) %>% 
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
