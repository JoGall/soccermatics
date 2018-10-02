#' @include soccerPitchFG.R
#' @import ggplot2
#' @import dplyr
NULL
#' Plot average player position
#' @description Draws the average x,y-positions of each player from one or both teams on a soccer pitch.
#' 
#' @param df dataframe containing x,y-coordinates of player position
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres
#' @param fill1,fill2 character, fill colour of position points (team 1, team 2 (if present))
#' @param col1,col2 character, border colour of position points and labels (team 1, team 2 (if present))
#' @param node_size numeric, size of position points
#' @param label boolean, draw labels or not
#' @param label_size numeric, size of label names
#' @param fillPitch pitch fill colour
#' @param colPitch pitch line colour
#' @param lwd pitch line width
#' @param grass if \code{TRUE}, uses a more realistic pitch
#' @param arrow optional, adds arrow showing team attack direction as right (\code{'r'}) or left (\code{'l'})
#' @param title,subtitle optional, adds title and subtitle to plot
#' @param x,y = name of variables containing x,y-coordinates
#' @param id character, the name of the column containing player identity. Defaults to \code{'id'}
#' @param team character, the name of the column containing team identity. Optional, defaults to \code{'NULL'}
#' @examples
#' # Tromso IL average player position
#' data(tromso)
#' soccerPositionMap(tromso, grass = TRUE)
#' 
#' # France average pass position
#' data(statsbomb)
#' statsbomb %>% 
#'   filter(type.name == "Pass" & team.name == "France" & minute < 43) %>% 
#'   soccerPositionMap(id = "player.name", x = "location.x", y = "location.y", 
#'                     fill1 = "blue", grass = T,
#'                     arrow = "r", 
#'                     title = "France (vs Argentina, 30th June 2016)", 
#'                     subtitle = "Average pass position (1' - 42')")
#' 
#' @seealso \code{\link{soccerPitch}}  for plotting a soccer pitch for the purpose of drawing over event data, average position, player trajectories, etc..
#' @export
soccerPositionMap <- function(df, lengthPitch = 105, widthPitch = 68, fill1 = "red", col1 = "white", fill2 = "blue", col2 = "white", node_size = 6, label = TRUE, label_size = 3, fillPitch = "white", colPitch = "grey60", lwd = 0.5, grass = FALSE, arrow = c("none", "r", "l"), title = NULL, subtitle = NULL, x = "x", y = "y", id = "id", team = NULL) {
  
  if(!is.null(team)) {
    # get average position for both teams
    pos <- df %>%
      group_by_(team, id) %>%
      summarise(x.mean = mean(!!sym(x)), y.mean = mean(!!sym(y))) %>% 
      ungroup() %>% 
      mutate_(team = team, id = id) %>% 
      mutate(team = as.factor(team), id = as.factor(id))
    
    p <- soccerPitch(fillPitch = fillPitch, colPitch = colPitch, arrow = arrow, grass = grass, title = title, subtitle = subtitle) +
      geom_point(aes(x.mean, y.mean, group = team, fill = team, colour = team), data = pos, shape = 21, size = 6, stroke = 1.3) +
      scale_colour_manual(values = c(col1, col2)) +
      scale_fill_manual(values = c(fill1, fill2)) +
      guides(colour = FALSE, fill = FALSE)
    
  } else {
    # get average position for one team
    pos <- df %>%
      group_by_(id) %>%
      summarise(x.mean = mean(!!sym(x)), y.mean = mean(!!sym(y))) %>% 
      ungroup() %>% 
      mutate_(id = id) %>% 
      mutate(id = as.factor(id))
    
    p <- soccerPitch(fillPitch = fillPitch, colPitch = colPitch, grass = grass, arrow = arrow, title = title, subtitle = subtitle) +
      geom_point(aes(x.mean, y.mean), data = pos, col = col1, fill = fill1, shape = 21, size = node_size, stroke = 1.3)
  }
  
  # add labels
  if(label) {
    if(!is.null(team)) {
      p <- p +
        geom_text(aes(x.mean, y.mean, label = id, group = team, colour = team), data = pos, hjust=0.5, vjust=0.5, fontface = "bold", size = label_size)
    } else {
      p <- p +
        geom_text(aes(x.mean, y.mean, label = id), data = pos, hjust=0.5, vjust=0.5, fontface = "bold", size = label_size)
    }
  }
  
  # add arrow
  
  
  return(p)
}
