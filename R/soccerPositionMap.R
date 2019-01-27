#' @include soccerPitch.R
#' @import ggplot2
#' @import dplyr
#' @importFrom ggrepel geom_text_repel geom_label_repel
NULL
#' Plot average player position
#' @description Draws the average x,y-positions of each player from one or both teams on a soccer pitch.
#' 
#' @param df dataframe containing x,y-coordinates of player position
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres
#' @param fill1,fill2 character, fill colour of position points of team 1, team 2 (team 2 \code{NULL} by default)
#' @param col1,col2 character, border colour of position points of team 1, team 2 (team 2 \code{NULL} by default)
#' @param labelCol character, label text colour
#' @param homeTeam if \code{df} contains two teams, the name of the home team to be displayed on the left hand side of the pitch (i.e. attacking from left to right). If \code{NULL}, infers home team as the team of the first event in \code{df}.
#' @param flipAwayTeam flip x,y-coordinates of away team so attacking from right to left
#' @param label type of label to draw, player names (\code{name}), jersey numbers (\code{number}), or \code{none}
#' @param labelBox add box around label text
#' @param shortNames shorten player names to display last name as label
#' @param nodeSize numeric, size of position points
#' @param labelSize numeric, size of labels
#' @param arrow optional, adds arrow showing team attack direction as right (\code{'r'}) or left (\code{'l'})
#' @param theme draws a \code{light}, \code{dark}, \code{grey}, or \code{grass} coloured pitch
#' @param title,subtitle optional, adds title and subtitle to plot
#' @param source if \code{statsbomb}, uses StatsBomb definitions of required variable names (i.e. `location.x`, `location.y`, `player.id`, `team.name`). If \code{manual}, respects variable names defined in function arguments \code{x}, \code{y}, \code{id}, \code{name}, and \code{team}.
#' @param x,y,id,name,team names of variables containing x,y-coordinates, unique player ids, player names, and team names. If \code{team} remains NULL, function expects only one team.
#' @examples
#' library(dplyr)
#' 
#' # average player position for one team w/ jersey numbers
#' data(tromso)
#' tromso %>% 
#'   soccerPositionMap(label = "number", name = "id",
#'                     labelCol = "white", nodeSize = 8, arrow = "r", theme = "grass",
#'                     title = "Tromso IL (vs. Stromsgodset, 3rd Nov 2013)", 
#'                     subtitle = "Average player position (1' - 16')")
#' 
#' # average pass position for one team w/ player name labelled as text
#' data(statsbomb)
#' statsbomb %>%
#'   filter(type.name == "Pass" & team.name == "France" & period == 1) %>%
#'   soccerPositionMap(source = "statsbomb",
#'                     fill1 = "blue", arrow = "r", labelBox = F, theme = "grey", labelCol = "white",
#'                     title = "France (vs Argentina, 30th June 2018)",
#'                     subtitle = "Average pass position (1' - 45')")
#'                  
#' # average pass position for two teams w/ player name labelled as text in boxes
#' statsbomb %>%
#'   filter(type.name == "Pass" & period == 1) %>%
#'   soccerPositionMap(source = "statsbomb",
#'                     fill1 = "lightblue", fill2 = "blue",
#'                     title = "Argentina vs France, 30th June 2018",
#'                     subtitle = "Average pass position (1' - 45')")
#' 
#' @export
soccerPositionMap <- function(df, lengthPitch = 105, widthPitch = 68, fill1 = "red", col1 = NULL, fill2 = "blue", col2 = NULL, labelCol = "black", homeTeam = NULL, flipAwayTeam = TRUE, label = c("name", "number", "none"), labelBox = TRUE, shortNames = TRUE, nodeSize = 5, labelSize = 4, arrow = c("none", "r", "l"), theme = c("light", "dark", "grey", "grass"), title = NULL, subtitle = NULL, source = c("manual", "statsbomb"), x = "x", y = "y", id = "id", name = NULL, team = NULL) {
  
  # define colours by theme
  if(theme[1] == "grass") {
    colText <- "white"
  } else if(theme[1] == "light") {
    colText <- "black"
  } else if(theme[1] %in% c("grey", "gray")) {
    colText <- "black"
  } else {
    colText <- "white"
  }
  if(is.null(col1)) col1 <- fill1
  if(is.null(col2)) col2 <- fill2
  
  # set variable names
  if(source[1] == "statsbomb") {
    x <- "location.x"
    y <- "location.y"
    id <- "player.id"
    team <- "team.name"
    name <- "player.name"
  }
  
  df$x <- df[,x]
  df$y <- df[,y]
  df$id <- df[,id]
  if(is.null(name)) name <- id
  df$name <- df[,name]
  if(is.null(team)) team <- "Team A"
  df$team <- df[,team]
  
  # shorten player name
  if(!is.null(name) & shortNames == TRUE) {
    df$name <- soccerShortenName(df$name)
  }
  
  # if two teams in df
  if(length(unique(df$team)) > 1) {
    
    # home team taken as first team in df if unspecified
    if(is.null(homeTeam)) homeTeam <- df[,team][1]
    
    # flip x,y-coordinates of home team
    if(flipAwayTeam) {
      df <- df %>% 
        soccerFlipDirection(teamToFlip = homeTeam, periodToFlip = 1:2)
    }
    
    # get average positions
    pos <- df %>%
      group_by(team, id, name) %>%
      dplyr::summarise(x.mean = mean(x), y.mean = mean(y)) %>% 
      ungroup() %>%
      mutate(team = as.factor(team), id = as.factor(id)) %>%
      as.data.frame()
    
    # plot
    p <- soccerPitch(theme = theme[1], title = title, subtitle = subtitle) +
      geom_point(data = pos, aes(x.mean, y.mean, group = team, fill = team, colour = team), shape = 21, size = 6, stroke = 1.3) +
      scale_colour_manual(values = c(col1, col2)) +
      scale_fill_manual(values = c(fill1, fill2)) +
      guides(colour = FALSE, fill = FALSE)
    
  # if one team
  } else {
    # get average positions
    pos <- df %>%
      group_by(id, name) %>%
      dplyr::summarise(x.mean = mean(x), y.mean = mean(y)) %>% 
      # ungroup() %>% 
      # mutate(id = as.factor(id)) %>% 
      as.data.frame()
    
    # plot
    p <- soccerPitch(arrow = arrow, theme = theme[1], title = title, subtitle = subtitle) +
      geom_point(data = pos, aes(x.mean, y.mean), col = col1, fill = fill1, shape = 21, size = nodeSize, stroke = 1.3)
  }
  
  # add non-overlapping names as labels
  if(label[1] == "name") {
    if(labelBox) {
      p <- p +
        geom_label_repel(data = pos, aes(x.mean, y.mean, label = name), segment.colour = colText, segment.size = 0.3, max.iter = 1000, size = labelSize, fontface = "bold")
    } else {
      p <- p +
        geom_text_repel(data = pos, aes(x.mean, y.mean, label = name), col = labelCol, segment.colour = colText, segment.size = 0.3, max.iter = 1000, size = labelSize, fontface = "bold")
    }
  # add jersey numbers directly to points
  } else if(label[1] == "number") {
      p <- p +
        geom_text(data = pos, aes(x.mean, y.mean, label = name), col = labelCol, fontface = "bold")
  }
  
  return(p)
  
}