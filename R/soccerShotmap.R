#' @include soccerPitch.R
#' @include soccerPitchHalf.R
#' @import ggplot2
#' @importFrom cowplot draw_text
NULL
#' Draw an individual, team, or two team shotmap using StatsBomb data
#'
#' @description If \code{df} contains two teams, draws a shotmap of each team at either end of a full pitch. If \code{df} contains one or more players from a single team, draws a vertical half pitch. Currently only works with StatsBomb data but compatability with other (non-StatsBomb) shot data will be added soon.
#' 
#' @param lengthPitch,widthPitch length and width of pitch, in metres
#' @param homeTeam if \code{df} contains two teams, the name of the home team to be displayed on the left hand side of the pitch. If \code{NULL}, infers home team as the team of the first event in \code{df}.
#' @param theme draws a \code{light}, \code{dark}, \code{grey}, or \code{grass} coloured pitch with appropriate point colours
#' @param title,subtitle optional, adds title and subtitle to half pitch plot. Title defaults to scoreline and team identity when two teams are defined in \code{df}.
#' @return a ggplot object
#' @examples
#' library(dplyr)
#' data(statsbomb)
#' 
#' # shot map of two teams on full pitch
#' statsbomb %>%
#'   soccerShotmap(theme = "gray")
#' 
#' # shot map of one player on half pitch
#' statsbomb %>% 
#'   filter(player.name == "Antoine Griezmann") %>% 
#'   soccerShotmap(theme = "grass",
#'                 title = "Antoine Griezmann", 
#'                 subtitle = "vs. Argentina, World Cup 2018")
#' 
#' @export
soccerShotmap <- function(df, lengthPitch = 105, widthPitch = 68, homeTeam = NULL, theme = c("light", "dark", "grey", "grass"), title = NULL, subtitle = NULL) {
  
  # define colours by theme
  if(theme[1] == "grass") {
    colGoal <- "#E77100"
    colMiss <- "#234987"
    colText <- "white"
  } else if(theme[1] == "light") {
    colGoal <- "#E77100"
    colMiss <- "#93a5c1"
    colText <- "black"
  } else if(theme[1] %in% c("grey", "gray")) {
    colGoal <- "#efa340"
    colMiss <- "#4c6896"
    colText <- "black"
  } else {
    colGoal <- "#E77100"
    colMiss <- "#88adea"
    colText <- "white"
  }
  
  # full pitch shotmap for two teams
  if(length(unique(df$team.name)) > 1) {
    
    # home team taken as first team in df if unspecified
    if(is.null(homeTeam)) homeTeam <- df$team.name[1]
    awayTeam <- unique(df$team.name)[unique(df$team.name) != homeTeam]
    
    # flip x,y-coordinates of home team and factorise variables
    df <- df %>% 
      soccerFlipDirection(teamToFlip = homeTeam, periodToFlip = 1:2, x = "location.x", y = "location.y", team = "team.name") %>% 
      mutate(shot.outcome = as.factor(if_else(shot.outcome.name == "Goal", 1, 0)),
             penalty = as.factor(if_else(shot.type.name == "Penalty", 1, 0)),
             team.name = factor(team.name, levels = c(homeTeam, awayTeam)))
    
    # actual goals (plus own goals)
    goals <- df %>% 
      group_by(team.name) %>% 
      filter(type.name == "Shot") %>% 
      dplyr::summarise(g = length(shot.outcome.name[shot.outcome.name == "Goal"]) + length(type.name[type.name == "Own Goal For"]))
    
    # expected goals
    xg_totals <- df %>% 
      filter(penalty == 0) %>% 
      group_by(team.name) %>% 
      dplyr::summarise(xg = sum(shot.statsbomb_xg))
    
    # penalties
    pen_totals <- df %>% 
      group_by(team.name) %>% 
      filter(type.name == "Shot") %>% 
      dplyr::summarise(pen = length(shot.outcome[penalty == 1 & shot.outcome == 1]))
    
    # own goals
    og_totals <- df %>% 
      group_by(team.name) %>% 
      dplyr::summarise(og = length(type.name[type.name == "Own Goal For"]))
    
    # xg labels
    xg1 <- sprintf("%.2f", xg_totals$xg[1])
    if(pen_totals$pen[1] > 0 & og_totals$og[1] == 0) {
      xg1 <- paste0("(+", pen_totals$pen[1], " P) ", xg1)
    } else if(pen_totals$pen[1] == 0 & og_totals$og[1] > 0) {
      xg1 <- paste0("(+", og_totals$og[1], " OG) ", xg1)
    } else if(pen_totals$pen[1] > 0 & og_totals$og[1] > 0) {
      xg1 <- paste0("(+", pen_totals$pen[1], " P, +", og_totals$og[1], " OG) ", xg1)
    }
    xg2 <- sprintf("%.2f", xg_totals$xg[2])
    if(pen_totals$pen[2] > 0) {
      xg2 <- paste0(xg2, " (+", pen_totals$pen[2], " P)")
    } else if(pen_totals$pen[2] == 0 & og_totals$og[2] > 0) {
      xg2 <- paste0(xg2, " (+", og_totals$og[2], " OG)")
    } else if(pen_totals$pen[2] > 0 & og_totals$og[2] > 0) {
      xg2 <- paste0(xg2, " (+", pen_totals$pen[2], " P, +", og_totals$og[2], " OG) )")
    }
    
    # subset shots for plotting
    df <- df %>%
      filter(type.name == "Shot" & penalty == 0) %>% 
      mutate(size = scales::rescale(shot.statsbomb_xg, c(1, 15), c(0, 1))) %>% 
      arrange(as.numeric(shot.outcome), size)
    
    # plot
    p <- soccerPitch(lengthPitch, widthPitch, theme = theme[1]) +
      geom_point(data = df, aes(x = location.x, y = location.y, size = size, colour = shot.outcome), alpha = 0.8) +
      scale_size_identity() +
      scale_colour_manual(name = "Outcome", breaks = c(0,1), values = c(colMiss, colGoal)) +
      guides(colour = FALSE, size = FALSE)
    
    # add labels
    p <- p +
      draw_text(paste0(xg_totals$team.name[1], "  ", goals$g[1]), x = lengthPitch / 2 - 1, y = widthPitch + 5, hjust = 1, vjust = 1, size = 15, fontface = 'bold', colour = colText) +
      draw_text(":", x = lengthPitch / 2, y = widthPitch + 5, hjust = 0.5, vjust = 1, size = 15, fontface = 'bold', colour = colText) +
      draw_text(paste0(goals$g[2], "  ", xg_totals$team.name[2]), x = lengthPitch / 2 + 1, y = widthPitch + 5, hjust = 0, vjust = 1, size = 15, fontface = 'bold', colour = colText) +
      draw_text(xg1, x = lengthPitch / 2 - 1, y = widthPitch - 5, hjust = 1, vjust = 0, size = 15, colour = colText) +
      draw_text(xg2, x = lengthPitch / 2 + 1, y = widthPitch - 5, hjust = 0, vjust = 0, size = 15, colour = colText) +
      theme(plot.margin = unit(c(-0.9,-0.9,-0.7,-0.9), "cm"))
    
    # half pitch if one team
  } else {
    
    df <- df %>% 
      mutate(shot.outcome = as.factor(if_else(shot.outcome.name == "Goal", 1, 0)),
             penalty = as.factor(if_else(shot.type.name == "Penalty", 1, 0)))
    
    goals <- df %>% 
      filter(type.name == "Shot") %>% 
      dplyr::summarise(g = length(shot.outcome.name[shot.outcome.name == "Goal"]) + length(type.name[type.name == "Own Goal For"]))
    
    # expected goals
    xg_totals <- df %>% 
      filter(penalty == 0) %>% 
      dplyr::summarise(xg = sum(shot.statsbomb_xg))
    
    # penalties
    pen_totals <- df %>% 
      filter(type.name == "Shot") %>% 
      dplyr::summarise(pen = length(shot.outcome[penalty == 1 & shot.outcome == 1]))
    
    df <- df %>% 
      filter(type.name == "Shot" & penalty == 0) %>% 
      mutate(size = scales::rescale(shot.statsbomb_xg, c(1, 15), c(0, 1))) %>% 
      arrange(as.numeric(shot.outcome), size)
    
    p <- soccerPitchHalf(lengthPitch, widthPitch, title = title, subtitle = subtitle, theme = theme[1]) +
      geom_point(data = df, aes(x = location.y, y = location.x, size = size, colour = shot.outcome), alpha = 0.8) +
      scale_size_identity() +
      scale_colour_manual(name = "Outcome", breaks = c(0,1), values = c(colMiss, colGoal)) +
      guides(colour = FALSE, size = FALSE)
    
    xg <- sprintf("%.2f", xg_totals$xg)
    if(pen_totals$pen > 0) {
      xg <- paste0(xg, " (+", pen_totals$pen, " P)")
    }
    
    p <- p +
      annotate("text", x = 2, y = lengthPitch/2 + 6, label = paste0("Goals: ", goals$g), hjust = 0, vjust = 0, size = 5, colour = colText) +
      annotate("text", x = 2, y = lengthPitch/2 + 2, label = paste0("xG: ", xg), hjust = 0, vjust = 0, size = 5, colour = colText)
    
  }
  
  return(p)
  
}
