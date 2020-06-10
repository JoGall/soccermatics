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
#' @param adj adjust xG using conditional probability to account for multiple shots per possession
#' @param n_players number of highest xG players to display
#' @param size_lim minimum and maximum size of points, \code{c(min, max)}
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
soccerShotmap <- function(df, lengthPitch = 105, widthPitch = 68, homeTeam = NULL, adj = TRUE, n_players = 0, size_lim = c(2,15), theme = c("light", "dark", "grey", "grass"), title = NULL, subtitle = NULL) {
  
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
  
  # ensure input is dataframe
  df <- as.data.frame(df)
  
  # full pitch shotmap for two teams
  if(length(unique(df$team.name)) > 1) {

    # home team taken as first team in df if unspecified
    if(is.null(homeTeam)) homeTeam <- df$team.name[1]
    awayTeam <- unique(df$team.name)[unique(df$team.name) != homeTeam]

    # flip x,y-coordinates of home team and factorise variables
    df <- df %>%
      soccerFlipDirection(teamToFlip = homeTeam, x = "location.x", y = "location.y", team = "team.name") %>%
      mutate(shot.outcome = as.factor(if_else(shot.outcome.name == "Goal", 1, 0)),
             penalty = as.factor(if_else(shot.type.name == "Penalty", 1, 0)),
             team.name = factor(team.name, levels = c(homeTeam, awayTeam))) %>%
      rename(xg = shot.statsbomb_xg)

    # actual goals (including own goals)
    goals <- df %>%
      group_by(team.name) %>%
      filter(type.name == "Shot") %>%
      dplyr::summarise(g = length(shot.outcome.name[shot.outcome.name == "Goal"]) + length(type.name[type.name == "Own Goal For"]))

    # penalties
    pen_totals <- df %>%
      group_by(team.name) %>%
      filter(type.name == "Shot") %>%
      dplyr::summarise(pen = length(shot.outcome[penalty == 1 & shot.outcome == 1]))

    # own goals
    og_totals <- df %>%
      group_by(team.name) %>%
      dplyr::summarise(og = length(type.name[type.name == "Own Goal For"]))

    # adjust xG using conditional probability when there are multiple shots in a single possession
    if(adj) {
      df <- df %>%
        filter(type.name == "Shot" & penalty == 0) %>%
        group_by(team.name, possession) %>%
        mutate(xg_cond = (1 - prod(1 - xg))) %>%
        mutate(xg_adj = xg_cond * (xg / sum(xg))) %>%
        ungroup() %>%
        select(-xg, -xg_cond) %>%
        rename(xg = xg_adj)
    }

    # expected goals
    xg_totals <- df %>%
      filter(penalty == 0) %>%
      group_by(team.name) %>%
      dplyr::summarise(xg = sum(xg))

    # labels
    score1 <- goals$g[1] + og_totals$og[1]
    score2 <- goals$g[2] + og_totals$og[2]
    
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
      mutate(size = scales::rescale(xg, size_lim, c(0, 1))) %>%
      arrange(as.numeric(shot.outcome), size)

    # plot
    p <- soccerPitch(lengthPitch, widthPitch, theme = theme[1]) +
      geom_point(data = df, aes(x = location.x, y = location.y, size = size, colour = shot.outcome), alpha = 0.8) +
      scale_size_identity() +
      scale_colour_manual(name = "Outcome", breaks = c(0,1), values = c(colMiss, colGoal)) +
      guides(colour = FALSE, size = FALSE)

    # add labels
    p <- p +
      draw_text(paste0(xg_totals$team.name[1], "  ", score1), x = lengthPitch / 2 - 1, y = widthPitch + 5, hjust = 1, vjust = 1, size = 15, fontface = 'bold', colour = colText) +
      draw_text(":", x = lengthPitch / 2, y = widthPitch + 5, hjust = 0.5, vjust = 1, size = 15, fontface = 'bold', colour = colText) +
      draw_text(paste0(score2, "  ", xg_totals$team.name[2]), x = lengthPitch / 2 + 1, y = widthPitch + 5, hjust = 0, vjust = 1, size = 15, fontface = 'bold', colour = colText) +
      draw_text(xg1, x = lengthPitch / 2 - 1, y = widthPitch - 5, hjust = 1, vjust = 0, size = 15, colour = colText) +
      draw_text(xg2, x = lengthPitch / 2 + 1, y = widthPitch - 5, hjust = 0, vjust = 0, size = 15, colour = colText) +
      theme(plot.margin = unit(c(-0.9,-0.9,-0.7,-0.9), "cm"))

    # top xG by player
    if(n_players > 0) {
      top_xgs <- df %>%
        group_by(player.name, team.name) %>%
        summarise(xg = sum(xg, na.rm=T)) %>%
        ungroup() %>%
        mutate(name = soccerShortenName(player.name)) %>%
        arrange(-xg) %>%
        head(n_players) %>%
        group_by(team.name) %>%
        mutate(rowid = 1:n()) %>%
        ungroup() %>%
        mutate(label = if_else(team.name == homeTeam,
                               sprintf("%s %s", name, sprintf("%.2f", xg)),
                               sprintf("%s %s", sprintf("%.2f", xg), name)),
               x = if_else(team.name == homeTeam, lengthPitch/2 - 1, lengthPitch/2 + 1),
               hjust = if_else(team.name == homeTeam, 1, 0),
               y = widthPitch - 5 - (rowid * 2.5))

      p <- p +
        geom_text(data = top_xgs[top_xgs$team.name == homeTeam,], aes(x, y, label = label, hjust = hjust), size = 4, colour = colText) +
        geom_text(data = top_xgs[top_xgs$team.name != homeTeam,], aes(x, y, label = label, hjust = hjust), size = 4, colour = colText)
    }

  # half pitch if one team
  } else {
    
    df <- df %>%
      mutate(shot.outcome = as.factor(if_else(shot.outcome.name == "Goal", 1, 0)),
             penalty = as.factor(if_else(shot.type.name == "Penalty", 1, 0))) %>% 
      rename(xg = shot.statsbomb_xg)
    
    # goals
    goals <- df %>%
      filter(type.name == "Shot") %>%
      dplyr::summarise(g = length(shot.outcome.name[shot.outcome.name == "Goal"]) + length(type.name[type.name == "Own Goal For"]))
    
    # penalties
    pen_totals <- df %>%
      filter(type.name == "Shot") %>%
      dplyr::summarise(pen = length(shot.outcome[penalty == 1 & shot.outcome == 1]))

    # adjust xG using conditional probability when multiple shots in a single possession
    if(adj) {
      df <- df %>%
        filter(type.name == "Shot" & penalty == 0) %>%
        group_by(team.name, possession) %>%
        mutate(xg_cond = (1 - prod(1 - xg))) %>%
        mutate(xg_adj = xg_cond * (xg / sum(xg))) %>%
        ungroup() %>%
        select(-xg, -xg_cond) %>%
        rename(xg = xg_adj)
    }
    
    # expected goals
    xg <- df %>%
      filter(penalty == 0) %>%
      dplyr::summarise(xg = sum(xg)) %>% 
      pull %>% 
      sprintf("%.2f", .)
    
    df <- df %>%
      filter(type.name == "Shot" & penalty == 0) %>%
      mutate(size = scales::rescale(xg, size_lim, c(0, 1))) %>%
      arrange(as.numeric(shot.outcome), size)
    
    p <- soccerPitchHalf(lengthPitch, widthPitch, theme = theme[1], title = title, subtitle = subtitle) +
      geom_point(data = df, aes(x = location.y, y = location.x, size = size, colour = shot.outcome), alpha = 0.7) +
      scale_size_identity() +
      scale_colour_manual(name = "Outcome", breaks = c(0,1), values = c(colMiss, colGoal)) +
      guides(colour = FALSE, size = FALSE)
    
    # top xG by player
    if(n_players > 0) {
      top_xgs <- df %>%
        group_by(player.name, position.name) %>%
        summarise(xg = sum(xg, na.rm=T)) %>%
        ungroup() %>%
        mutate(name = soccerShortenName(player.name)) %>%
        arrange(-xg) %>%
        head(n_players) %>%
        arrange(xg) %>%
        mutate(rowid = 1:n()) %>%
        mutate(label = sprintf("%s %s", sprintf("%.2f", xg), name),
        y = lengthPitch/2 + (rowid * 2.5))
    
    p <- p +
      geom_text(data = top_xgs, aes(x = 2, y, label = label, hjust = 0), size = 4, colour = colText) +
        annotate("text", x = 2, y = lengthPitch/2 + 6 + max(top_xgs$rowid * 2.5), label = paste0("Goals: ", goals$g), hjust = 0, vjust = 0, size = 5, colour = colText) +
        annotate("text", x = 2, y = lengthPitch/2 + 2 + max(top_xgs$rowid * 2.5), label = paste0("xG: ", xg), hjust = 0, vjust = 0, size = 5, colour = colText)
    } else {
      p <- p +
        annotate("text", x = 2, y = lengthPitch/2 + 6, label = paste0("Goals: ", goals$g), hjust = 0, vjust = 0, size = 5, colour = colText) +
        annotate("text", x = 2, y = lengthPitch/2 + 2, label = paste0("xG: ", xg), hjust = 0, vjust = 0, size = 5, colour = colText)
    }
  
  }
  
  return(p)
  
}
