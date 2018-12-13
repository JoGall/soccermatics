#' @include soccerPitch.R
#' @import ggplot2
#' @import dplyr
#' @importFrom ggrepel geom_label_repel
#' @importFrom forcats fct_explicit_na
#' @importFrom scales rescale
NULL
#' Draw a passing network using StatsBomb data
#' 
#' @description Draw an undirected passing network of completed passes on pitch from StatsBomb data. Nodes are scaled by number of successful passes; edge width is scaled by number of successful passes between each node pair. Only passes made until first substition shown (ability to specify custom minutes will be added soon). Total number of passes attempted and percentage of completed passes shown. Compatability with other (non-StatsBomb) shot data will be added soon.
#' 
#' @param df dataframe containing x,y-coordinates of player passes
#' @param lengthPitch,widthPitch numeric, length and width of pitch, in metres
#' @param fill,col fill and border colour of nodes
#' @param edgeCol colour of edge lines. Default is complementary to \code{theme} colours.
#' @param edgeAlpha transparency of edge lines, from \code{0} - \code{1}. Defaults to \code{0.6} so overlapping edges are visible.
#' @param label boolean, draw labels
#' @param shortNames shorten player names to display last name as label
#' @param maxNodeSize maximum size of nodes
#' @param maxEdgeSize maximum width of edge lines
#' @param labelSize size of player name labels
#' @param arrow optional, adds arrow showing team attack direction as right (\code{'r'}) or left (\code{'l'})
#' @param theme draws a \code{light}, \code{dark}, \code{grey}, or \code{grass} coloured pitch
#' @param title adds custom title to plot. Defaults to team name.
#' @examples
#' # France vs. Argentina, minimum of three passes
#' library(dplyr)
#' 
#' # Argentina pass map until first substituton with transparent edges
#' data(statsbomb)
#' statsbomb %>% 
#'   filter(team.name == "Argentina") %>% 
#'   soccerPassmap(fill = "lightblue", arrow = "r",
#'                 title = "Argentina (vs France, 30th June 2018)")
#' 
#' # France pass map until first substituton with opaque edges
#' statsbomb %>% 
#'   filter(team.name == "France") %>% 
#'   soccerPassmap(fill = "blue", minPass = 3,
#'                 maxEdgeSize = 30, edgeCol = "grey40", edgeAlpha = 1,
#'                 title = "France (vs Argentina, 30th June 2018)")
#' @export
soccerPassmap <- function(df, lengthPitch = 105, widthPitch = 68, minPass = 3, fill = "red", col = "black", edgeAlpha = 0.6, edgeCol = NULL, label = TRUE, shortNames = TRUE, maxNodeSize = 30, maxEdgeSize = 30, labelSize = 4, arrow = c("none", "r", "l"), theme = c("light", "dark", "grey", "grass"), title = NULL) {
  
  if(length(unique(df$team.name)) > 1) stop("Data contains more than one team")
  
  # define colours by theme
  if(theme[1] == "grass") {
    colText <- "white"
    if(is.null(edgeCol)) edgeCol <- "black"
  } else if(theme[1] == "light") {
    colText <- "black"
    if(is.null(edgeCol)) edgeCol <- "black"
  } else if(theme[1] %in% c("grey", "gray")) {
    colText <- "black"
    if(is.null(edgeCol)) edgeCol <- "black"
  } else {
    colText <- "white"
    if(is.null(edgeCol)) edgeCol <- "white"
  }
  
  # set variable names
  x <- "location.x"
  y <- "location.y"
  id <- "player.id"
  name <- "player.name"
  team <- "team.name"

  df$x <- df[,x]
  df$y <- df[,y]
  df$id <- df[,id]
  df$name <- df[,name]
  df$team <- df[,team]
  

  # full game passing stats for labels
  passes <- df %>% 
    filter(type.name == "Pass") %>% 
    group_by(pass.outcome.name) %>% 
    tally() %>% 
    filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown")) %>% 
    mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete"))
  pass_n <- sum(passes$n)
  pass_pc <- passes[passes$pass.outcome.name == "Complete",]$n / pass_n * 100
  
  
  # filter events before time of first substitution, if at least one substitution
  min_events <- df %>% 
    group_by(id) %>% 
    dplyr::summarise(period = min(period), timestamp = min(timestamp)) %>% 
    na.omit() %>% 
    arrange(period, timestamp)
  
  if(nrow(min_events) > 11) {
    max_event <- min_events[12,]
    idx <- which(df$period == max_event$period & df$timestamp == max_event$timestamp) - 1
    df <- df[1:idx,]
  }
  
  
  # get nodes and edges for plotting
  # node position and size based on touches
  nodes <- df %>% 
    filter(type.name %in% c("Pass", "Ball Receipt*", "Ball Recovery", "Shot", "Dispossessed", "Interception", "Clearance", "Dribble", "Shot", "Goal Keeper", "Miscontrol", "Error")) %>% 
    group_by(id, name) %>% 
    dplyr::summarise(x = mean(x, na.rm=T), y = mean(y, na.rm=T), events = n()) %>% 
    na.omit() %>% 
    as.data.frame()
  
  # edges based only on completed passes
  edgelist <- df %>% 
    mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
    filter(type.name == "Pass" & pass.outcome.name == "Complete") %>% 
    select(from = player.name, to = pass.recipient.name) %>% 
    group_by(from, to) %>% 
    dplyr::summarise(n = n()) %>% 
    na.omit()
  
  edges <- left_join(edgelist, 
            nodes %>% select(id, name, x, y),
            by = c("from" = "name"))
  
  edges <- left_join(edges, 
            nodes %>% select(id, name, xend = x, yend = y),
            by = c("to" = "name"))
  
  edges <- edges %>% 
    group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
    dplyr::summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])
  
  
  # filter minimum number of passes and rescale line width
  nodes <- nodes %>% 
    mutate(events = rescale(events, c(2, maxNodeSize), c(1, 200)))

  # rescale node size
  edges <- edges %>% 
    filter(n >= minPass) %>%
    mutate(n = rescale(n, c(1, maxEdgeSize), c(minPass, 75)))
  
  
  # shorten player name
  if(shortNames) {
    nodes$name <- soccerShortenName(nodes$name)
  }
  
  # if no title given, use team
  if(is.null(title)) {
    title <- unique(df$team)
  }
  
  subtitle <- paste0(min(df$minute)+1, "' - ", max(df$minute)+1, "', ", minPass, "+ passes shown")
  
  # plot network
  p <- soccerPitch(lengthPitch, widthPitch, 
                     arrow = arrow[1], theme = theme[1],
                     title = title, 
                     subtitle = subtitle) +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, size = n), col = edgeCol, alpha = edgeAlpha) +
    geom_point(data = nodes, aes(x, y, size = events), pch = 21, fill = fill, col = col) +
    scale_size_identity() +
    guides(size = F) +
    annotate("text", 104, 1, label = paste0("Passes: ", pass_n, "\nCompleted: ", sprintf("%.1f", pass_pc), "%"), hjust = 1, vjust = 0, size = labelSize * 7/8, col = colText)
  
  # add labels
  if(label) {
    p <- p +
      geom_label_repel(data = nodes, aes(x, y, label = name), size = labelSize)
  }
  
  return(p)
  
}
