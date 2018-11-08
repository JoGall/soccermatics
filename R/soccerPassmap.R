#' @include soccerPitchBG.R
#' @import ggplot2
#' @import dplyr
#' @importFrom ggrepel geom_text_repel
#' @importFrom forcats fct_explicit_na
#' @importFrom scales rescale
NULL
#' Draw a passing network on a pitch from StatsBomb data
#' 
#' @description Draw an undirected passing network of completed passes on pitch from StatsBomb data. Nodes are scaled by number of successful passes; edge width is scaled by number of successful passes between each node pair. Only passes made until first substition shown (ability to specify custom minutes will be added soon). Total number of passes attempted and percentage of completed passes shown. Compatability with other (non-StatsBomb) shot data will be added soon.
#' 
#' @param df dataframe containing x,y-coordinates of player passes
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres
#' @param fill,col fill and (border) colour of nodes
#' @param edgeCol,edgeAlpha colour and transparency of edge lines
#' @param maxEdgeSize maximum width of edge lines
#' @param maxNodeSize maximum size of nodes
#' @param labelSize size of player name labels
#' @param grass if \code{TRUE}, uses a more realistic pitch
#' @param arrow optional, adds arrow showing team attack direction as right (\code{'r'}) or left (\code{'l'})
#' @param title adds custom title to plot. Defaults to team name.
#' @param x,y = name of variables containing x,y-coordinates. Defaults to \code{'location.x'},\code{'location.y'} for StatsBomb data
#' @param id character, the name of the column containing unique player identity. Defaults to \code{'player.id'} for StatsBomb data
#' @param label character, the name of the column containing player name for labels. Defaults to \code{'player.name'} for StatsBomb data
#' @examples
#' # France vs. Argentina, minimum of three passes
#' library(dplyr)
#' library(soccermatics)
#' 
#' # Argentina pass map until first substituton with transparent edges
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
soccerPassmap <- function(df, lengthPitch = 105, widthPitch = 68, minPass = 3, fill = "red", col = "black", edgeCol = "black", edgeAlpha = 0.6, maxNodeSize = 30, maxEdgeSize = 30, labelSize = 4, grass = FALSE, arrow = c("none", "r", "l"), title = NULL, x = "location.x", y = "location.y", id = "player.id", label = "player.name", shortNames = TRUE) {
  
  if(length(unique(df$team.name)) > 1) stop("Data contains more than one team")
  
  df$x <- df[,x]
  df$y <- df[,y]
  df$id <- df[,id]
  df$label <- df[,label]
  

  # full game passing stats for labels
  passes <- df %>% 
    filter(type.name == "Pass") %>% 
    group_by(pass.outcome.name) %>% 
    tally() %>% 
    filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown")) %>% 
    mutate(pass.outcome.name = forcats::fct_explicit_na(pass.outcome.name, "Complete"))
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
    group_by(id, label) %>% 
    dplyr::summarise(x = mean(x, na.rm=T), y = mean(y, na.rm=T), events = n()) %>% 
    na.omit() %>% 
    as.data.frame()
  
  # edges based only on completed passes
  edgelist <- df %>% 
    mutate(pass.outcome.name = forcats::fct_explicit_na(pass.outcome.name, "Complete")) %>%
    filter(type.name == "Pass" & pass.outcome.name == "Complete") %>% 
    select(from = player.name, to = pass.recipient.name) %>% 
    group_by(from, to) %>% 
    dplyr::summarise(n = n()) %>% 
    na.omit()
  
  edges <- left_join(edgelist, 
            nodes %>% select(id, label, x, y),
            by = c("from" = "label"))
  
  edges <- left_join(edges, 
            nodes %>% select(id, label, xend = x, yend = y),
            by = c("to" = "label"))
  
  edges <- edges %>% 
    group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
    dplyr::summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])
  
  # filter minimum number of passes and rescale line width
  nodes <- nodes %>% 
    mutate(events = scales::rescale(events, c(2, maxNodeSize), c(1, 200)))

  # rescale node size
  edges <- edges %>% 
    filter(n >= minPass) %>%
    mutate(n = scales::rescale(n, c(1, maxEdgeSize), c(minPass, 75)))
  
  
  # shorten player name
  if(shortNames) {
    nodes$label <- soccerShortenName(nodes$label)
  }
  
  # if no title given, use team.name
  if(is.null(title)) {
    title <- unique(df$team.name)
  }
  
  
  # plot network
  soccerPitchBG(lengthPitch, widthPitch, 
              arrow = arrow[1], grass = grass,
              title = title, 
              subtitle = paste0(min(df$minute)+1, "' - ", max(df$minute)+1, "', ", minPass, "+ passes shown")) +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, size = n), col = edgeCol, alpha = edgeAlpha) +
    geom_point(data = nodes, aes(x, y, size = events), pch = 21, fill = fill, col = col) +
    ggrepel::geom_label_repel(data = nodes, aes(x, y, label = label), size = labelSize) +
    scale_size_identity() +
    guides(size = F) +
    annotate("text", 104, 1, label = paste0("Passes: ", pass_n, "\nCompleted: ", sprintf("%.1f", pass_pc), "%"), hjust = 1, vjust = 0, size = labelSize * 7/8)
  
}
