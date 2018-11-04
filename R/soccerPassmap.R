#' @include soccerPitchBG.R
#' @import ggplot2
#' @import dplyr
#' @importFrom ggrepel geom_text_repel
#' @importFrom forcats fct_explicit_na
NULL
#' Draw a passing network on a pitch from StatsBomb data
#' 
#' @description Draw an undirected passing network of completed passes on pitch from StatsBomb data. Nodes are scaled by number of successful passes; edge width is scaled by number of successful passes between each node pair. Only passes made until first substition shown (ability to specify custom minutes will be added soon). Total number of passes attempted and percentage of completed passes shown. Compatability with other (non-StatsBomb) shot data will be added soon.
#' 
#' @param df dataframe containing x,y-coordinates of player passes
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres
#' @param fill character, fill colour of nodes
#' @param col character, border colour of nodes
#' @param edge_col colour of edge lines
#' @param edge_alpha transparency of edge lines
#' @param edge_max_width maximum width of edge lines
#' @param grass if \code{TRUE}, uses a more realistic pitch
#' @param arrow optional, adds arrow showing team attack direction as right (\code{'r'}) or left (\code{'l'})
#' @param title optional, adds title to plot
#' @param x,y = name of variables containing x,y-coordinates
#' @param id character, the name of the column containing player identity. Defaults to \code{'id'}
#' @examples
#' # France vs. Argentina, minimum of three passes
#' data(statsbomb)
#' 
#' statsbomb %>% 
#'   filter(team.name == "France") %>% 
#'   soccerPassmap(x = "location.x", y = "location.y", id = "player.name",
#'                col = "blue", arrow = "r", minPass = 3, 
#'                title = "France (vs. Argentina, 2018-06-30")
#' @export
soccerPassmap <- function(df, lengthPitch = 105, widthPitch = 68, minPass = 3, minMinute = NULL, maxMinute = NULL, fill = "red", col = "black", edge_col = "black", edge_alpha = 0.6, edge_max_width = 100, label_size = 4, grass = FALSE, arrow = c("none", "r", "l"), x = "location.x", y = "location.y", id = "player.id", label = "player.name", shortNames = TRUE) {
  
  df$x <- df[,x]
  df$y <- df[,y]
  df$id <- df[,id]
  df$label <- df[,label]
  
  # minute of first substition
  if(is.null(maxMinute)) {
    maxMinute <- df %>% 
      group_by(id) %>% 
      dplyr::summarise(minute = min(minute)) %>% 
      na.omit %>% 
      filter(minute > 2) %>% 
      summarise(minute = min(minute)) %>% 
      .$minute
  }
  
  if(is.null(minMinute)) {
    minMinute <- 0
  }
  
  # filter minutes
  df <- df %>% 
    filter(minute >= minMinute & minute <= maxMinute)
  
  # filter data for starting XI until first substitute
  dd <- df %>% 
    filter(minute < first_sub_minute)
  
  # passing labels
  passes <- df %>% 
    filter(type.name == "Pass") %>% 
    group_by(pass.outcome.name) %>% 
    tally() %>% 
    filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown")) %>% 
    mutate(pass.outcome.name = forcats::fct_explicit_na(pass.outcome.name, "Complete"))
  pass_n <- sum(passes$n)
  pass_pc <- passes[passes$pass.outcome.name == "Complete",]$n / pass_n * 100
  
  # get nodes and edges for plotting in ggplot
  nodes <- dd %>% 
    filter(type.name == "Pass") %>% 
    group_by(id, label) %>% 
    dplyr::summarise(x = mean(x, na.rm=T), y = mean(y, na.rm=T), events = n()) %>% 
    na.omit() %>% 
    as.data.frame()
  
  edgelist <- dd %>% 
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
  edges <- edges %>% 
    filter(n >= minPass) %>%
    mutate(n = scales::rescale(n, c(1,edge_max_width), c(minPass, 75)))

  # shorten player name
  if(shortNames) {
    nodes$label <- soccerShortenName(nodes$label)
  }
  
  # plot network
  soccerPitchBG(lengthPitch, widthPitch, 
              arrow = arrow, grass = grass,
              title = unique(df$team.name), 
              subtitle = paste0(minMinute+1, "' - ", maxMinute, "', ", minPass, "+ passes shown")) +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, size = n), col = edge_col, alpha = edge_alpha) +
    geom_point(data = nodes, aes(x, y, size = events), pch = 21, fill = fill, col = col) +
    ggrepel::geom_label_repel(data = nodes, aes(x, y, label = label), size = label_size) +
    guides(size = F) +
    annotate("text", 104, 1, label = paste0("Passes: ", pass_n, "\nCompleted: ", sprintf("%.1f", pass_pc), "%"), hjust = 1, vjust = 0, size = label_size * 7/8)
}
