#' @include soccerPitchBG.R
#' @import ggplot2
#' @import dplyr
#' @importFrom ggrepel geom_text_repel
NULL
#' Draw a passing network on a pitch from StatsBomb data
#' 
#' @description Draw an undirected passing network of completed passes on pitch from StatsBomb data. Nodes are scaled by number of successful passes; edge width is scaled by number of successful passes between each node pair. Only passes made until first substition shown (ability to specify custom minutes will be added soon). Total number of passes attempted and percentage of completed passes shown. Compatability with other (non-StatsBomb) shot data will be added soon.
#' 
#' @param df dataframe containing x,y-coordinates of player passes
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres
#' @param fill character, fill colour of nodes
#' @param col character, border colour of nodes
#' @param line_col colour of edge lines
#' @param line_alpha transparency of edge lines
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
#'  soccerPassmap(x = "location.x", y = "location.y", id = "player.name",
#'                col = "blue", arrow = "r", minPass = 3, 
#'                title = "France (vs. Argentina, 2018-06-30")
#' @export
soccerPassmap <- function(df, lengthPitch = 105, widthPitch = 68, minPass = 3, fill = "red", col = "black", line_col = "black", line_alpha = 0.6, grass = FALSE, arrow = c("none", "r", "l"), title = NULL, x = "x", y = "y", id = "id") {
  
  df$x <- df[,x]
  df$y <- df[,y]
  df$id <- df[,id]
  
  # minute of first substition
  first_sub_minute <- df %>% 
    group_by(player.name) %>% 
    dplyr::summarise(minute = min(minute)) %>% 
    na.omit %>% filter(minute > 2) %>% 
    summarise(minute = min(minute)) %>% 
    .$minute
  
  # filter data for starting XI until first substitute
  dd <- df %>% 
    filter(minute < first_sub_minute)
  
  # passing label
  passes <- df %>% 
    filter(type.name == "Pass") %>% 
    group_by(pass.outcome.name) %>% 
    tally() %>% 
    filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown")) %>% 
    mutate(pass.outcome.name = forcats::fct_explicit_na(pass.outcome.name, "Complete"))
  pass_n <- sum(passes$n)
  pass_pc <- passes[passes$pass.outcome.name == "Complete",]$n / pass_n * 100
  pass_label <- paste0("Passes: ", pass_n, " (", sprintf("%.1f", pass_pc), "%)")
  
  # get nodes and edges for plotting in ggplot
  nodes <- dd %>% 
    filter(type.name == "Pass") %>% 
    group_by(player.name) %>% 
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
            nodes %>% select(player.name, x, y),
            by = c("from" = "player.name"))
  
  edges <- left_join(edges, 
            nodes %>% select(player.name, xend = x, yend = y),
            by = c("to" = "player.name"))
  
  edges <- edges %>% 
    group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
    dplyr::summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])
  
  # filter minimum number of passes and rescale line width
  edges <- edges %>% 
    filter(n >= minPass) %>%
    mutate(n = scales::rescale(n, c(1,100), c(minPass, 75)))
  
  # shorten player name
  nodes$name <- soccerShortenName(nodes$player.name)
  
  # plot network
  soccerPitchBG(lengthPitch, widthPitch, 
              arrow = arrow, grass = grass,
              title = title, 
              subtitle = paste0("1' - ", first_sub_minute, "'")) +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, size = n), col = line_col, alpha = line_alpha) +
    geom_point(data = nodes, aes(x, y, size = events), pch = 21, fill = fill, col = col) +
    ggrepel::geom_label_repel(data = nodes, aes(x, y, label = name)) +
    guides(size = F) +
    annotate("text", 0, widthPitch - 2, label = pass_label, hjust = 0)
}
