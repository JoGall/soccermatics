#' @include soccerShortenName.R
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom tidyr replace_na
NULL
#' Draw a timeline showing cumulative expected goals (xG) over the course of a match using StatsBomb data.
#'
#' @description Draw a timeline showing cumulative expected goals (xG, excluding penalties and own goals) by two teams over the course of a match, as well as plotting the scoreline and goalscorer at goal events. Currently only works with StatsBomb data but compatability with other (non-StatsBomb) shot data will be added soon.
#' 
#' @param df a dataframe containing StatsBomb data from one full match
#' @param homeCol,awayCol colours of the home and away team, respectively
#' @param adj adjust xG using conditional probability to account for multiple shots per possession
#' @param labels include scoreline and goalscorer labels for goals
#' @param y_buffer vertical space to add at the top of the y-axis (a quick and dirty way to ensure text annotations are not cropped).
#' @return a ggplot object
#' @examples
#' library(dplyr)
#' data(statsbomb)
#' 
#' # xG timeline of France vs. Argentina
#' # w/ goalscorer labels, adjusted xG data
#' statsbomb %>%
#'   soccerxGTimeline(homeCol = "blue", awayCol = "lightblue", y_buffer = 0.4)
#' 
#' # no goalscorer labels, raw xG data
#' statsbomb %>%
#'   soccerxGTimeline(homeCol = "blue", awayCol = "lightblue", adj = FALSE)
#' 
#' @export
soccerxGTimeline <- function(df, homeCol = "red", awayCol = "blue", adj = TRUE, labels = TRUE, y_buffer = 0.3) {
  minute<-second<-type.name<-shot.type.name<-type<-shot.outcome.name<-shot.statsbomb_xg<-team.name<-possession<-xg_total<-xg_adj<-outcome<-player.name<-hg<-ag<-name<-label<-NULL
  
  # ensure input is dataframe
  df <- as.data.frame(df)
  
  # preprocess data
  df <- df %>% 
    mutate(t = minute * 60 + second) %>% 
    mutate(type = if_else(type.name == "Own Goal For", "OG",
                          if_else(shot.type.name == "Penalty", "Pen", 
                                  if_else(type.name == "Shot", "Open", "NA")))) %>% 
    mutate(outcome = if_else(type == "OG", 1,
                             if_else(shot.outcome.name == "Goal", 1, 0)))
  
  # set variable names
  home_team <- df$team.name[1]
  away_team <- df[df$team.name != home_team,]$team.name %>% unique
  ht_t <- df[df$type.name == "Half End",]$t[1] #HT in seconds
  et_first <- ht_t - (45 * 60) #1H stoppage time
  df[df$period == 2,]$t <- df[df$period == 2,]$t + et_first #add 1H stoppage time to 2H
  ft_t <- df[df$type.name == "Half End",]$t[4] # FT in seconds
  
  # shot types
  df <- df %>% 
    filter(!is.na(type)) %>% 
    mutate(xg = shot.statsbomb_xg,
           xg = if_else(type %in% c("Pen", "OG"), 0, xg))
  
  # adjust xG using conditional probability when there are multiple shots in a single possession
  if(adj) {
    xg <- df %>%
      filter(type == "Open") %>%
      group_by(team.name, possession) %>%
      mutate(xg_total = (1 - prod(1 - xg))) %>%
      mutate(xg_adj = xg_total * (xg / sum(xg))) %>%
      ungroup() %>%
      select(id, xg_adj)
      
    df <- left_join(df, xg, by = "id") %>% 
      mutate(xg_adj = replace_na(xg_adj, 0)) %>%
      select(-xg) %>% 
      rename(xg = xg_adj)
  }
  
  # compute cumulative xG (non-penalty, non-OG goals only)
  shots <- df %>% 
    group_by(team.name) %>%
    mutate(xg = cumsum(xg)) %>% 
    ungroup() %>% 
    select(team.name, xg, t, id)
  # dummy first row at KO for start of geom_line
  shots_start <- shots %>% 
    group_by(team.name) %>% 
    summarise(xg = 0,
              t = 0,
              id = NA)
  # dummy final row at FT for end of geom_line
  shots_end <- shots %>% 
    group_by(team.name) %>% 
    summarise(xg = max(xg),
              t = ft_t,
              id = NA)
  # join shots data
  shots <- rbind(shots, shots_start, shots_end)
  
  # get goals
  goals <- df %>% 
    filter(outcome == 1) %>% 
    select(id, team.name, player.name, t, type)
  
  # join cumulative xG
  goals <- left_join(goals, 
                     shots %>% select(id, xg),
                     by = "id")
  
  
  # set plotting options and labels
  # score and goalscorer labels
  goals <- goals %>% 
    mutate(hg = if_else(team.name == home_team, 1, 0),
           ag = if_else(team.name != home_team, 1, 0)) %>%
    mutate(hg = cumsum(hg),
           ag = cumsum(ag)) %>% 
    mutate(name = soccermatics::soccerShortenName(player.name)) %>%
    mutate(label = paste0(hg, "-", ag, " ", name)) %>% 
    mutate(label = if_else(type == "Pen", paste0(label, " (P)"),
                   if_else(type == "OG", paste0(label, " (OG)"),
                   label)))

  y_lim <- ifelse(labels, max(shots$xg) + y_buffer, max(shots$xg) + 0.2)
  title_a <- paste0(home_team, " ", max(goals$hg), " (", sprintf("%.1f", max(shots[shots$team.name == home_team,]$xg)), ")")
  title_b <- paste0(away_team, " ", max(goals$ag), " (", sprintf("%.1f", max(shots[shots$team.name == away_team,]$xg)), ")")
  
  shots$team.name <- factor(shots$team.name, levels = c(home_team, away_team))
  goals$team.name <- factor(goals$team.name, levels = c(home_team, away_team))
  
  # plot
  p <- ggplot() +
    geom_vline(xintercept = ht_t, linetype = "longdash", col = "grey70") +
    geom_vline(xintercept = ft_t, linetype = "longdash", col = "grey70") +
    annotate("text", ht_t - 30, y_lim - 0.02, label = "HT", hjust = 1, vjust = 1, col = "grey70") +
    annotate("text", ft_t - 30, y_lim - 0.02, label = "FT", hjust = 1, vjust = 1, col = "grey70") +
    geom_step(data = shots, aes(t, xg, group = team.name, colour = team.name), lwd = 1) +
    geom_point(data = goals, aes(t, xg, group = team.name, colour = team.name), size = 3) +
    scale_y_continuous(limits = c(0, y_lim), expand = c(0,0)) +
    scale_x_continuous(breaks = c(seq(0, 45*60, 15*60), ht_t + seq(0, 45*60, 15*60)), labels = c("0'","15'","30'","","45'","60'","75'","90'"), limits = c(0, ft_t+60), expand = c(0, 0)) +
    scale_color_manual(breaks = c(home_team, away_team), values = c(homeCol, awayCol)) +
    guides(col="none") +
    labs(title = title_a,
         subtitle = title_b,
         y = "xG") +
    theme_bw(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 16, face = 'bold', colour = homeCol),
          plot.subtitle = element_text(size = 16, face = 'bold', colour = awayCol))
  
  if(labels) {
    p <- p +
      geom_text(data = goals, aes(t, xg + (y_lim/30), group = team.name, colour = team.name, label = label), angle = 90, hjust = 0) 
  }
  
  return(p)
  
}
