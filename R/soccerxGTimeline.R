 q#' @include soccerShortenName.R
#' @import ggplot2
NULL
#' Draw a timeline showing cumulative expected goals (xG) over the course of a match using StatsBomb data.
#'
#' @description Draw a timeline showing cumulative expected goals (xG, excluding penalties and own goals) by two teams over the course of a match, as well as plotting the scoreline and goalscorer at goal events. Currently only works with StatsBomb data but compatability with other (non-StatsBomb) shot data will be added soon.
#' 
#' @param dat a dataframe containing StatsBomb data from one full match
#' @param homeCol,awayCol colours of the home and away team, respectively
#' @param labels include scoreline and goalscorer labels for goals
#' @param y_buffer vertical space to add at the top of the y-axis (a quick and dirty way to ensure text annotations are not cropped).
#' @return a ggplot object
#' @examples
#' data(statsbomb)
#' 
#' # xG timeline of France vs. Argentina
#' statsbomb %>%
#'   soccerXGTimeline(homeCol = "blue", awayCol = "lightblue", y_buffer = 0.4)
#' 
#' @export
soccerxGTimeline <- function(dat, homeCol = "red", awayCol = "blue", labels = TRUE, y_buffer = 0.3) {
  
  # preprocess data
  dat <- dat %>% 
    mutate(t = minute * 60 + second) %>% 
    mutate(type = if_else(type.name == "Own Goal For", "OG",
                    if_else(shot.type.name == "Penalty", "Pen", 
                      if_else(type.name == "Shot", "Open", "NA")))) %>% 
    mutate(outcome = if_else(type == "OG", 1,
                       if_else(shot.outcome.name == "Goal", 1, 0)))
    
  # set variable names
  home_team <- dat$team.name[1]
  away_team <- dat[dat$team.name != home_team,]$team.name %>% unique
  ht_t <- dat[dat$type.name == "Half End",]$t[1] #HT in seconds
  et_first <- ht_t - (45 * 60) #1H stoppage time
  dat[dat$period == 2,]$t <- dat[dat$period == 2,]$t + et_first #add 1H stoppage time to 2H
  ft_t <- dat[dat$type.name == "Half End",]$t[4] # FT in seconds
  
  # compute cumulative xG (non-penalty, non-OG goals only)
  shots <- dat %>% 
    filter(!is.na(type)) %>% 
    mutate(shot.statsbomb_xg = if_else(type %in% c("Pen", "OG"), 0, shot.statsbomb_xg)) %>% 
    group_by(team.name) %>%
    mutate(xg = cumsum(shot.statsbomb_xg)) %>% 
    ungroup() %>% 
    select(team.name, xg, t, id)
  # dummy first row at KO for plotting
  shots_start <- shots %>% 
    group_by(team.name) %>% 
    summarise(xg = 0,
              t = 0,
              id = NA)
  # dummy final row at FT for plotting
  shots_end <- shots %>% 
    group_by(team.name) %>% 
    summarise(xg = max(xg),
              t = ft_t,
              id = NA)
  # join
  shots <- rbind(shots, shots_start, shots_end)
  
  # get goals
  goals <- dat %>% 
    filter(outcome == 1) %>% 
    select(id, team.name, player.name, t, type)
  
  # join cumulative xG
  goals <- left_join(goals, 
                     shots %>% select(id, xg),
                     by = "id")
  
  # make labels
  # get scoreline
  goals <- goals %>% 
    mutate(hg = if_else(team.name == home_team, 1, 0),
           ag = if_else(team.name != home_team, 1, 0)) %>%
    mutate(hg = cumsum(hg),
           ag = cumsum(ag))
  
  # add parentheses for pen or OG
  goals <- goals %>% 
    mutate(name = soccermatics::soccerShortenName(player.name)) %>%
    mutate(label = paste0(hg, "-", ag, " ", name)) %>% 
    mutate(label = if_else(type == "Pen", paste0(label, " (P)"),
                   if_else(type == "OG", paste0(label, " (OG)"),
                   label)))
  
  # plot
  y_lim <- ifelse(labels, max(shots$xg) + y_buffer, max(shots$xg) + 0.2)
  
  title_a <- paste0(home_team, " ", max(goals$hg), " (", sprintf("%.1f", max(shots[shots$team.name == home_team,]$xg), 1), ")")
  title_b <- paste0(away_team, " ", max(goals$ag), " (", sprintf("%.1f", max(shots[shots$team.name == away_team,]$xg), 1), ")")
  
  shots$team.name <- factor(shots$team.name, levels = c(home_team, away_team))
  goals$team.name <- factor(goals$team.name, levels = c(home_team, away_team))
  
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
    guides(col = FALSE) +
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
