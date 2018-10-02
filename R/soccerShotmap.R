#' @include soccerPitch.R
NULL
#' Draw a shotmap on a half pitch from StatsBomb data
#'
#' @description Draw a shotmap on a half pitch from StatsBomb data. Compatability with other (non-StatsBomb) shot data will be added soon.
#' 
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param colGoal,colMiss colour of points representing scored and missed shots
#' @param alpha transparency of points
#' @param legend boolean, include legend or not
#' @param fillPitch,colPitch pitch background and line colour
#' @param grass if \code{TRUE}, uses a more realistic pitch
#' @param lwd numeric, pitch line width
#' @return a ggplot object
#' @examples
#' data(statsbomb)
#' 
#' # shot map of France vs. Argentina (2018-06-30)
#' statsbomb %>% 
#'   filter(team.name == "France") %>% 
#'   soccerShotmap()
#' 
#' @export
soccerShotmap <- function(dat, lengthPitch = 105, widthPitch = 68, colGoal = "skyblue", colMiss = "grey60", alpha = 0.8, legend = FALSE, theme = c("light", "dark", "grass"), lwd = 0.5) {
  
  if(theme[1] == "dark") {
    fillPitch <- "#1C1F26"
    colPitch <- "white"
    grass <- FALSE
  } else if(theme[1] == "light") {
    fillPitch <- "white"
    colPitch <- "grey60"
    grass <- FALSE
  } else if(theme[1] == "grass") {
    grass <- TRUE
  }
  
  dat <- dat %>%
    filter(type.name == "Shot") %>% 
    mutate(shot.outcome = as.factor(if_else(shot.outcome.name == "Goal", 1, 0)))
  
  p <- soccerPitch(lengthPitch, widthPitch, fillPitch = fillPitch, colPitch = colPitch, grass = grass, lwd = lwd) +
    geom_point(data = filter(dat, shot.outcome == 0), aes(x = location.x, y = location.y, size = shot.statsbomb_xg, colour = "Miss"), alpha = alpha) +
    geom_point(data = filter(dat, shot.outcome == 1), aes(x = location.x, y = location.y, size = shot.statsbomb_xg, colour = "Goal"), alpha = alpha) +
    coord_flip(xlim=c(lengthPitch/2 - 0.05, lengthPitch+5), expand=0) +
    theme(aspect.ratio = widthPitch / lengthPitch) +
    scale_colour_manual(name = "Outcome", breaks = c("Goal", "Miss"), values = c(colGoal, colMiss)) +
    scale_size_continuous(name = "xG")
  
  if(legend) {
    p <- p + 
      guides(colour = guide_legend(override.aes = list(size=6)))
  } else {
    p <- p + 
      guides(size = FALSE, colour = FALSE)
  }
  
  p
  
}
