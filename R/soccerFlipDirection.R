#' @import ggplot2
#' @import dplyr
#' @importFrom ggforce geom_arc geom_circle
NULL
#' Flips x,y-coordinates horizontally in one half to account for changing sides at half-time
#'
#' @description Normalises direction of attack in both halves of both teams by 
#' flipping x,y-coordinates horizontally in either the first or second half; 
#' i.e. teams attack in the same direction all game despite changing sides at 
#' half-time.
#' 
#' @param df = dataframe containing unnormalised x,y-coordinates
#' @param lengthPitch,widthPitch = length, width of pitch in metres
#' @param teamToFlip = character, name of team to flip. If \code{NULL}, all x,y-coordinates in \code{df} will be flipped
#' @param periodToFlip = integer, period(s) to flip
#' @param team = character, name of variables containing x,y-coordinates
#' @param period = character, name of variable containing period labels
#' @param x,y = character, name of variables containing x,y-coordinates
#' @return a dataframe
#' @examples
#' library(dplyr)
#' 
#' # flip x,y-coords of France in both halves of statsbomb data
#' data(statsbomb)
#' statsbomb %>% 
#'   soccerFlipDirection(lengthPitch = 105, widthPitch = 68, teamToFlip = "France", team = "team.name", x = "location.x", y = "location.y")
#'   
#' # flip x,y-coords in 2nd half of Tromso, based on a dummy period variable
#' data(tromso)
#' tromso %>% 
#'   mutate(period = if_else(t > as.POSIXct("2013-11-07 21:14:00 GMT"), 1, 2)) %>% 
#'   soccerFlipDirection(lengthPitch = 105, widthPitch = 68, periodToFlip = 2)
#'   
#' @export
soccerFlipDirection <- function(df, lengthPitch = 105, widthPitch = 68, teamToFlip = NULL, periodToFlip = 1:2, team = "team", period = "period", x = "x", y = "y") {

  # flip all x,y-coordinates in df
  if(is.null(teamToFlip)) {
    df <- df %>%
      mutate(!!x := if_else(!!sym(period) %in% periodToFlip, lengthPitch - !!sym(x), !!sym(x)),
             !!y := if_else(!!sym(period) %in% periodToFlip, widthPitch - !!sym(y), !!sym(y)))
    
  # flip x,y-coords of only one team
  } else {
    df <- df %>%
      mutate(!!x := if_else(!!sym(team) == teamToFlip & !!sym(period) %in% periodToFlip, lengthPitch - !!sym(x), !!sym(x)),
             !!y := if_else(!!sym(team) == teamToFlip & !!sym(period) %in% periodToFlip, widthPitch - !!sym(y), !!sym(y)))
  }
  
  return(df)
}
