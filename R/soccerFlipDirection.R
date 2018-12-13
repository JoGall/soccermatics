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
#' @param period = name of variable containing period labels
#' @param periodToFlip = identity of period to flip
#' @param x,y = name of variables containing x,y-coordinates
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
soccerFlipDirection <- function(df, lengthPitch = 105, widthPitch = 68, teamToFlip = NULL, periodToFlip = 1:2, period = "period", team = "team", x = "x", y = "y") {

  if(is.null(teamToFlip)) {
    df[,x] <- ifelse(df[,period] %in% periodToFlip, lengthPitch - df[,x], df[,x])
    df[,y] <- ifelse(df[,period] %in% periodToFlip, widthPitch - df[,y], df[,y])    
  } else {
    df[,x] <- ifelse(df[,team] == teamToFlip & df[,period] %in% periodToFlip, lengthPitch - df[,x], df[,x])
    df[,y] <- ifelse(df[,team] == teamToFlip & df[,period] %in% periodToFlip, widthPitch - df[,y], df[,y])
  }
  
  return(df)
}
