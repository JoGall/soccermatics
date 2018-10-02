#' @import ggplot2
#' @importFrom ggforce geom_arc geom_circle
NULL
#' Flips x,y-coordinates horizontally in one half to account for changing sides at half-time
#'
#' @description Normalises direction of attack in both halves of both teams by 
#' flipping x,y-coordinates horizontally in either the first or second half; 
#' i.e. teams attack in the same direction all game despite changing sides at 
#' half-time.
#' 
#' @param dat = dataframe containing unnormalised x,y-coordinates
#' @param pitchLength,pitchWidth = length, width of pitch in metres
#' @param period = name of variable containing period labels
#' @param periodToFlip = identity of period to flip
#' @param x,y = name of variables containing x,y-coordinates
#' @return a dataframe
#' @examples
#' # fake period data for tromso dataset, and flip direction of '2nd half'
#' tromso %>% 
#'   mutate(period = if_else(t > as.POSIXct("2013-11-07 21:14:00 GMT"), 1, 2))
#'   soccerFlipDirection(pitchLength = 120, pitchWidth = 80, periodToFlip = 2)
#' 
#' @export
soccerFlipDirection <- function(dat, pitchLength = 105, pitchWidth = 68, period = "period", periodToFlip = 1, x = "x", y = "y") {

  dat[,x] <- ifelse(dat[,period] == periodToFlip, pitchLength - dat[,x], dat[,x])
  dat[,y] <- ifelse(dat[,period] == periodToFlip, pitchWidth - dat[,y], dat[,y])
  
  return(dat)
}
