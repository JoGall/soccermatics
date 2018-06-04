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
#' @param dat = dataframe containing unnormalised x,y-coordinates named `x` and `y`
#' @param periodVar = name of variable containing period labels
#' @param periodToFlip = which period to flip
#' @param pitchLength,pitchWidth = length, width of pitch in metres
#' @return a dataframe
#' @examples
#' # to flip coordinates in 2nd half of a dataframe with 1st/2nd half identity labelled by variable named `period`
#' soccerFlipHoriz(df, "period", 2, 105, 68)
#' 
#' @export
soccerFlipHoriz <- function(dat, periodVar = "period", periodToFlip = 1, pitchLength = 105, pitchWidth = 68) {

  dat$x <- ifelse(dat[,periodVar] == periodToFlip, pitchLength - dat$x, dat$x)
  dat$y <- ifelse(dat[,periodVar] == periodToFlip, pitchWidth - dat$y, dat$y)
  
  return(dat)
}
