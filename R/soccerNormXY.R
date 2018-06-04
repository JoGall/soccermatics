#' @include soccerPath.R
#' @import ggplot2
#' @importFrom ggforce geom_arc geom_circle
NULL
#' Normalises x,y-coordinates to metres units for use with soccermatics functions
#'
#' @description Normalises x,y-coordinates from between any arbitrary bounds to metre units bounded by [0 < x < pitchLength, 0 < y < pitchWidth]
#' 
#' @param dat = dataframe containing unnormalised x,y-coordinates named `x` and `y`
#' @param xMin,xMax,yMin,yMax = range of x,y-coordinates possible in the raw dataset
#' @param pitchLength,pitchWidth = length, width of pitch in metres
#' @return a dataframe
#' @examples
#' # Three examples with true pitch dimesions (in metres):
#' lengthPitch <- 101
#' widthPitch <- 68
#' 
#' # Example 1. Opta-style -------------------------------------------------------
#' # limits = [0 < x < 100, 0 < y < 100]
#' # centre of pitch = [50,50]
#' 
#' df <- data.frame(t = 1:12,
#'                  x = c(50,55,61,66,62,58,51,44,45,42,41,32),
#'                  y = c(50,48,47,40,42,45,49,51,59,75,88,100))
#' 
#' df <- soccerNormXY(df, 0, 100, 0, 100, lengthPitch, widthPitch)
#' 
#' soccerPath(df, lengthPitch = lengthPitch, widthPitch = widthPitch)
#' 
#' 
#' # Example 2. StrataBet-style --------------------------------------------------
#' # limits = [0 < x < 420, -136 < y < 136]
#' # centre of pitch = [210,0]
#' 
#' df <- data.frame(t = 1:12,
#'                  x = c(210,222,201,192,178,170,143,122,104,91,75,60),
#'                  y = c(0,-5,-20,-12,-8,-2,4,8,13,20,30,45))
#' 
#' df <- soccerNormXY(df, 0, 420, -136, 136, lengthPitch, widthPitch)
#' 
#' soccerPath(df, lengthPitch = lengthPitch, widthPitch = widthPitch)
#' 
#' 
#' # Example 3. Other ------------------------------------------------------------
#' # limits = [-5250 < x < 5250, -3400 < y < 3400]
#' # centre of pitch = [0,0]
#' 
#' xMin <- -5250
#' xMax <- 5250
#' yMin <- -3400
#' yMax <- 3400
#' 
#' df <- data.frame(x = c(0,-452,-982,-1099,-1586,-2088,-2422,-2999,-3200,-3857),
#'                  y = c(0,150,300,550,820,915,750,620,400,264))
#' 
#' df <- soccerNormXY(df, -5250, 5250, -3400, 3400, lengthPitch, widthPitch)
#' 
#' soccerPath(df, lengthPitch = lengthPitch, widthPitch = widthPitch)
#' 
#' @export
soccerNormXY <- function(dat, xMin, xMax, yMin, yMax, pitchLength, pitchWidth) {
  
  dat$x <- (dat$x - xMin) / diff(c(xMin,xMax)) * pitchLength
  dat$y <- (dat$y - yMin) / diff(c(yMin,yMax)) * pitchWidth
  
  return(dat)
}