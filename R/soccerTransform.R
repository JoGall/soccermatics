#' @import dplyr
NULL
#' Normalises x,y-coordinates to metres units for use with soccermatics functions
#'
#' @description Normalises x,y-coordinates from between arbitrary limits to metre units bounded by [0 < x < pitchLength, 0 < y < pitchWidth]
#' 
#' @param dat dataframe containing arbitrary x,y-coordinates
#' @param xMin,xMax,yMin,yMax range of possible x,y-coordinates in the raw dataframe
#' @param pitchLength,pitchWidth length, width of pitch in metres
#' @param method = source of data, either \code{"opta"}, \code{"statsbomb"}, \code{"tracab"}, or \code{"manual"}
#' @param x,y variable names of x,y-coordinates. Not required when \code{method} other than \code{"manual"} is defined; defaults to \code{"x"} and \code{"y"} if manual.
#' @return a dataframe
#' 
#' @examples
#' # Three examples with true pitch dimensions (in metres):
#' lengthPitch <- 105
#' widthPitch <- 68
#' 
#' # Example 1. Opta ----------------------------------------------------------
#' 
#' # limits = [0 < x < 100, 0 < y < 100]
#' opta_df <- data.frame(team_id = as.factor(c(1, 1, 1, 2, 2)),
#'                       x = c(50.0, 41.2, 44.4, 78.6, 76.7),
#'                       y = c(50.0, 55.8, 47.5, 55.1, 45.5),
#'                       endx = c(42.9, 40.2, 78.0, 80.5, 72.4),
#'                       endy = c(57.6, 47.2, 55.6, 48.1, 26.3))
#' 
#' soccerTransform(opta_df, method = "opta")
#' 
#' 
#' # Example 2. StatsBomb -----------------------------------------------------
#' 
#' # limits = [0 < x < 120, 0 < y < 80]
#' 
#' soccerTransform(statsbomb, method = "statsbomb")
#' 
#' 
#' # Example 3. TRACAB --------------------------------------------------------
#' 
#' # limits = [-5250 < x < 5250, -3400 < y < 3400]
#' 
#' xMin <- -5250
#' xMax <- 5250
#' yMin <- -3400
#' yMax <- 3400
#' 
#' tracab_df <- data.frame(x = c(0,-452,-982,-1099,-1586,-2088,-2422,-2999,-3200,-3857),
#'                         y = c(0,150,300,550,820,915,750,620,400,264))
#' 
#' soccerTransform(tracab_df, -5250, 5250, -3400, 3400, method = "tracab")
#' 
#' 
#' # Example 4. Manual -----------------------------------------------------
#' 
#' # limits = [0 < x < 420, -136 < y < 136]
#'
#' my_df <- data.frame(team = as.factor(c(1, 1, 1, 2, 2)),
#'                     my_x = c(210, 173, 187, 330, 322),
#'                     my_y = c(0, 16, -7, 14, -12),
#'                     my_endx = c(180, 169, 328, 338, 304),
#'                     my_endy = c(21, -8, 15, -5, -65))
#'                     
#' soccerTransform(my_df, 0, 420, -136, 136, x = c("my_x", "my_endx"), y = c("my_y", "my_endy"))
#' 
#' @export
soccerTransform <- function(dat, xMin, xMax, yMin, yMax, lengthPitch = 105, widthPitch = 68, method = c("manual", "statsbomb", "opta"), x = "x", y = "y") {
  
  if(method[1] == "statsbomb") {
    
    xMin <- 1
    xMax <- 120
    yMin <- 1
    yMax <- 80
    
    dat <- dat %>% 
      mutate_at(vars(contains('.x')), funs((. - xMin) / diff(c(xMin, xMax)) * lengthPitch)) %>% 
      mutate_at(vars(contains('.y')), funs((. - yMin) / diff(c(yMin, yMax)) * widthPitch))
    
  } else if(method[1] == "opta") {
    
    xMin <- 0
    xMax <- 100
    yMin <- 0
    yMax <- 100
    
    dat$x <- (dat$x - xMin) / diff(c(xMin,xMax)) * lengthPitch
    dat$y <- (dat$y - yMin) / diff(c(yMin,yMax)) * widthPitch
    dat$endx <- (dat$endx - xMin) / diff(c(xMin,xMax)) * lengthPitch
    dat$endy <- (dat$endy - yMin) / diff(c(yMin,yMax)) * widthPitch
    
  } else if(method[1] == "tracab") {
    
    xMin <- -5250
    xMax <- 5250
    yMin <- -3400
    yMax <- 3400
    
    dat$x <- (dat$x - xMin) / diff(c(xMin,xMax)) * lengthPitch
    dat$y <- (dat$y - yMin) / diff(c(yMin,yMax)) * widthPitch
    
  } else {

    dat <- dat %>% 
      mutate_at(vars(!!!enquo(x)), funs((. - xMin) / diff(c(xMin,xMax)) * lengthPitch)) %>% 
      mutate_at(vars(!!!enquo(y)), funs((. - yMin) / diff(c(yMin,yMax)) * widthPitch))
    
  }
  
  return(dat)
  
}
