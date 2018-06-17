#' @include soccerPitchBG.R
NULL
#' Draw a map of all passes from StatsBomb data
#'
#' @description Draw a map of all passes from StatsBomb data. Compatability with non-StatsBomb data will be added soon.
#' 
#' @param lengthPitch,widthPitch length and width of pitch in metres
#' @param colGoal,colMiss colour of circles for scored and missed shots 
#' @param fillPitch pitch fill colour
#' @param colPitch pitch line colour
#' @param grass if \code{TRUE}, draws a more realistic looking pitch
#' @param lwd pitch line width
#' @param border size of border drawn around pitch perimeter (t,r,b,l)
#' @return a ggplot object
#' @examples
#' library(StatsBombR)
#' 
#' # get data
#' Matches <- FreeMatches(37)
#' d <- allinfo(Matches[1])
#' 
#' # transform all x,y-coordinates of StatsBomb data
#' d <- soccerTransformSB(d)
#' 
#' # shotmap of Manchester City WFC
#' d %>% 
#'   filter(type.name == "Pass",
#'   team.name == "Manchester City WFC") %>% 
#'   soccerPassmap(SB = TRUE)
#' 
#' @seealso \code{\link{soccerPitchBG}} for drawing a soccer pitch as foreground over an existing ggplot object
#' @export
soccerPassmap <- function(dat, lengthPitch = 105, widthPitch = 68, colComplete = "blue", colFail = "red", alpha = 0.8, fillPitch = "white", colPitch = "grey60", grass = FALSE, lwd = 0.5, border = c(4, 4, 4, 4), SB = FALSE) {
  
  dat <- dat %>% 
    mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), 1, 0)))
  
  soccerPitchBG(lengthPitch, widthPitch, fillPitch = fillPitch, colPitch = colPitch, grass = grass, lwd = lwd, SB = SB) +
    geom_segment(data = filter(dat, pass.outcome == 0), aes(x = location.x, xend = pass.end_location.x, y = location.y, yend = pass.end_location.y), col = colFail, alpha = alpha) +
    geom_point(data = filter(dat, pass.outcome == 0), aes(x = location.x, y = location.y), col = colFail, alpha = alpha) +
    geom_segment(data = filter(dat, pass.outcome == 1), aes(x = location.x, xend = pass.end_location.x, y = location.y, yend = pass.end_location.y), col = colComplete, alpha = alpha) +
    geom_point(data = filter(dat, pass.outcome == 1), aes(x = location.x, y = location.y), col = colComplete, alpha = alpha)
  
}