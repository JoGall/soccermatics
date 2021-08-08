#' Sample StatsBomb event data containing the x,y-locations and identity of players involved in pass events, shot events, defensive actions, and more.
#'
#' Sample StatsBomb event data from the France vs. Argentina World Cup 2018 game on the 30th June 2018, made publicly available by StatsBomb \href{https://github.com/statsbomb/open-data}{here}. Data contains 145 variables in total, including x,y-coordinates (\code{location.x}, \code{location.y}). StatsBomb pitch dimensions are 120m long and 80m wide, meaning \code{lengthPitch} should be specified as \code{120} and \code{widthPitch} as \code{80}. Event data for all World Cup games (and other competitions) are accessible via the StatsBombR package available \href{https://github.com/statsbomb/StatsBombR}{here}.
#' @docType data
#' @name statsbomb
#' @format A dataframe containing 12000 frames of x,y-coordinates and timestamps from 11 players.
#' @source \href{http://home.ifi.uio.no/paalh/dataset/alfheim/}{ZXY Sport Tracking}
#' @references \href{https://github.com/statsbomb/open-data}{StatsBomb Open Data}
#' @usage data(statsbomb)
NULL
