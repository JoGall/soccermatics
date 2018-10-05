#' Extract shortened player names
#' @description Helper function to extract last name from full player names
#' 
#' @param names vector of strings containing full player names
#' @examples
#' attach(statsbomb)
#' statsbomb$name <- soccerShortenName(statsbomb$player.name)
#' 
#' @export
soccerShortenName <- function(names) {
  sub(":", " ", sub(".* ", "", sub(" (di|De|de|El|el|Da|da|Dos|dos|Van|van|N') ", " \\1:", names)))
}
