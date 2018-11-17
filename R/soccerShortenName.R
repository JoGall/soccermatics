#' Extract player surname
#' @description Helper function to extract last name (including common nobiliary particles) from full player names
#' 
#' @param names vector of strings containing full player name
#' @examples
#' attach(statsbomb)
#' statsbomb$name <- soccerShortenName(statsbomb$player.name)
#' 
#' @export
soccerShortenName <- function(names) {
  sub(":", " ", sub(".* ", "", sub(" (di|De|de|El|el|Da|da|Dos|dos|Van|van|Von|von|Le|le|La|la|N') ", " \\1:", names)))
}
