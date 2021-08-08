#' Extract player surname
#' @description Helper function to extract last name (including common nobiliary particles) from full player names
#' 
#' @param names vector of strings containing full player name
#' @examples
#' data(statsbomb)
#' statsbomb$name <- soccerShortenName(statsbomb$player.name)
#' 
#' @export
soccerShortenName <- function(names) {
  
  # collapse special cases with >1 prefix separated by a space
  names <- sub("van der", "_vander_", names)
  names <- sub("van de", "_vande_", names)
  
  # define prefixes
  prefixes <- " (Di|di|De|de|El|el|Da|da|Dos|dos|Van|van|Von|von|Le|le|La|la|N'|_vander_|_vande_) "
  
  # remove all of string before last name and any defined prefixes
  names <- sub(":", " ", sub(".* ", "", sub(prefixes, " \\1:", names)))
  
  # expand special cases
  names <- sub("_vander_", "van der", names)
  names <- sub("_vande_", "van de", names)
  
  return(names)
  
}
