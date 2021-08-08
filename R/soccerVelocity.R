#' @import dplyr
#' @importFrom magrittr "%>%"
NULL
#' Compute instantaneous distance, speed and direction from x,y-coordinates
#'
#' @description Compute instantaneous distance moved (in metres), speed (in metres per second), and direction (in radians) between subsequent frames in a dataframe of x,y-coordinates.
#' 
#' @param dat dataframe containing unnormalised x,y-coordinates \code{x} and \code{y}, time variable \code{'t'}, and player identifier \code{'id'}
#' @return a dataframe with columns \code{'dist'}, \code{'speed'}, and \code{'direction'} added
#' @examples
#' data(tromso)
#' 
#' # calculate distance, speed, and direction for \code{tromso} dataset
#' soccerVelocity(tromso)
#' 
#' @export
soccerVelocity <- function(dat) {
  delta_t<-x<-y<-velocity<-distance<-NULL
  
  dat %>%
    group_by(id) %>% 
    mutate(delta_t = c(NA, diff(t)),
           velocity = c(NA, diff(complex(real = x, imaginary = y))),
           direction = c(diff(Arg(velocity)) %% (2*pi), NA) - pi,
           distance = Mod(velocity),
           speed = distance / delta_t) %>% 
    select(-delta_t, -velocity) %>% 
    ungroup()
  
}