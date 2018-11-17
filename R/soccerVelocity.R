#' @import dplyr
NULL
#' Compute instantaneous distance, speed and direction from x,y-coordinates
#'
#' @description Compute instantaneous distance moved (in metres), speed (in metres per second), and direction (in radians) between subsequent frames in a dataframe of x,y-coordinates.
#' 
#' @param dat dataframe containing unnormalised x,y-coordinates `x` and `y`, time variable `t`, and identifier `id`
#' @return a dataframe
#' @examples
#' # calculate distance, speed, and direction for tromso dataset
#' soccerVelocity(tromso)
#' 
#' @export
soccerVelocity <- function(dat) {
  
  dat %>%
    group_by(id) %>% 
    mutate(diff = c(NA, diff(t)),
           v = c(NA, diff(complex(real = x, imaginary = y))),
           direction = c(diff(Arg(v)) %% (2*pi), NA) - pi,
           dist = Mod(v),
           speed = dist / diff) %>% 
    select(-diff, -v)
  
}