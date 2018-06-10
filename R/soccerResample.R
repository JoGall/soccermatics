#' @import ggplot2
#' @import dplyr
#' @import xts
#' @import zoo
NULL
#' Resample the frequency of x,y,t- time series with linear interpolation of x,y-coordinates.
#'
#' @description Downsample or upsample dataframe containing x,y-coordinates and a time variable `t` with linear interpolation of x,y-coordinates and constant interpolation of all other variables.
#' 
#' @param dat = dataframe containing unnormalised x,y-coordinates named `x` and `y` and a time variable named `t`
#' @param r resampling rate in frames per second
#' @return a dataframe
#' @examples
#' # resample tromso dataset from ~21 fps to 10 fps
#' soccerResample(tromso)
#' @export
soccerResample <- function(dat, r = 10) {
  
  # create new time index 
  time.index <- seq(min(dat$t), max(dat$t), by = as.difftime(1/resample_fps, units='secs'))
  
  # remove ALL rows that have duplicated timestamps
  dat <- dat %>% 
    group_by(id) %>% 
    filter(!(duplicated(t) | duplicated(t, fromLast = TRUE))) %>% 
    ungroup()
  
  # resample and interpolate for each id
  dat.proc <- lapply(unique(dat$id), function(x) {
    
    #subset
    ss <- dat[dat$id == x,]
    
    # convert data to xts object
    ss.xts <- xts(ss %>% select(-t),
                  ss$t)
    
    # join to time index
    ss.join <- merge(ss.xts, time.index, all=T) %>% 
      ggplot2::fortify() %>% 
      rename_at(vars(Index),~"t")
    
    # linear interpolatation of x,y,z with omission of leading / lagging NAs; constant interpolation of other variables
    ss.join %>% 
      mutate_at(vars(-one_of("t", "x", "y", "z")), function(x) na.approx(x, method = "constant", na.rm=F)) %>%
      mutate_at(vars(one_of("x", "y", "z")), function(x) na.approx(x, na.rm=F)) %>% 
      filter(t %in% time.index)
    
  }) %>% 
    plyr::rbind.fill()
  
  # generate frame variable
  time.index2 <- data.frame(t = time.index, frame = 1:length(time.index))
  dat.proc <- left_join(dat.proc, time.index2, by = "t")
  
  return(dat.proc)
  
}
