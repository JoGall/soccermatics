#' @import ggplot2
#' @import dplyr
#' @importFrom zoo na.approx
#' @importFrom plyr rbind.fill
#' @importFrom xts xts
NULL
#' Resample the frames per second of any tracking data using linear interpolation
#'
#' @description Downsample or upsample any tracking data containing x,y,t data using linear interpolation of x,y-coordinates (plus constant interpolation of all other variables in dataframe)
#' 
#' @param df a dataframe containing x,y-coordinates and time variable
#' @param r resampling rate in frames per second
#' @param x,y name of variables containing x,y-coordinates
#' @param t name of variable containing time data
#' @param id name of variable containing player identifier
#' @return a dataframe with interpolated rows added
#' @examples
#' data(tromso)
#' 
#' # resample tromso dataset from ~21 fps to 10 fps
#' soccerResample(tromso, r=10)
#' 
#' @export
soccerResample <- function(df, r = 10, x = "x", y = "y", t = "t", id = "id") {
  Index<-NULL
  
  # create new time index 
  time.index <- seq(min(df[,t]), max(df[,t]), by = as.difftime(1/r, units='secs'))
  
  # remove all rows that have duplicated timestamps
  df <- df %>% 
    group_by(!!sym(id)) %>% 
    filter(!(duplicated(!!sym(t)) | duplicated(!!sym(t), fromLast = TRUE))) %>% 
    ungroup()
  
  # resample and interpolate for each id
  ids <- as.numeric(as.vector(unique(df[[id]])))
  df_resampled <- lapply(ids, function(i) {
    #subset
    ss <- df[df[,id] == i,]
    
    # convert data to xts object
    ss.xts <- xts(ss[, names(ss) != t], ss[[t]])
    
    # join to time index
    ss.join <- merge(ss.xts, time.index, all=TRUE) %>% 
      ggplot2::fortify() %>% 
      rename_at(vars(Index),~"t")
    
    # linear interpolatation of x,y-coords with omission of leading / lagging NAs; constant interpolation of other variables
    ss.join %>% 
      mutate_at(vars(-one_of(t, x, y)), function(x) na.approx(x, method = "constant", na.rm=FALSE)) %>%
      mutate_at(vars(one_of(x, y)), function(x) na.approx(x, na.rm=FALSE)) %>% 
      filter(!!sym(t) %in% time.index)
    
  }) %>% 
    plyr::rbind.fill()
  
  # generate frame variable
  time.index2 <- data.frame(t = time.index, frame = 1:length(time.index))
  names(time.index2)[names(time.index2) == "t"] <- t
  df_resampled <- left_join(df_resampled, time.index2, by = t)
  
  return(df_resampled)
  
}

