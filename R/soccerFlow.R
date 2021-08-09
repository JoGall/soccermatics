#' @include soccerHeatmap.R
#' @include soccerSpokes.R
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr "%>%"
NULL
#' Draw a flow field of passing direction on a soccer pitch
#' @description A flow field to show the mean angle and distance of passes in zones of the pitch
#' 
#' @param df dataframe of event data containing fields of start x,y-coordinates, pass distance, and pass angle
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param xBins,yBins integer, the number of horizontal (length-wise) and vertical (width-wise) bins the soccer pitch is to be divided up into; if \code{yBins} is NULL (default), it will take the value of \code{xBins}
#' @param x,y,angle,distance names of variables containing pass start x,y-coordinates, angle, and distance
#' @param col colour of arrows
#' @param lwd thickness of arrow segments
#' @param arrow adds team direction of play arrow as right (\code{'r'}) or left (\code{'l'}); \code{'none'} by default
#' @param title,subtitle adds title and subtitle to plot; NULL by default
#' @param theme palette of pitch background and lines, either \code{light} (default), \code{dark}, \code{grey}, or \code{grass}
#' @param plot base plot to add path layer to; NULL by default
#' @return a ggplot object of a heatmap on a soccer pitch
#' @examples
#' library(dplyr)
#' data(statsbomb)
#' 
#' # transform x,y-coords, filter only France pass events,
#' # draw flow field showing mean angle, distance of passes per pitch zone
#' statsbomb %>% 
#'   soccerTransform(method = 'statsbomb') %>% 
#'   filter(team.name == "France" & type.name == "Pass") %>% 
#'   soccerFlow(xBins=7, yBins=5,
#'              x="location.x", y="location.y", angle="pass.angle", distance="pass.length")
#'                    
#' # transform x,y-coords, standarise column names,
#' # filter only France pass events
#' my_df <- statsbomb %>% 
#'   soccerTransform(method = 'statsbomb') %>% 
#'   soccerStandardiseCols(method = 'statsbomb') %>%
#'   filter(team_name == "France" & event_name == "Pass")
#'   
#' # overlay flow field onto heatmap showing proportion of team passes per pitch zone
#' soccerHeatmap(my_df, xBins=7, yBins=5) %>%
#'   soccerFlow(my_df, xBins=7, yBins=5, plot = .)
#'            
#' @seealso \code{\link{soccerHeatmap}} for drawing a heatmap of player position, or \code{\link{soccerSpokes}} for drawing spokes to show all directions in each area of the pitch.
#' @export
soccerFlow <- function(df, lengthPitch = 105, widthPitch = 68, xBins = 5, yBins = NULL, x = "x", y = "y", angle = "angle", distance = "distance", col = "black", lwd = 0.5, arrow = c("none", "r", "l"), title = NULL, subtitle = NULL, theme = c("light", "dark", "grey", "grass"), plot = NULL) {
  x.bin<-y.bin<-bin<-x.bin.coord<-y.bin.coord<-angle.mean<-radius.mean<-NULL
  
  # check value for vertical bins and match to horizontal bins if NULL
  if(is.null(yBins)) yBins <- xBins

  # adjust range and n bins
  x.range <- seq(0, lengthPitch, length.out = xBins+1)
  y.range <- seq(0, widthPitch, length.out = yBins+1)
  
  # bin plot values
  x.bin.coords <- data.frame(x.bin = 1:xBins, 
                             x.bin.coord = (x.range + (lengthPitch / (xBins) / 2))[1:xBins])
  y.bin.coords <- data.frame(y.bin = 1:yBins,
                             y.bin.coord = (y.range + (widthPitch / (yBins) / 2))[1:yBins])

  # bin data
  suppressWarnings( #suppress warnings about empty bins
    df <- df %>%
      rowwise() %>%
      mutate(x.bin = max(which(!!sym(x) > x.range)),
             y.bin = max(which(!!sym(y) > y.range)),
             bin = paste(x.bin, y.bin, sep = "_")) %>%
      ungroup() %>% 
      filter(is.finite(x.bin) & is.finite(y.bin))
  )
  
  # summarise angle for each bin
  df <- df %>%
    group_by(bin) %>%
    mutate(x.mean = mean(!!sym(x), na.rm=T),
           y.mean = mean(!!sym(y), na.rm=T),
           angle.mean = mean(!!sym(angle), na.rm=T),
           radius.mean = mean(!!sym(distance), na.rm=T)) %>%
    ungroup()
  
  # add bin centre x,y-coords for plotting 
  df <- left_join(df, x.bin.coords, by = "x.bin")
  df <- left_join(df, y.bin.coords, by = "y.bin")
  
  # scale radius
  df$radius.mean <- scales::rescale(df$radius.mean, c(2, lengthPitch / xBins))
  
  if(missing(plot)) {
    soccerPitch(lengthPitch = lengthPitch, widthPitch = widthPitch, theme = theme) +
      geom_spoke(data = df, aes(x = x.bin.coord, y = y.bin.coord,
                                angle = angle.mean, radius = radius.mean),
                 size = lwd, col = col, arrow=arrow(length = unit(0.2,"cm"))) +
      guides(fill="none")
  } else {
    plot +
      geom_spoke(data = df, aes(x = x.bin.coord, y = y.bin.coord,
                                angle = angle.mean, radius = radius.mean),
                 size = lwd, col = col, arrow=arrow(length = unit(0.2,"cm"))) +
      guides(fill="none")
  }
}
