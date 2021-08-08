#' @include soccerHeatmap.R
#' @include soccerFlow.R
#' @import ggplot2
#' @import dplyr
NULL
#' Draw spokes of passing direction on a soccer pitch
#' 
#' @description Multiple arrows to show the distribution of pass angle and distance in zones of the pitch; similar to a radar plot but grouped by pitch location rather than player
#' 
#' @param df a dataframe of event data containing fields of start x,y-coordinates, pass distance, and pass angle
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres
#' @param xBins,yBins integer, the number of horizontal (length-wise) and vertical (width-wise) bins the soccer pitch is to be divided up into; if \code{yBins} is NULL (default), it will take the value of \code{xBins}
#' @param angleBins integer, the number of arrows to draw in each zone of the pitch; for example, a value of 4 clusters has direction vectors up, down, left, and right
#' @param x,y,angle names of variables containing pass start x,y-coordinates and angle
#' @param minLength numeric, ratio between size of shortest arrow and longest arrow depending on number of events
#' @param minAlpha,minWidth numeric, minimum alpha and line width of arrows drawn
#' @param col colour of arrows
#' @param legend if \code{TRUE}, adds legend for arrow transparency
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
#'   soccerSpokes(xBins=7, yBins=5, angleBins=12, legend=FALSE)
#'                    
#' # transform x,y-coords, standarise column names,
#' # filter only France pass events
#' my_df <- statsbomb %>%
#'  soccerTransform(method = 'statsbomb') %>%
#'  soccerStandardCols(method = 'statsbomb') %>%
#'   filter(team == "France" & event == "Pass")
#'   
#' # overlay flow field onto heatmap showing proportion of team passes per pitch zone
#' soccerHeatmap(my_df, xBins=7, yBins=5,
#'               title = "France passing radar") %>%
#'   soccerSpokes(my_df, xBins=7, yBins=5, angleBins=8, legend=FALSE, plot = .)
#'            
#' @seealso \code{\link{soccerHeatmap}} for drawing a heatmap of player position, or \code{\link{soccerFlow}} for drawing a single arrow for pass distance and angle per pitch zone.
#' @export
soccerSpokes <- function(df, lengthPitch=105, widthPitch=68, xBins=5, yBins=NULL, angleBins=8, x="x", y="y", angle="angle", minLength=0.6, minAlpha=0.5, minWidth=0.5, col="black", legend=TRUE, arrow=c("none", "r", "l"), title=NULL, subtitle=NULL, theme=c("light", "dark", "grey", "grass"), plot=NULL) {
  x.bin<-y.bin<-bin<-x.bin.coord<-y.bin.coord<-angle.theta<-radius<-lwd<-NULL
  
  border <- c(4, 4, 4, 4)
  
  # check value for vertical bins and match to horizontal bins if NULL
  if(is.null(yBins)) yBins <- xBins
  
  x.range <- seq(0, lengthPitch, length.out=xBins+1)
  y.range <- seq(0, widthPitch, length.out=yBins+1)
  
  angle.bin <- seq(-pi, pi, length.out=angleBins+1)
  
  # bin plot values
  x.bin.coords <- data.frame(x.bin=1:xBins, 
                             x.bin.coord=(x.range + (lengthPitch / (xBins) / 2))[1:xBins])
  y.bin.coords <- data.frame(y.bin=1:yBins,
                             y.bin.coord=(y.range + (widthPitch / (yBins) / 2))[1:yBins])
  
  angle.bin.theta <- data.frame(angle.bin=1:angleBins,
                                angle.theta=angle.bin[1:angleBins])
  
  # bin by x,y
  suppressWarnings( #suppress warnings about empty bins
    df <- df %>%
      rowwise() %>%
      mutate(x.bin=max(which(!!sym(x) > x.range)),
             y.bin=max(which(!!sym(y) > y.range)),
             bin=paste(x.bin, y.bin, sep="_")) %>%
      ungroup() %>% 
      filter(is.finite(x.bin) & is.finite(y.bin))
  )
  
  # bin by angle
  suppressWarnings( #suppress warnings about empty bins
    df <- df %>%
      group_by(bin) %>%
      rowwise() %>%
      mutate(angle.bin=max(which(!!sym(angle) > angle.bin))) %>%
      ungroup() %>% 
      filter(is.finite(angle.bin))
  )
  
  # count number of events in each angle bin
  df <- df %>%
    group_by(bin, angle.bin) %>%
    summarise(n.angles=n(),
              x.bin=x.bin[1],
              y.bin=y.bin[1]) %>% 
    ungroup()
  
  # join x,y-coords and theta to each bin
  df <- left_join(df, x.bin.coords, by="x.bin")
  df <- left_join(df, y.bin.coords, by="y.bin")
  df <- left_join(df, angle.bin.theta, by="angle.bin")
  
  df$alpha <- minAlpha + ((1-minAlpha) / max(df$n.angles) * df$n.angles)
  df$lwd <- 0.5 + ((1.6-0.5) / max(df$n.angles) * df$n.angles)
  df$radius <- (minLength * widthPitch / (yBins+5)) + ((1-minLength) * (df$n.angles / max(df$n.angles)) * widthPitch / (yBins+5))
  
  # plot
  if(missing(plot)) {
    plot <- soccerPitch(lengthPitch=lengthPitch, widthPitch=widthPitch,
                        title=title, subtitle=subtitle,
                        arrow=arrow, theme=theme)
  }
  
  plot <- plot +
      geom_spoke(data=df,
                 aes(x=x.bin.coord, y=y.bin.coord,
                     angle=angle.theta, radius=radius,
                     size=lwd, alpha=alpha),
                 col=col, arrow=arrow(length=unit(0.15,"cm"))) +
      scale_size_continuous(range=c(minWidth, 1.6)) +
      scale_alpha(range=c(minAlpha, 1))
  
  # add legend
  if(!legend) {
    plot <- plot +
      guides(radius="none", size="none", alpha="none")
  } 
  
  return(plot)
}
