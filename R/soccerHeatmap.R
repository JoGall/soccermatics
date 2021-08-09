#' @include soccerPitch.R
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom MASS kde2d
NULL
#' Draw a heatmap on a soccer pitch using any event or tracking data.
#' @description Draws a heatmap showing player position frequency in each area of the pitch and adds soccer pitch outlines.
#' 
#' @param df dataframe containing x,y-coordinates of player position
#' @param xBins,yBins integer, the number of horizontal (length-wise) and vertical (width-wise) bins the soccer pitch is to be divided up into. If no value for \code{yBins} is provided, it will take the value of \code{xBins}.
#' @param kde use kernel density estimates for a smoother heatmap; FALSE by default
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param arrow adds team direction of play arrow as right (\code{'r'}) or left (\code{'l'}); \code{'none'} by default
#' @param colLow,colHigh character, colours for the low and high ends of the heatmap gradient; white and red respectively by default
#' @param title,subtitle adds title and subtitle to plot; NULL by default
#' @param x,y name of variables containing x,y-coordinates
#' @return a ggplot object of a heatmap on a soccer pitch.
#' @details uses \code{ggplot2::geom_bin2d} to map 2D bin counts
#' @examples
#' library(dplyr)
#' 
#' # tracking data heatmap with 21x5 zones(~5x5m)
#' data(tromso)
#' tromso %>% 
#'   filter(id == 8) %>% 
#'   soccerHeatmap(xBins = 10)
#' 
#' # transform x,y-coords, filter only France pressure events,
#' # heatmap with 6x3 zones
#' data(statsbomb)
#' statsbomb %>%
#'   soccerTransform(method='statsbomb') %>% 
#'   filter(type.name == "Pressure" & team.name == "France") %>% 
#'   soccerHeatmap(x = "location.x", y = "location.y",
#'                 xBins = 6, yBins = 3, arrow = "r", 
#'                 title = "France (vs Argentina, 30th June 2016)", 
#'                 subtitle = "Defensive pressure heatmap")
#'
#' # transform x,y-coords, standardise column names,
#' # filter player defensive actions, plot kernel density estimate heatmap
#' statsbomb %>%
#'   soccerTransform(method='statsbomb') %>% 
#'   soccerStandardiseCols() %>% 
#'   filter(event_name %in% c("Duel", "Interception", "Clearance", "Block") &
#'          player_name == "Samuel Yves Umtiti") %>%
#'   soccerHeatmap(kde = TRUE, arrow = "r",
#'                 title = "Umtiti (vs Argentina, 30th June 2016)",
#'                 subtitle = "Defensive actions heatmap")
#'                 
#' @export
soccerHeatmap <- function(df, lengthPitch = 105, widthPitch = 68, xBins = 10, yBins = NULL, kde = FALSE, arrow = c("none", "r", "l"), colLow = "white", colHigh = "red", title = NULL, subtitle = NULL, x = "x", y = "y") {
  z <- NULL
  
  # ensure input is dataframe
  df <- as.data.frame(df)
  
  # rename variables
  df$x <- df[,x]
  df$y <- df[,y]
  
  # zonal heatmap
  if(!kde) {
    # check value for vertical bins and match to horizontal bins if NULL
    if(is.null(yBins)) yBins <- xBins
    
    # filter invalid values outside pitch limits
    df <- df[df$x > 0 & df$x < lengthPitch & df$y > 0 & df$y < widthPitch,]
    
    # define bin ranges
    x.range <- seq(0, lengthPitch, length.out = xBins+1)
    y.range <- seq(0, widthPitch, length.out = yBins+1)
    
    # plot heatmap on blank pitch lines
    p <- soccerPitch(lengthPitch, widthPitch, arrow = arrow, title = title, subtitle = subtitle, theme = "blank") +
      geom_bin2d(data = df, aes(x, y), binwidth = c(diff(x.range)[1], diff(y.range)[1])) +
      scale_fill_gradient(low = colLow, high = colHigh) +
      guides(fill="none")
    
    # redraw pitch lines  
    p <- soccerPitchFG(p, title = !is.null(subtitle), subtitle = !is.null(title))
    
  # kernel density estimate heatmap
  } else {
    dens <- kde2d(df$x, df$y, n=200, lims=c(c(0.25, lengthPitch-0.25), c(0.25, widthPitch-0.25)))
    dens_df <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
    
    p <- soccerPitch(lengthPitch, widthPitch, arrow = arrow, title = title, subtitle = subtitle, theme = "light") +
      geom_tile(data = dens_df, aes(x = x, y = y, fill = z)) +
      scale_fill_distiller(palette="Spectral", na.value="white") +
      guides(fill="none")
    
    p <- soccerPitchFG(p, title = !is.null(subtitle), subtitle = !is.null(title))
  }
  
  return(p)
  
}
