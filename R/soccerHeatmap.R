#' @include soccerPitch.R
#' @import ggplot2
#' @import dplyr
#' @importFrom MASS kde2d
NULL
#' Draw a heatmap on a soccer pitch.
#' @description Draws a heatmap showing player position frequency in each area of the pitch and adds soccer pitch outlines.
#' 
#' @param df dataframe containing x,y-coordinates of player position
#' @param xBins,yBins integer, the number of horizontal (length-wise) and vertical (width-wise) bins the soccer pitch is to be divided up into. If no value for \code{yBins} is provided, it will take the value of \code{xBins}.
#' @param kde use kernel density estimates for a smoother heatmap
#' @param lengthPitch,widthPitch numeric, length and width of pitch in metres.
#' @param arrow optional, adds arrow showing team attack direction as right (\code{'r'}) or left (\code{'l'})
#' @param colLow,colHigh character, colours for the low and high ends of the heatmap gradient.
#' @param title,subtitle optional, adds title and subtitle to plot
#' @param x,y = name of variables containing x,y-coordinates
#' @return a ggplot object of a heatmap on a soccer pitch.
#' @details uses \code{ggplot2::geom_bin2d} to map 2D bin counts
#' @examples
#' library(dplyr)
#' 
#' # Heatmap w/ ~5x5m bins (pitchLength / 5 = 21, pitchWidth / 5 = 13.6)
#' data(tromso)
#' tromso %>% 
#'   filter(id == 8) %>% 
#'   soccerHeatmap(xBins = 10)
#' 
#' # Heatmap w/ 6x3 zones
#' data(statsbomb)
#' statsbomb %>%
#'   filter(type.name == "Pressure" & team.name == "France") %>% 
#'   soccerHeatmap(x = "location.x", y = "location.y", xBins = 6, yBins = 3,
#'                 arrow = "r", 
#'                 title = "France (vs Argentina, 30th June 2016)", 
#'                 subtitle = "Defensive pressure heatmap")
#'
#' # Kernel density estimate heatmap
#' statsbomb %>%
#'   filter(type.name %in% c("Duel", "Interception", "Clearance", "Block") & player.name == "Samuel Yves Umtiti") %>%
#'   soccerHeatmap(kde = T, x = "location.x", y = "location.y", arrow = "r",
#'                 title = "Umtiti (vs Argentina, 30th June 2016)",
#'                 subtitle = "Defensive actions heatmap")
#'                 
#' @export
soccerHeatmap <- function(df, lengthPitch = 105, widthPitch = 68, xBins = 10, yBins = NULL, kde = FALSE, arrow = c("none", "r", "l"), colLow = "white", colHigh = "red", title = NULL, subtitle = NULL, x = "x", y = "y") {
  
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
      guides(fill=FALSE)
    
    # redraw pitch lines  
    p <- soccerPitchFG(p, title = !is.null(subtitle), subtitle = !is.null(title))
    
  # kernel density estimate heatmap
  } else {
    dens <- kde2d(df$x, df$y, n=200, lims=c(c(0.25, lengthPitch-0.25), c(0.25, widthPitch-0.25)))
    dens_df <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
    
    p <- soccerPitch(lengthPitch, widthPitch, arrow = arrow, title = title, subtitle = subtitle, theme = "light") +
      geom_tile(data = dens_df, aes(x = x, y = y, fill = z)) +
      scale_fill_distiller(palette="Spectral", na.value="white") +
      guides(fill = FALSE)
    
    p <- soccerPitchFG(p, title = !is.null(subtitle), subtitle = !is.null(title))
  }
  
  return(p)
  
}
