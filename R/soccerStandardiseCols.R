#' @import dplyr
#' @importFrom magrittr "%>%"
NULL
#' Rename columns in a dataframe for easier use with other {soccermatics} functions
#'
#' @description Rename columns (e.g. \code{"location.x"} -> \code{"x"}, \code{"team.name"} -> \code{"team"}, etc...) to interface directly with other soccermatics functions without having to explicitly define column names as arguments. Currently only supports Statsbomb data.
#' 
#' @param df a dataframe of Statsbomb event data
#' @param method source of data; only \code{"statsbomb"} currently supported
#' @return a dataframe with column names x, y, distance, angle, player_id, player_name, team_name, event_name
#' @examples
#' library(dplyr)
#' data(statsbomb)
#' 
#' # transform x,y-coords, standardise column names
#' my_df <- statsbomb %>% 
#'   soccerTransform(method = 'statsbomb') %>% 
#'   soccerStandardiseCols(method = 'statsbomb')
#'   
#' # feed to other functions without defining variables,
#' # x, y, id,distance, angle, etc...
#' soccerHeatmap(my_df)
#' 
#' @export
soccerStandardiseCols <- function(df, method = c("statsbomb")) {
  location.x<-location.y<-pass.length<-pass.angle<-player.id<-player.name<-team.name<-type.name<-NULL
  
  if(method[1] == "statsbomb") {

    df <- df %>%
      select(-id) %>% 
      rename(x = location.x,
             y = location.y,
             distance = pass.length,
             angle = pass.angle,
             player_id = player.id,
             player_name = player.name,
             team_name = team.name,
             event_name = type.name)
    
  }
  
  return(df)
  
}
