soccermatics
=====

soccermatics provides tools to visualise spatial tracking and event data from football (soccer) matches. There are currently functions to visualise shot maps (with xG), average positions, heatmaps, and individual player trajectories. There are also helper functions to smooth, interpolate, and prepare x,y-coordinate tracking data for plotting and calculating further metrics.

Many more functions are planned - see [To Do List](https://github.com/JoGall/soccermatics/issues/8) - suggestions and/or help welcomed!

The sample x,y-coordinate tracking data in `tromso` and `tromso_extra` were made available by [Pettersen et al. (2014)](http://home.ifi.uio.no/paalh/dataset/alfheim/), whilst the event data in `statsbomb` is taken from the World Cup 2018 data [made public by StatsBomb](https://github.com/statsbomb/open-data).

Use of the name soccermatics kindly permitted by the eponymous book's author, [David Sumpter](https://www.bloomsbury.com/uk/soccermatics-9781472924124/).

soccermatics is built on R v3.4.2.

---

### Installation
You can install `soccermatics` from GitHub in R using [`devtools`](https://github.com/hadley/devtools):

```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jogall/soccermatics")

library(soccermatics)
```

---

### Examples

Below are some sample visualisations produced by `soccermetrics` with code snippets underneath. See the individual help files for each function (e.g. `?soccerHeatmap`) for more information.

#### Shotmaps (showing xG)

Dark theme:

<img src="https://user-images.githubusercontent.com/17113779/46407957-7d42dd80-c708-11e8-8005-2c1fd6afb1f0.png" width="500">

```{r}
statsbomb %>%
  filter(team.name == "France") %>%
  soccerShotmap(theme = "dark")
```


Grass theme with custom colours:

<img src="https://user-images.githubusercontent.com/17113779/46407959-7d42dd80-c708-11e8-85b3-b808e4cec48f.png" width="500">

```{r}
statsbomb %>%
  filter(team.name == "Argentina") %>%
  soccerShotmap(theme = "grass", colGoal = "yellow", colMiss = "blue", legend = T)
```


#### Passing networks

Default aesthetics:

<img src="https://user-images.githubusercontent.com/17113779/47968043-4c81fb00-e05c-11e8-8f10-1d77d60fe177.png" width="500">

```{r}
statsbomb %>%
  filter(team.name == "Argentina") %>%
  soccerPassmap(fill = "lightblue", arrow = "r",
                title = "Argentina (vs France, 30th June 2018)")
```

Grass background, non-transparent edges:

<img src="https://user-images.githubusercontent.com/17113779/47968127-3e80aa00-e05d-11e8-92a0-77f15d82e7bb.png" width="500">

```{r}
statsbomb %>%
  filter(team.name == "France") %>%
  soccerPassmap(fill = "blue", minPass = 3,
                edge_max_width = 30, edge_col = "grey40", edge_alpha = 1,
                title = "France (vs Argentina, 30th June 2018)")
```


#### Heatmaps

Passing heatmap with approx 10x10m bins:

<img src="https://user-images.githubusercontent.com/17113779/46407961-7d42dd80-c708-11e8-843c-1963c168eb52.png" width="500">

```{r}
statsbomb %>%
  filter(type.name == "Pass" & team.name == "France") %>% 
  soccerHeatmap(x = "location.x", y = "location.y",
                title = "France (vs Argentina, 30th June 2016)", 
                subtitle = "Passing heatmap")
```


Defensive pressure heatmap with approx 5x5m bins:

<img src="https://user-images.githubusercontent.com/17113779/46407962-7ddb7400-c708-11e8-8884-d067746d7747.png" width="500">

```{r}
statsbomb %>%
  filter(type.name == "Pressure" & team.name == "France") %>% 
  soccerHeatmap(x = "location.x", y = "location.y", xBins = 21, yBins = 14,
                title = "France (vs Argentina, 30th June 2016)", 
                subtitle = "Defensive pressure heatmap")
```

Player position heatmaps also possible using TRACAB-style x,y-location data.


#### Average position

Average pass position:

<img src="https://user-images.githubusercontent.com/17113779/46407966-7ddb7400-c708-11e8-986c-2b7a71472710.png" width="500">

```{r}
statsbomb %>% 
  filter(type.name == "Pass" & team.name == "France" & minute < 43) %>% 
  soccerPositionMap(id = "player.name", x = "location.x", y = "location.y", 
                    fill1 = "blue", grass = T,
                    arrow = "r", 
                    title = "France (vs Argentina, 30th June 2016)", 
                    subtitle = "Average pass position (1' - 42')")
```


Average pass position (both teams):

<img src="https://user-images.githubusercontent.com/17113779/46407954-7caa4700-c708-11e8-97cb-dfb8713a9a1c.png" width="500">

```{r}
statsbomb %>% 
  filter(type.name == "Pass" & minute < 43) %>% 
  soccerPositionMap(id = "player.name", team = "team.name", x = "location.x", y = "location.y",
    fill1 = "lightblue", fill2 = "blue", label_col = "black",
    repel = T, teamToFlip = 2,
    title = "France vs Argentina, 30th June 2018",
    subtitle = "Average pass position (1' - 42')")
```


Average player position using TRACAB-style x,y-location data:

<img src="https://user-images.githubusercontent.com/17113779/46407955-7d42dd80-c708-11e8-8514-b1c289d829e6.png" width="500">

```{r}
tromso_extra[1:11,] %>% 
  soccerPositionMap(grass = T, title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)", subtitle = "Average player position (1' - 16')")
```


#### Custom plots

Inbuilt functions for many of these will be added soon.


Locations of multiple events:

<img src="https://user-images.githubusercontent.com/17113779/46407960-7d42dd80-c708-11e8-8be7-1a970418287b.png" width="500">

```{r}
d2 <- statsbomb %>% 
  filter(type.name %in% c("Pressure", "Interception", "Block", "Dispossessed", "Ball Recovery") & team.name == "France")

soccerPitch(arrow = "r", 
            title = "France (vs Argentina, 30th June 2016)", 
            subtitle = "Defensive actions") +
  geom_point(data = d2, aes(x = location.x, y = location.y, col = type.name), size = 3, alpha = 0.5)
```


Start and end locations of passes:


<img src="https://user-images.githubusercontent.com/17113779/46408063-c09d4c00-c708-11e8-82fa-2c5f7f2d551c.png" width="500">

```{r}
d3 <- statsbomb %>% 
  filter(type.name == "Pass" & team.name == "France") %>% 
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), 1, 0)))

soccerPitch(arrow = "r",
            title = "France (vs Argentina, 30th June 2016)", 
            subtitle = "Pass map") +
  geom_segment(data = d3, aes(x = location.x, xend = pass.end_location.x, y = location.y, yend = pass.end_location.y, col = pass.outcome), alpha = 0.75) +
  geom_point(data = d3, aes(x = location.x, y = location.y, col = pass.outcome), alpha = 0.5) +
  guides(colour = FALSE)
```


#### Player paths

Path of a single player:

<img src="https://user-images.githubusercontent.com/17113779/46407964-7ddb7400-c708-11e8-8e93-13f4748d4c2f.png" width="500">

```{r}
subset(tromso, id == 8)[1:1800,] %>%
  soccerPath(col = "red", grass = TRUE, arrow = "r",
             title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)",
             subtitle = "Player #8 path (1' - 3')")
```


Path of multiple players:

<img src="https://user-images.githubusercontent.com/17113779/46407965-7ddb7400-c708-11e8-8337-af73b3048e94.png" width="500">

```{r}
tromso %>%
  dplyr::group_by(id) %>%
  dplyr::slice(1:1200) %>%
  soccerPath(id = "id", arrow = "r", 
             title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)", 
             subtitle = "Player paths (1')")
```

---
