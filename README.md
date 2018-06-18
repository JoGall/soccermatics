soccermatics
=====

soccermatics provides tools to visualise spatial tracking and event data from football (soccer) matches. There are currently functions to visualise shot maps and expected goals, passing diagrams, heatmaps of player position, flow fields of passing or movement direction, average formation, and individual player trajectories.

Many more functions are planned - see [To Do List](https://github.com/JoGall/soccermatics/issues/8), suggestions are welcomed!

The sample x,y-coordinate data in `tromso` and `tromso_extra` were made available [by Pettersen et al. (2014)](http://home.ifi.uio.no/paalh/dataset/alfheim/), whilst event data has been kindly made public by StatsBomb.

Use of the name `soccermatics` kindly permitted by the eponymous book's author, [David Sumpter](https://www.bloomsbury.com/uk/soccermatics-9781472924124/).

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

### Updates

The package is getting a makeover over the next few months and may occassionally break during this time. Should installation fail during this time, please install the latest stable version (v0.8.4) from [this tarball](https://github.com/JoGall/soccermatics/blob/master/soccermatics_0.8.4.tar.gz) like so:

```{r}
install.packages("https://github.com/JoGall/soccermatics/blob/master/soccermatics_0.8.4.tar.gz", repo=NULL, type="source")
```

---

### Usage and samples

Below are some sample visualisations produced by `soccermetrics`. See the [package manual pdf](https://github.com/JoGall/soccermatics/blob/master/soccermatics.pdf) or the individual help files for each function (e.g. `?soccerHeatmap`) for more information and example code.

<img src="https://user-images.githubusercontent.com/17113779/41536877-c7f93904-72fe-11e8-806e-0d225475b9cd.png" width="500">

**Figure 1.** Shot map of Manchester City WFC (vs. Chelsea LFC, 2018-02-24) showing xG and shot outcome. Event data made available by StatsBomb; figure made using `soccerShotmap()`.

<img src="https://user-images.githubusercontent.com/17113779/41536876-c7dc1ad6-72fe-11e8-9be6-4331a846b033.png" width="500">

**Figure 2.** Pass map of Manchester City WFC (vs. Chelsea LFC, 2018-02-24) showing pass origin origin (circles), path (lines) and outcome. Event data made available by StatsBomb; figure made using `soccerPassmap()`.

<img src="https://user-images.githubusercontent.com/17113779/41537613-eef2567e-7300-11e8-8bd4-0517246c7e0e.png" width="500">

**Figure 3.** Average position of Tromsø IL players (vs. Strømsgodset, 2013-11-03). Made using `soccerPositions()` and `soccerDirection()`.

<img src="https://user-images.githubusercontent.com/17113779/41537612-eed60e24-7300-11e8-9da0-beae21cf5f17.png" width="500">

**Figure 4.** Average position of Manchester City WFC players (vs. Chelsea LFC, 2018-02-24). Made using `soccerPositions()` and `soccerDirection()`.

<img src="https://user-images.githubusercontent.com/17113779/41536878-c81847ea-72fe-11e8-994a-6d7fe711bace.png" width="500">

**Figure 5.** Player heatmap over 90 minutes (player #8; Tromsø IL vs. Anzhi, 2013-11-07). Bins are appromximately 5m x 5m; arrow shows direction of play. Made using `soccerHeatmap()` and `soccerDirection()`.

<img src="https://user-images.githubusercontent.com/17113779/41536879-c82f3eaa-72fe-11e8-8472-7cc43311b5cf.png" width="500">

**Figure 6.** Movement path of one player over 120 seconds (player #9; Tromsø IL vs. Strømsgodset, 2013-11-03). Made using `soccerPath()`.

<img src="https://user-images.githubusercontent.com/17113779/41536880-c8481024-72fe-11e8-9d18-b6e8472f8c90.png" width="500">

**Figure 7.** Movement path of 3 players over 15 minutes (Tromsø IL vs. Strømsgodset, 2013-11-03). Made using `soccerPath()`.

<img src="https://user-images.githubusercontent.com/17113779/30522405-4311a35c-9bc7-11e7-8d16-a5d5efe154e6.png" width="500">

**Figure 8.** Flow visualisation showing average player movement in different areas of the pitch (player #9; Tromsø IL vs. Strømsgodset, 2013-11-03). Made using `soccerFlow()`.

<img src="https://user-images.githubusercontent.com/17113779/31055659-1bd82eee-a6be-11e7-9a99-084e6116e926.png" width="500">

**Figure 9.** Spoke map of all movement events in each pitch bin (player #8; Tromsø IL vs. Anzhi, 2013-11-07). This figure uses a random sample of player movement, but this method is intended to visualise pass or shot event data which are not yet publicly available. Made using `soccerSpokes()`.
