library(soccermatics)

# get data
url <- "http://s3-irl-epl.squawka.com/dp/ingame/32991"
dat <- getPasses(url)

# set pitch dimensions
pitchLength <- 101
pitchWidth <- 68

# scale x,y-coords to pitch size
dat$x.start <- dat$x.start * (pitchLength / 100)
dat$x.end <- dat$x.end * (pitchLength / 100)
dat$y.start <- dat$y.start * (pitchWidth / 100)
dat$y.end <- dat$y.end * (pitchWidth / 100)

# subset one player
dd <- subset(dat, dat$passer == "Jordan Henderson")

# plot
p <- soccerPitchBG(pitchLength, pitchWidth)

p +
  geom_segment(data = dd, aes(x = x.start, xend = x.end, y = y.start, yend = y.end, col = as.factor(completed)), size = 1, alpha = 0.75, arrow = arrow(length = unit(0.15, "cm"))) +
  guides(col = FALSE)


# pass network
dd <- subset(dat, dat$team == "Liverpool")

dd$name <- stringr::str_extract(dd$passer, '\\w+$')

pos <- dd %>%
  group_by(name) %>%
  summarise(x = mean(x.start), y = mean(y.start), n = n()) %>%
  arrange(desc(n)) %>%
  head(11)

p +
  geom_point(data = pos, aes(x, y), shape = 21, size = 6, colour = "black", fill = "red", stroke = 1) +
  geom_point(data = pos, aes(x, y), shape = 21, size = 6, colour = "black", fill = NA, stroke = 1) +
  geom_text(data = pos, aes(x, y, label = name), hjust=0.5, vjust=2, size = 3.7)
