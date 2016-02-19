# Chapter 7
# Following the examples worked through in the chapter with modifications
# to players/teams used Written by Mike Nowicki.

require(curl)
require(data.table)
require(dplyr)
require(chron)
require(pacman)
require(plyr)
require(ggplot2)
require(retrosheet)
require(pitchRx)
require(Lahman)
library(lattice)

# While the book first looks at Mussina I chose to look up Drew Hutchison to see if anything can
# be found to explain his extremely weird year.

hutchison <- expand.grid(balls=0:3, strikes=0:2)
hutchison$values <- c(157, 144, 204, 272, 50, 74, 107, 148, 13, 30, 55, 94)

countmap <- function(data) {
  require(plotrix)
  data <- xtabs(values ~ ., data)
  color2D.matplot(data, show.values=2, axes=FALSE, xlab="", ylab="")
  axis(side=2, at=3.5:0.5, labels = rownames(data), las=1)
  axis(side=3, at=0.5:2.5, labels = colnames(data), las=1)
  mtext(text="balls", side=2, line=2, cex.lab=1)
  mtext(text="strikes", side=3, line=2, cex.lab=1)
}
countmap(hutchison)
# xtabs(hutchison$values ~ ., hutchison)

pbp2015 <- read.csv("data/all_data.csv")
headers <- read.csv("data/fields.csv")
names(pbp2015) <- headers$Header

tor.games.all <- getRetrosheet("play", 2015, "TOR")
head(tor.games.all[[5]])
