# Chapter 6
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

sampleRows <- sample(1:nrow(verlander), 20)
