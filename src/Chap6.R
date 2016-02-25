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
require(RSQLite)
require(pitchRx)
require(Lahman)
library(lattice)

# Will do the first section analying Hutchisons bizarre 2015 season.
db <- src_sqlite("data/pitchfx.sqlite", create = FALSE)
#scrape(start = "2015-04-05", end = "2015-11-05", connect = db$con)
#pitch <- dbGetQuery(db$con, "select pitch_type, px, pz, start_speed, des, gameday_link, num from pitch")
#bat <- dbGetQuery(db$con, "select batter_name, pitcher_name, event, stand, b_height, gameday_link, num, b, s, o from atbat")
#combined <- join(bat, pitch, c("gameday_link","num"))

#drew.pitch <- with(combined, subset(combined, pitcher_name == "Drew Hutchison"))
