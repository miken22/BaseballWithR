# Chapter 7
# Following the examples worked through in the chapter with modifications
# to players/teams used Written by Mike Nowicki.

require(curl)
require(data.table)
require(dplyr)
require(chron)
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

drew <- subset(Master, Master$nameFirst == "Drew" & Master$nameLast == "Hutchison")

pbp2015 <- read.csv("data/all_data.csv")
headers <- read.csv("data/fields.csv")
names(pbp2015) <- headers$Header
# Retrosheet data is missing games before April 12 for the Jays for some reason, considers
# Drew's first start as the game vs Baltimore.
drew.pbp2015 <- subset(pbp2015, pbp2015$PIT_ID == drew$retroID)

# Remove non-pitch events:
drew.pbp2015$PITCH_SEQ <- gsub("[.>123+*]", "", drew.pbp2015$PITCH_SEQ)
# Find all pitch sequences (taken from Appendix A)
drew.pbp2015$c00 <- 1
drew.pbp2015$c10 <- grepl("^[BIPV]", drew.pbp2015$PITCH_SEQ)
drew.pbp2015$c01 <- grepl("^[CFKLMOQRST]", drew.pbp2015$PITCH_SEQ)
drew.pbp2015$c20 <- grepl("^[BIPV]{2}", drew.pbp2015$PITCH_SEQ)
drew.pbp2015$c30 <- grepl("^[BIPV]{3}", drew.pbp2015$PITCH_SEQ)
drew.pbp2015$c02 <- grepl("^[CFKLMOQRST]{2}", drew.pbp2015$PITCH_SEQ)

drew.pbp2015$c11 <- grepl("^([CFKLMOQRST][BIPV]|[BIPV][CFKLMOQRST])", drew.pbp2015$PITCH_SEQ)
drew.pbp2015$c21 <- grepl("^([BIPV][BIPV][CFKLMOQRST]
                          |[BIPV][CFKLMOQRST][BIPV]
                          |[CFKLMOQRST][BIPV][BIPV])", drew.pbp2015$PITCH_SEQ)
drew.pbp2015$c31 <- grepl("^([BIPV][BIPV][BIPV][CFKLMOQRST]
                          |[BIPV][CFKLMOQRST][BIPV][BIPV]|[BIPV][BIPV][CFKLMOQRST][BIPV]
                          |[BIPV][BIPV][BIPV][CFKLMOQRST])", drew.pbp2015$PITCH_SEQ)

drew.pbp2015$c12 <- grepl("^([CFKLMOQRST][CFKLMOQRST][FR]*[BIPV]
                          |[BIPV][CFKLMOQRST][CFKLMOQRST]
                          |[CFKLMOQRST][BIPV][CFKLMOQRST])", drew.pbp2015$PITCH_SEQ)

drew.pbp2015$c22 <- grepl("^([CFKLMOQRST][CFKLMOQRST][FR]*[BIPV][FR]*[BIPV]
                          |[BIPV][BIPV][CFKLMOQRST][CFKLMOQRST]
                          |[BIPV][CFKLMOQRST][BIPV][CFKLMOQRST]
                          |[BIPV][CFKLMOQRST][CFKLMOQRST][FR]*[BIPV]
                          |[CFKLMOQRST][BIPV][CFKLMOQRST][FR]*[BIPV]
                          |[CFKLMOQRST][BIPV][BIPV][CFKLMOQRST])", drew.pbp2015$PITCH_SEQ)

drew.pbp2015$c32 <- grepl("^[CFKLMOQRST]*[BIPV][CFKLMOQRST]*[BIPV][CFKLMOQRST]*[BIPV]", drew.pbp2015$PITCH_SEQ) &
  grepl("^[BIPV]*[CFKLMOQRST][BIPV]*[CFKLMOQRST]", drew.pbp2015$PITCH_SEQ)

# Convert TRUE/FALSE to 1/0:
for( i in 0:3) {
  for (j in 0:2) {
    col.name <- paste("c", i, j, sep = "")
    drew.pbp2015[,col.name] <- gsub("FALSE", 0, drew.pbp2015[,col.name])
    drew.pbp2015[,col.name] <- gsub("TRUE", 1, drew.pbp2015[,col.name])
  }
}

# Compute the run values since I'm not using their provided data:
drew.pbp2015$RUNS <- with(drew.pbp2015, AWAY_SCORE_CT + HOME_SCORE_CT)

drew.pbp2015$HALF_INNING <- with(drew.pbp2015, paste(GAME_ID, INN_CT, BATTING_TM))

drew.pbp2015$RUNS_SCORED <- with(drew.pbp2015, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
RUNS_SCORED_INNING <- aggregate(drew.pbp2015$RUNS_SCORED, list(HALF_INNING=drew.pbp2015$HALF_INNING), sum)
RUNS_SCORED_START <- aggregate(drew.pbp2015$RUNS, list(HALF_INNING=drew.pbp2015$HALF_INNING), "[", 1)

MAX <- data.frame(HALF_INNING = RUNS_SCORED_START$HALF_INNING)
MAX$x <- RUNS_SCORED_INNING$x + RUNS_SCORED_START$x

drew.pbp2015 <- merge(drew.pbp2015, MAX)
N <- ncol(drew.pbp2015)
names(drew.pbp2015)[N] <- "MAX_RUNS"

drew.pbp2015$RUNS_ROI <- with(drew.pbp2015, MAX_RUNS - RUNS)
RUNNER1 <- ifelse (as.character(drew.pbp2015[,"BASE1_RUN_ID"]) == "", 0, 1)
RUNNER2 <- ifelse (as.character(drew.pbp2015[,"BASE2_RUN_ID"]) == "", 0, 1)
RUNNER3 <- ifelse (as.character(drew.pbp2015[,"BASE3_RUN_ID"]) == "", 0, 1)

get.state <- function(runner1, runner2, runner3, outs) {
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}
drew.pbp2015$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, drew.pbp2015$OUTS_CT)

NRUNNER1 <- with(drew.pbp2015, as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1))
NRUNNER2 <- with(drew.pbp2015, as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | BAT_DEST_ID == 2))
NRUNNER3 <- with(drew.pbp2015, as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 | RUN3_DEST_ID == 3 | BAT_DEST_ID == 3))
NOUTS <- with(drew.pbp2015, OUTS_CT + EVENT_OUTS_CT)
drew.pbp2015$NEW_STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

drew.pbp2015 <- subset(drew.pbp2015, (STATE != NEW_STATE) | RUNS_SCORED > 0)

# Adjust for partial half innings
data.outs <- ddply(drew.pbp2015, .(HALF_INNING), summarize, Outs.Inning=sum(EVENT_OUTS_CT))
drew.pbp2015 <- merge(drew.pbp2015, data.outs)
pbpdata2015C <- subset(drew.pbp2015, Outs.Inning == 3)

RUNS <- with(pbpdata2015C, aggregate(RUNS_ROI, list(STATE), mean))

# Data for Drew in 2015 shows these two situations never arose, so we must manually include them
RUNS <- rbind(RUNS, c("011 0", as.numeric(0)))
RUNS <- rbind(RUNS, c("001 0", as.numeric(0)))      
RUNS$Outs <- substr(RUNS$Group, 5, 5)
RUNS$x <- as.numeric(RUNS$x)
RUNS <- RUNS[order(RUNS$Outs),]
# Creates the run expectancy matrix
RUNS.out <- matrix(round(RUNS$x, 2), 8, 3)
dimnames(RUNS.out)[[2]] <- c("0 Outs", "1 Out", "2 Outs")
dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

# Compute the value of a batting play
RUNS_POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1)
dimnames(RUNS_POTENTIAL)[[1]] <- c(RUNS$Group.1, "000 3", "001 3", "010 3", "011 3", "100 3", "101 3", "110 3", "111 3")
drew.pbp2015$RUNS_STATE <- RUNS_POTENTIAL[drew.pbp2015$STATE, ]
drew.pbp2015$RUNS_NEW_STATE <- RUNS_POTENTIAL[drew.pbp2015$NEW_STATE, ]
drew.pbp2015$RUNS_VALUE <- drew.pbp2015$RUNS_NEW_STATE - drew.pbp2015$RUNS_STATE + drew.pbp2015$RUNS_SCORED

ab10 <- subset(drew.pbp2015, c10 == 1)
ab01 <- subset(drew.pbp2015, c01 == 1)

ab11 <- subset(drew.pbp2015, c01 == 1)
ab20 <- subset(drew.pbp2015, c01 == 1)
ab02 <- subset(drew.pbp2015, c01 == 1)

ab21 <- subset(drew.pbp2015, c01 == 1)
ab12 <- subset(drew.pbp2015, c01 == 1)
ab30 <- subset(drew.pbp2015, c01 == 1)

ab31 <- subset(drew.pbp2015, c01 == 1)
ab32 <- subset(drew.pbp2015, c01 == 1)

# Comparing what runs were worth when Drew worked ahead the expected runs are -0.035 in the 0-1 count,
# and is +0.089 runs when Drew falls behind 1-0.
c(mean(ab10$RUNS_VALUE), mean(ab01$RUNS_VALUE))

runs.by.count <- expand.grid(balls=0:3, strikes=0:2)
runs.by.count$values <- 0

bs.count.run.value <- function (b, s) {
  column.name <- paste("c", b, s, sep="")
  column.name
  mean(drew.pbp2015[drew.pbp2015[, column.name] == 1, "RUNS_VALUE"])
}

runs.by.count$values <- mapply(FUN=bs.count.run.value, 
                              b = runs.by.count$balls, 
                              s = runs.by.count$strikes)
countmap(runs.by.count)
