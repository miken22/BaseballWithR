# Chapter 4
# Following the examples worked through in the chapter with modifications
# to players/teams followed. Written by Mike Nowicki.

# Load packages into environment if not already.
require(curl)
require(data.table)
require(chron)
require(dplyr)
require(Lahman)
require(pacman)
require(pitchRx)
require(plyr)
require(retrosheet)

# Chapter 4.2, Runs and Wins
teams <- subset(Teams, Teams$yearID > 1990)[, c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
teams$RD <- with(teams, R - RA)
teams$Wpct <- with(teams, W/(W+L))

plot(teams$RD, teams$Wpct, xlab="run diff", ylab="Wpct", main="Run Diff v. Wpct", col="black", pch=19)

best_wpct <- with(teams, teams[teams$Wpct == max(teams$Wpct),])
best_rd <- with(teams, teams[teams$RD == max(teams$RD),])
worst_wpct <- with(teams, teams[teams$Wpct == min(teams$Wpct),])

points(best_wpct$RD, best_wpct$Wpct, pch=19, col="green")
points(best_rd$RD, best_rd$Wpct, pch=19, col="yellow") # 1998 Yankees /s
points(worst_wpct$RD, worst_wpct$Wpct, pch=19, col="red")

#text(c(-337+10), c(0.2654321), "DET '03")
#text(c(300-10), c(0.7160494+1), "SEA '01")

linfit <- lm(teams$Wpct ~ teams$RD, data = teams)
teams$linWpct <- predict(linfit)
teams$linResiduals <- residuals(linfit)

# Takes intercept, slope, and line width.
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=1)

# Win Percent v. Linear Regression model
plot(teams$Wpct ~ teams$linWpct)
linfit2 <- lm(teams$Wpct ~ teams$linWpct, data = teams)
abline(a=coef(linfit2)[1], b=coef(linfit2)[2], lwd=1)

plot(teams$RD ~ teams$linResiduals)

# Pythagorean formula for Wpct
teams$pytWpct <- with(teams, R^2 / (R^2 + RA^2))
teams$pytResiduals <- teams$Wpct - teams$pytWpct

# Estimating Pythag exponent
teams$logWratio <- log(teams$W / teams$L)
teams$logRratio <- log(teams$R / teams$RA)
pytFit <- lm(teams$logWratio ~ 0 + teams$logRratio, data = teams)

# Get game logs for TOR 2014
tor_gl <- getRetrosheet("game", 2014, "TOR")
tor2014 <- subset(tor_gl, tor_gl$HmTm=="TOR" | tor_gl$VisTm=="TOR")[, c("VisTm", "HmTm", "VisRuns", "HmRuns")]
tor2014$scoreDiff <- with(tor2014, ifelse(tor2014$HmTm=="TOR", 
                                          tor2014$HmRuns - tor2014$VisRuns, 
                                          tor2014$VisRuns - tor2014$HmRuns))
tor2014$W <- tor2014$scoreDiff > 0

# Get summary stats
aggregate(abs(tor2014$scoreDiff), list(W=tor2014$W), summary)

# Look at the same metrics for all teams
gl2014 <- getRetrosheet("game", 2014)
results <- gl2014[, c("VisTm", "HmTm", "VisRuns", "HmRuns")]
results$winner <- ifelse(results$HmRuns > results$VisRuns, as.character(results$HmTm), as.character(results$VisTm))
results$diff <- abs(results$VisRuns - results$HmRuns)

# Find all one run games from 2014
onerungames <- subset(results, diff==1)
onerunwins <- as.data.frame(table(onerungames$winner))
names(onerunwins) <- c("teamID", "onerunW")

# Look at residuals and one run wins
teams2014 <- subset(teams, yearID==2014)
teams2014[teams2014$teamID == "LAA", "teamID"] <- "ANA" # Rename for retrosheets
teams2014 <- merge(teams2014, onerunwins)

plot(teams2014$onerunW, teams2014$pytResiduals, xlab="One Run Wins", ylab="Pyth Residuals", pch=19)
# Allows you to select points on the plot to be labelled
identify(teams2014$onerunW, teams2014$pytResiduals, labels=teams2014$teamID)

# Find top closers
top_closer <- subset(Pitching, Pitching$GF > 50 & Pitching$ERA < 2.7)[,c("playerID", "yearID", "teamID")]
teams_top_closers <- merge(teams, top_closer)
summary(teams_top_closers$pytResiduals) # Mean is 0.007413, times 162 games is about 1.201 wins/season since 1990

# Determining how many runs truly lead to a win
IR <- function(RS=5, RA=5) {
  round( (RS ^ 2 + RA ^ 2) ^ 2 / (2 * RS * RA ^ 2), 1)
}

# Create all possible combinations between the two vectors
IRTable <- expand.grid(RS=seq(3, 6, 0.5), RA=seq(3, 6, 0.5))
IRTable$IRW <- IR(IRTable$RS, IRTable$RA)
xtabs(IRW ~ RS + RA, data=IRTable)

# Exercise 1
batting.decade <- function(b,e) { # Getting batting history for a decade, specified by begin b and end e.
  batting <- subset(Teams, Teams$yearID > b & Teams$yearID <= e)[, c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
  batting$RD <- with(batting, R - RA)
  batting$Wpct <- with(batting, W/(W+L))  
  return(batting)
}
# Get the batting stats over the decades
batting.1960 <- batting.decade(1960, 1970)
batting.1970 <- batting.decade(1970, 1980)
batting.1980 <- batting.decade(1980, 1990)
batting.1990 <- batting.decade(1990, 2000)

# Create linear models based on the run diff and win percentage
linfit.1960 <- lm(Wpct ~ RD, data=batting.1960)
batting.1960$linWpct <- predict(linfit.1960)
batting.1960$linResiduals <- residuals(linfit.1960)

linfit.1970 <- lm(Wpct ~ RD, data=batting.1970)
batting.1970$linWpct <- predict(linfit.1970)
batting.1970$linResiduals <- residuals(linfit.1970)

linfit.1980 <- lm(Wpct ~ RD, data=batting.1980)
batting.1980$linWpct <- predict(linfit.1980)
batting.1980$linResiduals <- residuals(linfit.1980)

linfit.1990 <- lm(Wpct ~ RD, data=batting.1990)
batting.1990$linWpct <- predict(linfit.1990)
batting.1990$linResiduals <- residuals(linfit.1990)

# Visualize plots with linear models
plot(c(batting.1960$RD, batting.1970$RD, batting.1980$RD, batting.1990$RD), 
     c(batting.1960$Wpct,batting.1970$Wpct,batting.1980$Wpct, batting.1990$Wpct),
     xlab="run diff", ylab="Wpct", main="Run Diff v. Wpct Since 1960s",
     col=c("black", "red", "blue", "green"), pch=19)

abline(a=coef(linfit.1960)[1], b=coef(linfit.1960)[2], lwd=2, col="black")
abline(a=coef(linfit.1970)[1], b=coef(linfit.1970)[2], lwd=2, col="red")
abline(a=coef(linfit.1980)[1], b=coef(linfit.1980)[2], lwd=2, col="blue")
abline(a=coef(linfit.1990)[1], b=coef(linfit.1990)[2], lwd=2, col="green")

# Identify not quite proper.
identify(c(batting.1990$RD, batting.1980$RD, batting.1970$RD, batting.1960$RD),
         c(batting.1990$Wpct, batting.1980$Wpct, batting.1970$Wpct, batting.1960$Wpct),
         labels=c(batting.1990$teamID, batting.1980$teamID, batting.1970$teamID, batting.1960$teamID))

# Compare across the decades the predicted Wpct for teams with a run diff of 10
diff10.60 <- subset(batting.1960, batting.1960$RD >= 10)
diff10.70 <- subset(batting.1970, batting.1970$RD >= 10)
diff10.80 <- subset(batting.1980, batting.1980$RD >= 10)
diff10.90 <- subset(batting.1990, batting.1990$RD >= 10)

# Exercise 2
batting.1800 <- subset(Teams, Teams$yearID<1900)[,c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA")]
batting.1800$RD <- with(batting.1800, R - RA)
batting.1800$Wpct <- with(batting.1800, W/(W+L))
# Create pythagorean model
batting.1800$pytWpct <- with(batting.1800, R^2 / (R^2 + RA^2))
batting.1800$pytResiduals <- batting.1800$Wpct - batting.1800$pytWpct
# Create a plot
plot(batting.1800$RD, batting.1800$pytResiduals, xlab = "Run Diff", ylab = "Pythagorean Residuals")

# Excercise 3
# Get gamelogs for last 15 years
gl2014 <- getRetrosheet("game", 2014)
gl2013 <- getRetrosheet("game", 2013)
gl2012 <- getRetrosheet("game", 2012)
gl2011 <- getRetrosheet("game", 2011)
gl2010 <- getRetrosheet("game", 2010)
gl2009 <- getRetrosheet("game", 2009)
gl2008 <- getRetrosheet("game", 2008)
gl2007 <- getRetrosheet("game", 2007)
gl2006 <- getRetrosheet("game", 2006)
gl2005 <- getRetrosheet("game", 2005)
gl2004 <- getRetrosheet("game", 2004)
gl2003 <- getRetrosheet("game", 2003)
gl2002 <- getRetrosheet("game", 2002)
gl2001 <- getRetrosheet("game", 2001)
gl2000 <- getRetrosheet("game", 2000)

master.log <- rbind(gl2014, gl2014, gl2012, gl2011, gl2010, gl2009, gl2008, gl2007,
                    gl2006, gl2005, gl2004, gl2003, gl2002, gl2001, gl2000)

rm(gl2014, gl2013, gl2012, gl2011, gl2010, gl2009, gl2008, gl2007,
   gl2006, gl2005, gl2004, gl2003, gl2002, gl2001, gl2000)

master.log <- master.log[,c("Date", "HmTm", "VisTm","HmMgrNm", "VisMgrNm", "HmRuns", "VisRuns")]
master.log$WMgr <- with(master.log, ifelse(HmRuns > VisRuns, as.character(HmMgrNm), as.character(VisMgrNm)))
master.log$LMgr <- with(master.log, ifelse(HmRuns < VisRuns, as.character(HmMgrNm), as.character(VisMgrNm)))

mgr.wins <- count(master.log, c("WMgr"))
mgr.loss <- count(master.log, c("LMgr"))

managers <- as.data.frame(unique(master.log$HmMgrNm)) # Get home managers, assumes every manager worked one home game in career

# Rename common column for joining all tables
dimnames(managers)[[2]] <- "ManagerName"
dimnames(mgr.wins)[[2]] <- c("ManagerName", "Wins")
dimnames(mgr.loss)[[2]] <- c("ManagerName", "Losses")

managers <- merge(managers, mgr.wins, by = "ManagerName")
managers <- merge(managers, mgr.loss, by = "ManagerName")

managers$Wpct <- with(managers, Wins/(Wins+Losses))

# Ugly way of getting all the runs for/against for a managers career, will look to improve this asap
mgr.hrf <- aggregate(master.log$HmRuns ~ master.log$HmMgrNm, sum, data=master.log)
mgr.arf <- aggregate(master.log$VisRuns ~ master.log$VisMgrNm, sum, data=master.log)
dimnames(mgr.hrf)[[2]] <- c("ManagerName", "Runs For")
dimnames(mgr.arf)[[2]] <- c("ManagerName", "Runs For")

managers <- merge(managers, mgr.hrf, by = "ManagerName")
managers <- merge(managers, mgr.arf, by = "ManagerName")
dimnames(managers)[[2]] <- c("ManagerName", "Wins", "Losses", "Wpct", "HmRF", "AwyRF")
managers$TotalRF <- managers$HmRF + managers$AwyRF


# Get runs against 
mgr.hra <- aggregate(master.log$VisRuns ~ master.log$HmMgrNm, sum, data=master.log)
mgr.ara <- aggregate(master.log$HmRuns ~ master.log$VisMgrNm, sum, data=master.log)
dimnames(mgr.hra)[[2]] <- c("ManagerName", "Runs Against")
dimnames(mgr.ara)[[2]] <- c("ManagerName", "Runs Against")
managers <- merge(managers, mgr.hra, by = "ManagerName")
managers <- merge(managers, mgr.ara, by = "ManagerName")
dimnames(managers)[[2]] <- c("ManagerName", "Wins", "Losses", "Wpct", "HmRF", "AwyRF", "TotalRF", "HmRA", "AwyRA")
managers$TotalRA <- managers$HmRA + managers$AwyRA

# Create Pythagorean model for managers Wpct
managers$pytWpct <- with(managers, TotalRF^2 / (TotalRF^2 + TotalRA^2))
managers$pytResiduals <- managers$Wpct - managers$pytWpct

# Plot to visualize the data
plot(managers$TotalRF-managers$TotalRA, managers$Wpct,
     xlab="Run Diff", ylab="Wpct", main="Run Diff v. Wpct", col="black", pch=19)
identify(managers$TotalRF-managers$TotalRA, managers$Wpct, labels = managers$ManagerName)

# Easier way using Lahman Database:
managers.lahman <- subset(Managers, Managers$yearID > 2000)[,c("playerID", "yearID", "teamID", "W", "L")]
teams.2000 <- subset(Teams, Teams$yearID > 2000)[,c("yearID", "teamID", "R", "RA")]

managers.lahman <- merge(managers.lahman, teams.2000, by=c("yearID", "teamID"))

managers.totals <- aggregate(managers.lahman$W ~ managers.lahman$playerID, sum, data=managers.lahman)
managers.totals <- merge(managers.totals, 
                         aggregate(managers.lahman$L ~ managers.lahman$playerID, sum, data=managers.lahman),
                         by= "managers.lahman$playerID")
managers.totals <- merge(managers.totals,
                         aggregate(managers.lahman$R ~ managers.lahman$playerID, sum, data=managers.lahman),
                         by="managers.lahman$playerID")
managers.totals <- merge(managers.totals,
                         aggregate(managers.lahman$RA ~ managers.lahman$playerID, sum, data=managers.lahman),
                         by="managers.lahman$playerID")

dimnames(managers.totals)[[2]] <- c("playerID", "W", "L", "R", "RA")

managers.totals$RD <- with(managers.totals, R - RA)
managers.totals$Wpct <- with(managers.totals, W/(W+L))
managers.totals$pytWpct <- with(managers.totals, R^2/(R^2 + RA^2))
managers.totals$pytResiduals <- with(managers.totals, Wpct - pytWpct)

managers.totals <- with(managers.totals, subset(managers.totals, W+L >= 162))

plot(managers.totals$RD, managers.totals$Wpct,
     xlab="Run Diff", ylab="Wpct", main="Run Diff vs Wpct for Managers since 2000", pch=19)
linfit.mgr <- lm(Wpct ~ RD, data=managers.totals)
managers.totals$linWpct <- predict(linfit.mgr)
managers.totals$linResiduals <- residuals(linfit.mgr)
abline(a=coef(linfit.mgr)[1], b=coef(linfit.mgr)[2], lwd=2)
