library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)

# Read in files and prep data
Xdata <- read.csv("PitcherX Game.csv", header = T)
Ydata <- read.csv("PitcherY Game.csv", header = T)
Zdata <- read.csv("PitcherZ Game.csv", header = T)

Xdata$PitchCount <- seq.int(nrow(Xdata))
Ydata$PitchCount <- seq.int(nrow(Ydata))
Zdata$PitchCount <- seq.int(nrow(Zdata))

alldata <- rbind(Xdata, Ydata)
# alldata <- rbind(alldata, Zdata) --> Didn't work because column mislabelled in Zdata
colnames(Zdata)
colnames(alldata)
colnames(Zdata)[24] <- colnames(alldata)[24]
colnames(Zdata)
alldata <- rbind(alldata, Zdata)

alldata$K <- factor(alldata$KorBB == "Strikeout", labels = c(0, 1))
alldata$K <- as.numeric(as.character(alldata$K))
alldata$BB <- factor(alldata$KorBB == "Walk", labels = c(0, 1))
alldata$BB <- as.numeric(as.character(alldata$BB))
alldata$Hit <- factor(alldata$PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), labels = c(0, 1))
alldata$Hit <- as.numeric(as.character(alldata$Hit))
alldata$OutYorN <- factor(alldata$KorBB == "Strikeout" | alldata$OutsOnPlay > 0, labels = c(0, 1))
alldata$OutYorN <- as.numeric(as.character(alldata$OutYorN))

# Question 4a: Command
HeightDev <- abs(alldata$PlateLocHeight - 2.5) - 1
SideDev <- abs(alldata$PlateLocSide) - 0.75
TotalDev <- abs(HeightDev) + abs(SideDev)
mydata <- data.frame(alldata$Pitcher, alldata$TaggedPitchType, TotalDev)
colnames(mydata) <- c("Pitcher", "TaggedPitchType","TotalDev")

my_func <- function(x) c(length(x), mean(x), var(x))  
agg_table <- aggregate(TotalDev ~  Pitcher * TaggedPitchType, data = mydata, my_func)
agg_table <- agg_table[order(agg_table$Pitcher, decreasing = FALSE),]
write.csv(agg_table, file = "agg_table.csv")
mytable <- read.csv("agg_table.csv", header = T)
colnames(mytable) <- c("Index", "Pitcher", "PitchType", "Count", "Mean_CornerMiss", "Variance")
mytable <- subset(mytable, select=c("Pitcher", "PitchType", "Count", "Mean_CornerMiss", "Variance"))
mytable$Mean_CornerMiss <- round(mytable$Mean_CornerMiss, 3)
mytable$Variance <- round(mytable$Variance, 3)
mytable

g <- tableGrob(mytable, rows = NULL, theme = ttheme_minimal(
  core=list(bg_params = list(fill=c("white","white","white","white",
                                    "grey", "grey", "grey", "grey", "grey"),
                             col=TRUE), fg_params=list(fontface = 1))))

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

grid.draw(g)


# Question 4b: Out Pitch
dev.off()
mydata2 <- data.frame(alldata$Pitcher, alldata$TaggedPitchType, alldata$OutYorN)
colnames(mydata2) <- c("Pitcher", "TaggedPitchType","OutYorN")

my_func2 <- function(x) c(sum(x), length(x), (sum(x)/length(x))) 
agg_table <- aggregate(OutYorN ~ Pitcher * TaggedPitchType, data = mydata2, my_func2)
agg_table <- agg_table[order(agg_table$Pitcher, decreasing = FALSE),]
write.csv(agg_table, file = "agg_table.csv")
mytable2 <- read.csv("agg_table.csv", header = T)
colnames(mytable2) <- c("Index", "Pitcher", "PitchType", "OutPitches", "TotalPitches", "OutRate")
mytable2 <- subset(mytable2, select=c("Pitcher", "PitchType", "OutPitches", "TotalPitches", "OutRate"))
mytable2$OutRate <- round(mytable2$OutRate, 3)
mytable2

g2 <- tableGrob(mytable2, rows = NULL, theme = ttheme_minimal(
  core=list(bg_params = list(fill=c("white","white","white","white",
                                    "grey", "grey", "grey", "grey", "grey"),
                             col=TRUE), fg_params=list(fontface = 1))))
ind <- find_cell(g2, 3, 5, "core-bg")
ind2 <- find_cell(g2, 7, 5, "core-bg")
ind3 <- find_cell(g2, 13, 5, "core-bg")
g2$grobs[ind][[1]][["gp"]] <- gpar(fill="mediumpurple1", col = "mediumpurple4", lwd=5)
g2$grobs[ind2][[1]][["gp"]] <- gpar(fill="mediumpurple1", col = "mediumpurple4", lwd=5)
g2$grobs[ind3][[1]][["gp"]] <- gpar(fill="mediumpurple1", col = "mediumpurple4", lwd=5)
grid.draw(g2)


# Question 4c: Overall Performance
# Plot velocity and spin rate throughout game
dev.off()
VeloSpinPlots <- function(x){
  VeloPlot <- ggplot(data=x, aes(x=PitchCount, y=Release.Speed, group=TaggedPitchType, colour = TaggedPitchType)) + 
    geom_line(size = 1) + ggtitle("Velocity throughout game")
  SpinPlot <- ggplot(data=x, aes(x=PitchCount, y=SpinRate, group=TaggedPitchType, colour = TaggedPitchType)) + 
    geom_line(size = 1) + ggtitle("Spin Rate throughout game")
  grid.arrange(VeloPlot, SpinPlot, ncol=1)
}

VeloSpinPlots(Xdata)
VeloSpinPlots(Ydata)
VeloSpinPlots(Zdata)

# Plot exit velocity against launch angle w/ barrel zone highlighted
aggregate(ExitSpeed ~ Pitcher, data = alldata, mean)
Bzone <- data.frame(x=c(98, 98, 116, 116), y=c(30, 26, 8, 50))
nBzone <- data.frame(x=c(96, 96, 116, 116), y=c(32, 24, 5, 53))
plot1 <- ggplot(show.legend = TRUE) + xlim(55,116) + ggtitle("Exit Speed vs Launch Angle by Pitcher       ") + 
           geom_point(data = alldata, mapping = aes(x = ExitSpeed, y = Angle, group = Pitcher, colour = Pitcher), size = 4) + 
           theme(legend.position = c(0.9,0.15)) + theme(legend.background = element_rect(fill="white", colour="black"))
plot1 <- plot1 + geom_polygon(data = nBzone, mapping = aes(x=x,y=y), 
                              fill = "lightgoldenrod", colour = "red", alpha = .2)
plot1 <- plot1 + geom_polygon(data = Bzone, mapping = aes(x=x,y=y), 
                     fill = "orange", colour = "red", alpha = .2)
plot1 + annotate("text", x = 109, y = 29, label = "Barrel Zone", colour = "orange", size = 6.5) + 
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))


# Calculate walks, runs, hits, Ks, and outs per plate appearance
GameStats <- function(x, y){
  mydata4 <- filter(alldata, Pitcher == y)
  PACount <- length(unique(x$PAofGame))
  BBperPA <- round(sum(mydata4$BB)/PACount, 3)
  RperPA <- round(sum(mydata4$RunsScored)/PACount, 3)
  HperPA <- round(sum(mydata4$Hit)/PACount, 3)
  KperPA <- round(sum(mydata4$K)/PACount, 3)
  OutperPA <- round((sum(mydata4$OutsOnPlay) + sum(mydata4$K))/PACount, 3)
  cbind(BBperPA, RperPA, HperPA, KperPA, OutperPA)
}    

Xstats <- GameStats(Xdata, "Pitcher X")
Ystats <- GameStats(Ydata, "Pitcher Y")
Zstats <- GameStats(Zdata, "Pitcher Z")
StatsTbl <- data.frame(rbind(Xstats, Ystats, Zstats), row.names = c("Pitcher X", "Pitcher Y", "Pitcher Z"))

dev.off()
g4 <- tableGrob(StatsTbl, cols = c("BB/PA", "Runs/PA", "Hits/PA", "K/PA", "Outs/PA"))
grid.draw(g4)

# Plot values for each pitcher
c1 <- colnames(StatsTbl)
Xstats <- data.frame(c1, t(Xstats), row.names = NULL) 
colnames(Xstats) <- c("stat", "value")
Ystats <- data.frame(c1, t(Ystats), row.names = NULL) 
colnames(Ystats) <- c("stat", "value")
Zstats <- data.frame(c1, t(Zstats), row.names = NULL)
colnames(Zstats) <- c("stat", "value")

Xstats <- within(Xstats, stat <- factor(stat, levels= c("BBperPA", "RperPA", "HperPA", "KperPA", "OutperPA")))
Ystats <- within(Ystats, stat <- factor(stat, levels= c("BBperPA", "RperPA", "HperPA", "KperPA", "OutperPA")))
Zstats <- within(Zstats, stat <- factor(stat, levels= c("BBperPA", "RperPA", "HperPA", "KperPA", "OutperPA")))
Xplot <- ggplot(data = Xstats, aes(x = stat, y = value)) + ggtitle("Player X") + ylim(0,1) +
  geom_bar(stat = "identity", fill = "mediumpurple4", colour = "black") + theme(axis.title.x = element_blank(), 
      axis.title.y = element_blank(), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 10))
Yplot <- ggplot(data = Ystats, aes(x = stat, y = value)) + ggtitle("Player Y") + ylim(0,1) +
  geom_bar(stat = "identity", fill = "mediumpurple4", colour = "black") + theme(axis.title.x = element_blank(), 
      axis.title.y = element_blank(), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 10))
Zplot <- ggplot(data = Zstats, aes(x = stat, y = value)) + ggtitle("Player Z") + ylim(0,1) +
  geom_bar(stat = "identity", fill = "mediumpurple4", colour = "black") + theme(axis.title.x = element_blank(), 
      axis.title.y = element_blank(), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 10))
grid.arrange(Xplot, Yplot, Zplot, ncol=3)

# Calculate Game Score for each pitcher
GameScore <- function(x){
# Game score = Start at 50 then add/subtract...
# + 1 point for each out 
# + 2 points for each inning after 4th 
# + 1 for each strikeout 
# - 2 for each hit 
# - 3 for each run
# - 1 for each walk
  mydata5 <- filter(alldata, Pitcher == x)
  K <- sum(mydata5$K)
  H <- sum(mydata5$Hit)
  R <- sum(mydata5$RunsScored)
  BB <- sum(mydata5$BB)
  Outs <- sum(mydata5$OutsOnPlay) + K
  Inn4Plus <- (Outs - 12)/3
  GSc <- 50 + Outs + (2*Inn4Plus) + K - (2*H) - (3*R) - BB
  GSc
}   

GScX <- round(GameScore("Pitcher X"), 2)
GScY <- round(GameScore("Pitcher Y"), 2)
GScZ <- round(GameScore("Pitcher Z"), 2)

GScTbl <- data.frame(rbind(GScX, GScY, GScZ), row.names = c("Pitcher X", "Pitcher Y", "Pitcher Z"))

dev.off()
g5 <- tableGrob(GScTbl, cols = c("Game Score"))
grid.draw(g5)

# END