library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)

# Read in files and prep data
mydata <- read.csv("Analytics_Internship_2017_dataset.csv", header = T)
mydata <- filter(mydata, pitch_result != "BallIntentional")

mydata$Hit <- factor(mydata$event_result %in% c("Single", "Double", "Triple", "HomeRun"), 
                     labels = c("Out/Foul", "Hit"))
mydata$ball_or_strike <- factor(mydata$pitch_result %in% c("FoulBall", "InPlay", "StrikeCalled", "StrikeSwinging"), 
                     labels = c("Ball", "Strike"))
mydata$ball_or_strike01 <- factor(mydata$pitch_result %in% c("FoulBall", "InPlay", "StrikeCalled", "StrikeSwinging"), 
                                labels = c("0", "1"))
mydata$ball_or_strike01 <- as.numeric(as.character(mydata$ball_or_strike01))
mydata$swStr01 <- factor(mydata$pitch_result %in% c("StrikeSwinging"), labels = c("0", "1"))
mydata$swStr01 <- as.numeric(as.character(mydata$swStr01))
mydata$pitcher_id <- as.character(mydata$pitcher_id)

mydata1 <- filter(mydata, pitcher_id == "16053")
mydata2 <- filter(mydata, pitcher_id == "38704")


# Basic stats
aggregate(start_speed ~ pitcher_id, data = mydata, summary)
aggregate(spin_rate ~ pitcher_id, data = mydata, summary)
aggregate(break_z ~ pitcher_id, data = mydata, summary)
aggregate(break_x ~ pitcher_id, data = mydata, summary)

# Frequencies
summary(mydata1$pitch_result)
summary(mydata2$pitch_result)
summary(mydata1$hit_type)
summary(mydata2$hit_type)

# Strike rate
sum(mydata1$ball_or_strike01)/length(mydata1$pitch_number)
sum(mydata2$ball_or_strike01)/length(mydata2$pitch_number)

# Swinging strike rate
sum(mydata1$swStr01)/length(mydata1$pitch_number)
sum(mydata2$swStr01)/length(mydata2$pitch_number)

# First pitch strike rate
temp1 <- filter(mydata1, pitch_of_pa == 1)
sum(temp1$ball_or_strike01)/length(temp1$pitch_number)
temp2 <- filter(mydata2, pitch_of_pa == 1)
sum(temp2$ball_or_strike01)/length(temp2$pitch_number)


# Location
Kzone <- data.frame(x=c(-0.75, -0.75, 0.75, 0.75), y=c(1.5, 3.5, 3.5, 1.5))
plate <- data.frame(x=c(-0.75, -0.75, 0, 0.75, 0.75), y=c(0, 0.125, 0.25, 0.125, 0))
plot1 <- ggplot(show.legend = TRUE) + xlim(-3.75,3.75) + ylim(-0.75,4.5) +
  ggtitle("Strike zone plot for pitcher 16053 (pitcher's view)      ") + 
  geom_point(data = mydata1, mapping = aes(x = plate_x, y = plate_z, colour = ball_or_strike, 
                                           shape = pitch_result), size = 2.5) + 
  scale_colour_manual(values = c("firebrick1", "darkgreen")) + 
  theme(panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = c(0.9,0.15), legend.background = element_rect(fill="white", colour="black"))
plot1 <- plot1 + geom_polygon(data = Kzone, mapping = aes(x=x,y=y), 
                              fill = "orange", colour = "red", alpha = .2)
plot1 <- plot1 + geom_polygon(data = plate, mapping = aes(x=x,y=y), fill = NA, colour = "black")

mydata2$pitch_result <- factor(mydata2$pitch_result, levels = c("BallCalled", "BallIntentional", "FoulBall",
                                                                "InPlay", "StrikeCalled", "StrikeSwinging", "HitByPitch"))
plot2 <- ggplot(show.legend = TRUE) + xlim(-3.75,3.75) + ylim(-0.75,4.5) +
  ggtitle("Strike zone plot for pitcher 38704 (pitcher's view)       ") + 
  geom_point(data = mydata2, mapping = aes(x = plate_x, y = plate_z, colour = ball_or_strike, 
                                           shape = pitch_result), size = 2.5)  + 
  scale_colour_manual(values = c("firebrick1", "darkgreen")) + 
  theme(panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = c(0.9,0.15), legend.background = element_rect(fill="white", colour="black"))
plot2 <- plot2 + geom_polygon(data = Kzone, mapping = aes(x=x,y=y), 
                              fill = "orange", colour = "red", alpha = .2)
plot2 <- plot2 + geom_polygon(data = plate, mapping = aes(x=x,y=y), fill = NA, colour = "black")
grid.arrange(plot1, plot2, ncol=2)


# Release point
plot3 <- ggplot(show.legend = TRUE) + xlim(-5,5) + ylim(0,8) +
  ggtitle("Release point plot for pitcher 16053 (pitcher's view)       ") + 
  geom_point(data = mydata1, mapping = aes(x = release_x, y = release_z, colour = bat_side), size = 2) + 
  theme_bw() + theme(legend.position = c(0.9,0.15), legend.background = element_rect(fill="white", colour="black"))
plot4 <- ggplot(show.legend = TRUE) + xlim(-5,5) + ylim(0,8) +
  ggtitle("Release point plot for pitcher 38704 (pitcher's view)       ") + 
  geom_point(data = mydata2, mapping = aes(x = release_x, y = release_z, colour = bat_side), size = 2) + 
  theme_bw() + theme(legend.position = c(0.9,0.15), legend.background = element_rect(fill="white", colour="black"))
grid.arrange(plot3, plot4, ncol=2)


# Contact management
aggregate(exit_speed ~ pitcher_id, data = mydata, mean)
Bzone <- data.frame(x=c(98, 98, 116, 116), y=c(30, 26, 8, 50))
nBzone <- data.frame(x=c(96, 96, 116, 116), y=c(32, 24, 4, 54))
plot5 <- ggplot(show.legend = TRUE) + xlim(55,116) + ggtitle("Exit Speed vs Launch Angle by Pitcher       ") + 
  geom_point(data = mydata, mapping = aes(x = exit_speed, y = vert_exit_angle, 
                                          group = pitcher_id, colour = pitcher_id, shape = Hit), size = 3) + 
  theme(panel.grid.minor = element_blank(), legend.position = c(0.9,0.15), 
        legend.background = element_rect(fill="white", colour="black")) + 
  scale_colour_manual(values = c("darkcyan", "midnightblue"))
plot5 <- plot5 + geom_polygon(data = nBzone, mapping = aes(x=x,y=y), 
                              fill = "lightgoldenrod", colour = "red", alpha = .2)
plot5 <- plot5 + geom_polygon(data = Bzone, mapping = aes(x=x,y=y), 
                              fill = "orange", colour = "red", alpha = .2)
plot5 + annotate("text", x = 111, y = 33.5, label = "Barrel Zone", colour = "orange", size = 5.5) + 
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))



