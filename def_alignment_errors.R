mydata <- read.csv("savant_data_errors.csv", header = T)
mydata$hit_distance_sc[(mydata$hit_distance_sc == "null")] <- "0"
mydata$hit_distance_sc <- as.numeric(as.character(mydata$hit_distance_sc))
#mydata$hit_location <- as.factor(mydata$hit_location)
mydata$hit_location <- as.numeric(mydata$hit_location)

library(ggplot2)
ggplot(data = mydata, aes(x = launch_speed, y = hit_distance_sc, group = hit_location, 
                          colour = hit_location)) + geom_point()

ifdata <- mydata[mydata$hit_location %in% c(3,4,5,6),]
ggplot(data = ifdata, aes(x = launch_speed, y = hit_distance_sc, group = if_fielding_alignment, 
                          colour = if_fielding_alignment)) + geom_point()
ggplot(data = ifdata, aes(x = launch_speed, group = if_fielding_alignment, 
                          colour = if_fielding_alignment)) + geom_histogram(binwidth = 5)
ggplot(data = ifdata, aes(x = launch_speed, group = if_fielding_alignment, 
                          colour = if_fielding_alignment)) + geom_density()

ofdata <- mydata[mydata$hit_location %in% c(7,8,9),]
ggplot(data = ofdata, aes(x = launch_speed, y = hit_distance_sc, group = if_fielding_alignment, 
                          colour = if_fielding_alignment)) + geom_point()
