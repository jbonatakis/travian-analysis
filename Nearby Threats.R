rm(list=ls())
########### SETTINGS #################################
# Set x and y to coordinates of centroid village     
x <- 9                                            
y <- -68
radius <- 10                                       

# Label for centroid                                 
lab <- "Non *FU"                                     
centroid <- data.frame('x' = x, 'y' = y)             
######################################################

### Load libraries to be used later
library(ggplot2)

# Set the working directory (will need to change the path specific to where you downloaded the file on your machine)
setwd('/users/duncanmcconnell/Documents/Travian/US1 STARFU/Nearby Threats')

# Import file with no cleaning
dat <- read.csv("map.sql", sep=',', fill=TRUE, quote='', col.names = c('id', 'x', 'y', 'tid', 'vid', 'villageName', 'uid', 'playerName', 'aid', 'allianceName', 'population'))

# Remove SQL syntax
dat$villageName <- gsub("'", "", dat$villageName, fixed = TRUE)
dat$playerName <- gsub("'", "", dat$playerName, fixed = TRUE)
dat$allianceName <- gsub("'", "", dat$allianceName, fixed = TRUE)

dat$x <- as.numeric(levels(dat$x))[dat$x]
dat$population <- as.numeric(dat$population)

# Remove useless variables
dat$id <- NULL
dat$tid <- NULL
dat$vid <- NULL
dat$uid <- NULL
dat$aid <- NULL

FU <- subset(dat, allianceName == 'SUSFU' | allianceName == 'SNAFU')
dat <- subset(dat, allianceName != 'SUSFU' & allianceName != 'SNAFU' & playerName != 'Natars')
dat$dist <- 1000

for(i in 1:nrow(FU)){
  for(j in 1:nrow(dat)) {
    temp <- c(dat$dist[j], sqrt((dat$x[j] - FU$x[i])^2 + (dat$y[j] - FU$y[i])^2))
    dat$dist[j] <- temp[which.min(c(dat$dist[j], sqrt((dat$x[j] - FU$x[i])^2 + (dat$y[j] - FU$y[i])^2)))]
    print(paste(i,j,sep='   '))
  }
}

meanies <- subset(dat, dist < 10 & allianceName != 'SUSFU' & allianceName != 'SNAFU')

ggplot(data=meanies, aes(x, y)) + 
  geom_point(size=4, aes(color = allianceName))

write.csv(meanies, paste("/users/duncanmcconnell/Documents/Travian/US1 STARFU/Nearby Threats/",lab, " ", Sys.Date(), ".csv", sep = ""))

###################################################################################################

