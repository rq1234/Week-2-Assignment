setwd("C:\\Users\\yeoch\\OneDrive - Hewlett-Packard\\Course2\\Data Science\\John Hopkins Data Science\\05 Reproducible Research\\Week 1")

#########################################################################################################
## Read in the data
#########################################################################################################
filename <- "activity.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, filename)
  unzip(filename) 
}
activity <- read.csv("activity.csv")
str(activity)

#########################################################################################################
## Mean total number of steps per day
#########################################################################################################
stepsDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
hist(stepsDay, main="Daily No. of Steps", xlab="Steps", ylab="Frequency")

mean.stepsDay <- mean(stepsDay)
mean.stepsDay

median.stepsDay <- median(stepsDay)
median.stepsDay

#########################################################################################################
## Average daily activity pattern
#########################################################################################################
mean.stepsInterval <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
plot(row.names(mean.stepsInterval),mean.stepsInterval,
     type="l",
     xlab="5-minute Intervals",
     ylab="Avg No. of Steps",
     main="Avg Daily No. of Steps @ 5-minute Intervals")

mean.stepsInterval[which.max(mean.stepsInterval)]

#########################################################################################################
## Imputing missing values
#########################################################################################################
sum(is.na(activity))

activity2 <- activity
NAs <- is.na(activity2$steps)
activity2$steps[NAs] <- mean.stepsInterval[as.character(activity2$interval[NAs])]

steps2 <- tapply(activity2$steps, activity2$date, sum, na.rm=TRUE)
hist(steps2, main="Daily No. of Steps", xlab="Steps", ylab="Frequency")

mean.steps2 <- mean(steps2)
mean.steps2

median.steps2 <- median(steps2)
median.steps2

#########################################################################################################
## Activity patterns between weekdays and weekends
#########################################################################################################
# Create a new factor variable in the dataset with two levels - 
# "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

# Create a new dataframe where missing values were imputed
activity3 <- activity2

# Identify records which are on weekends
weekend <- weekdays(as.Date(activity3$date)) %in% c("Saturday", "Sunday")

# Set up a new column called day and populate it with weekday
activity3$day <- "weekday"

# Replace weekday with weekend if it falls on a Saturday or Sunday
activity3$day[weekend==TRUE] <- "weekend"

# Convert day to a factor variable
activity3$day <- as.factor(activity3$day)

str(activity3)
head(activity3)

#  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
steps3 <- aggregate(steps ~ interval+day, activity3, mean)
head(steps3)

library(ggplot2)
g <- ggplot(steps3, aes(interval, steps, color=day))
g + geom_line() + labs(title = "Avg Daily Steps by Week Type", x="Interval", y="Avg No. of Steps") +
  facet_wrap(~day, ncol=1, nrow=2)
print(plot)
