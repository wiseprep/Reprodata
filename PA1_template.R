# =================================================
# Loading and preprocessing the data
# =================================================
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")

# =================================================
# What is mean total number of steps taken per day?
# =================================================
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="Total number of steps taken per day")

mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)

# =================================================
# What is the average daily activity pattern?
# =================================================
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute Interval") +
  ylab("Average number of steps taken")

# the maximum number of steps
averages[which.max(averages$steps),]

# =================================================
# Imputing missing values
# =================================================
missing <- is.na(data$steps)
# the number of missing values
table(missing)

# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="Total number of steps taken each day")
mean(total.steps)
median(total.steps)

# =================================================
# Are there differences in activity patterns between weekdays and weekends?
# =================================================
filled.data$dateType <-  ifelse(as.POSIXlt(filled.data$date)$wday %in% c(0,6), 'weekend', 'weekday')

averagedfilled.data <- aggregate(steps ~ interval + dateType, data=filled.data, mean)
ggplot(averagedfilled.data, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(dateType ~ .) +
  xlab("5-minute interval") + 
  ylab("Average number of steps")
