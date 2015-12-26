# Reproducible Research - Project 1 Due August 16.

# Step 1: Load and Process Data
setwd("/Users/gamercier/git/RepData_PeerAssessment1")
data <- read.csv(unz("activity.zip","activity.csv"),stringsAsFactors=FALSE)
# data$interval <- factor(data$interval)
# data$date <- strptime(data$date,"%Y-%m-%d")

library(dplyr)
# Step 2: What is the mean total number of steps taken per day?
by_days <- group_by(data,date)
sum_by_days <- summarise(by_days,sum(steps,na.rm=TRUE))

# keeping days with no data set to zero
hist(sum_by_days$sum,breaks=10,main="Steps in a day\nDistribution",
     xlab="Number of steps in a day")
steps_in_day <- summary(sum_by_days$sum)
print(paste("Average:",steps_in_day["Mean"]))
print(paste("Median:",steps_in_day["Median"]))

# excluding days with no data, i.e. 0 steps
# non_zero_sum_by_days <- sum_by_days[sum_by_days$sum !=0,]

# Step 3: What is the average daily activity pattern?
by_interval <- group_by(data,interval)
mean_by_interval <- summarise(by_interval,mean(steps,na.rm=TRUE))

plot(mean_by_interval$interval,mean_by_interval$mean,type="l",
     main="Number Steps in An Interval\nAveraged over Days",
     xlab="Interval ID",ylab="Average Number of Steps")

# Step 4: Imputing Missing Values
num_missing_data <- sum(is.na(data$steps))

# Duplicating the data
clean_data <- data.frame(data)

# Filling data using the mean value for the interval
for (row in seq(nrow(clean_data))){
    if (is.na(clean_data$steps[row])){
        interval <- clean_data$interval[row]
        pick_mean <- mean_by_interval$interval == interval
        clean_data$steps[row] <- mean_by_interval$mean[pick_mean]
    }
}

# Repeating histogram and averages with imputed data
by_days <- group_by(clean_data,date)
sum_by_days <- summarise(by_days,sum(steps,na.rm=TRUE))

hist(sum_by_days$sum,breaks=10,main="Steps in a day\nDistribution with imputed Data",
     xlab="Number of steps in a day")
steps_in_day <- summary(sum_by_days$sum)
print(paste("Average:",steps_in_day["Mean"]))
print(paste("Median:",steps_in_day["Median"]))

# Step 5: Are there differences between activity patterns in weekdays and weekends?

what_day <- function(day){
    if(day %in% c("Saturday","Sunday")) return("Weekend")
    return("Weekday")
}
clean_data$what.day <- factor(sapply(weekdays(strptime(clean_data$date,"%Y-%m-%d")),
                                     what_day))

by_interval_what.day <- group_by(clean_data,interval,what.day)
avgs_interval_what.day <- summarise(by_interval_what.day,mean(steps))

library(lattice)

# convert the group_df into a dataframe that the plot function can take
data_as_df <- data.frame(avgs_interval_what.day)
# new names: interval, what.day, mean.steps.
g <- xyplot(mean.steps. ~ interval | what.day, data=data_as_df,
            type="l",layout=c(1,2), xlab = "Interval ID",
            ylab="Mean Number of Steps per Day")
print(g)
