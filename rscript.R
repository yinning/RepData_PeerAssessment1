library(knitr)
library(dplyr)
library(ggplot2)

opts_chunk$set(echo=TRUE, results="hold")

setwd("D:/Dropbox/Coursera/01 Data Science/05 Reproducible Research/RepData_PeerAssessment1")

## Loading and preprocessing the data
act <- read.csv("./activity/activity.csv")
act$date <- as.Date(act$date)
act$steps <- as.numeric(act$steps)
str(act)

## What is mean total number of steps taken per day?

#Calculate the total number of steps taken per day
act_sumByDay <- act %>% na.omit() %>% group_by(date) %>% summarise(totalsteps = sum(steps))


#Make a histogram of the total number of steps taken each day

histPlot <- ggplot(act_sumByDay, aes(x=totalsteps)) + geom_histogram(aes(fill = ..count..), binwidth=2000) +
            theme_bw() +
            labs(x="Total number of steps per day", y="Frequency") +
            ggtitle("Histogram of total number of steps taken each day") +
            theme(plot.title = element_text(face="bold"))


print(histPlot)

#Calculate and report the mean and median of the total number of steps taken per day
meanSteps <- mean(act_sumByDay$totalsteps)
medianSteps <- median(act_sumByDay$totalsteps)
meanSteps
medianSteps

##What is the average daily activity pattern?
act_avgDailyActivity <- act %>% group_by(interval) %>% summarise(meansteps = mean(steps, na.rm=TRUE))

#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

with(act_avgDailyActivity, plot(interval, 
                                meansteps, 
                                type="l", 
                                main="Average number of steps taken averaged across all days", 
                                ylab="Average number of steps", 
                                col=2))

#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
act_avgDailyActivity[which.max(act_avgDailyActivity$meansteps),]$interval

##Imputing missing values

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(act$steps))

#2. strategy for filling in all of the missing values in the dataset
impute <- act %>% 
    left_join(act_avgDailyActivity, by="interval") %>% 
    mutate(steps=ifelse(is.na(steps), meansteps, steps))

impute <- impute[,-4]

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

impute_sumByDay <- impute %>% group_by(date) %>% summarise(totalsteps = sum(steps))

imputeHistPlot <- ggplot(impute_sumByDay, aes(x=totalsteps)) + geom_histogram(aes(fill = ..count..), binwidth=2000) +
    theme_bw() +
    labs(x="Total number of steps per day", y="Frequency") +
    ggtitle("Histogram of total number of steps taken each day with imputed data") +
    theme(plot.title = element_text(face="bold"))

print(imputeHistPlot)

imputeMeanSteps <- mean(impute_sumByDay$totalsteps)
imputeMedianSteps <- median(impute_sumByDay$totalsteps)

imputeMeanSteps
imputeMedianSteps

meanSteps - imputeMeanSteps
medianSteps - imputeMedianSteps 

## Are there differences in activity patterns between weekdays and weekends?

#1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
impute <- impute %>% 
    mutate(day=ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
impute$day <- as.factor(impute$day)

#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

impute_weekday <- impute %>% group_by(interval,day) %>% summarise(meansteps=mean(steps))

qplot(interval, meansteps, data=impute_weekday, geom=c("line"), main="Average number of steps taken", xlab="Interval", ylab="No. of steps") + 
    facet_wrap(~day, ncol=1) + theme_bw()
