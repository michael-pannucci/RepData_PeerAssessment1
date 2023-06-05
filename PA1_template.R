################################################################################
# JHU Reproducible Research - Course Project 1
# Personal Activity Monitoring Device Study
################################################################################

# Preliminaries

library(dplyr)
library(ggplot2)
setwd("/Users/mpannucc/Documents/R/DS300/Course Project 1")

# 1-1: Read the data

filename <- "activity.zip"

if(!file.exists(filename)) {
    dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(dataURL, filename, method = "curl")
}

if(!file.exists("activity")) {
    unzip(filename)
}

activity <- read.csv("activity.csv", header = TRUE)

# 1-2: Process the data

main <- activity %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"))

# 2-1: Calculate the total number of steps per day

total1 <- main %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps))

# 2-2: Make a histogram of the total number of steps per day

hist(total1$total_steps)

# 2-3: Calculate the mean and median of the total number of steps per day

mean_steps <- mean(total1$total_steps); mean_steps
median_steps <- median(total1$total_steps); median_steps

# 3-1: Make a time series plot of the 5-minute intervals and the average number
# of steps taken, averaged across all days

intervals <- main %>% 
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(average_steps = mean(steps))

plot(intervals$interval, intervals$average_steps, type = "l", 
     xlab = "5-Minute Interval", ylab = "Average # of Steps", 
     main = "Average Number of Steps Taken per Interval")

# 3-2: Identify the 5-minute interval that contains the maximum number of steps
# on average across all days in the dataset

intervals$interval[which.max(intervals$average_steps)]

# 4-1: Calculate and report the total number of missing values in the dataset

total_na <- sum(is.na(main$steps)) / nrow(main)
total_na

# 4-2/4-3: Impute the missing values with the mean for the given 5-minute interval

main2 <- inner_join(main, intervals, by = "interval") %>%
    mutate(steps2 = if_else(!is.na(steps), steps, average_steps))

total2 <- main2 %>%
    group_by(date) %>%
    summarize(total_steps2 = sum(steps2))

# 4-4: Make a histogram of the total number of steps per day after imputation

hist(total2$total_steps2)

mean_steps2 <- mean(total2$total_steps2); mean_steps2
median_steps2 <- median(total2$total_steps2); median_steps2

# 5-1: Create a factor variable in the dataset with two levels, "weekday" and
# "weekend"

main2 <- main2 %>%
    mutate(weekday = weekdays(date), 
           day = as.factor(if_else(weekday %in% c("Saturday", "Sunday"), 
                                   "Weekend", "Weekday")))


# 5-2: Make a panel plot containing a time series plot of the 5-minute interval
# and the average number of steps taken, averaged across all weekday days and 
# weekend days

final <- main2 %>%
    group_by(day, interval) %>%
    summarize(average_steps = mean(steps2))

plot <- ggplot(final, aes(x = interval, y = average_steps)) +
    geom_line(stat = "identity") +
    facet_wrap(day ~ ., nrow = 2) +
    labs(title = "Average Number of Steps per Interval by Weekday/Weekend") +
    labs(x = "5-Minute Interval", y = "Average Number of Steps") +
    theme_bw(base_family = "Avenir", base_size = 12)

plot