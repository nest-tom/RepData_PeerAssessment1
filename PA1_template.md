---
title: "ReproducibleResearch_PeerAssessment_1"
output: html_document
---

requires the [activity_data.csv file] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip), unzipped into your working directory
load the activity_data into a data.frame object, excluding rows with NA values


```r
setwd("/Users/tlichtenberg/datasciencecoursera/RepData_PeerAssessment1")
activity_data <- read.csv("activity.csv")
activity_data$date <- as.Date(activity_data$date, format= "%Y-%m-%d")
df <- activity_data[complete.cases(activity_data),]
```

calculate the sums of steps per day


```r
steps_per_day <- aggregate(steps ~ date, data=df, FUN = sum)
```

show a histogram of steps per day


```r
hist(steps_per_day$steps, col='blue', breaks=seq(0,25000,2000), 
     main = "Histogram of Steps per Day", xlab="Total Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

calculate the mean and median of steps per day


```r
mean_steps   <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)
```

the mean is 1.0766189 &times; 10<sup>4</sup> and the median is 10765

show the average daily activity pattern


```r
intervals <- aggregate(steps ~ interval, data=df, FUN = mean)
plot(intervals$interval, intervals$steps, type="l", 
     ylab="Average Steps per Day", xlab="5 minute Interval",
     main="Average Daily Activity Pattern", col="blue") 
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

caulculate which time interval contains the maximum number of steps


```r
max_index <- which.max(intervals$steps)
max_interval <- intervals$interval[max_index]
```

the five-minute interval with the most steps is interval # 835 

using the original data, replace the missing values with the daily average


```r
missing_values_indices  <- which(is.na(activity_data$steps))
num_missing_values <- length(missing_values_indices)
activity_data2 <- activity_data
activity_data2$steps[missing_values_indices] <- sapply(
                            activity_data2[missing_values_indices,"interval"], 
                            function(a) {
                                index <- which(intervals[,"interval"]==a)
                                intervals[index,"steps"]
                            } ) 
```

make a histogram of the data with replaced missing values


```r
steps_per_day2 <- aggregate(steps ~ date, data=activity_data2, FUN = sum)
hist(steps_per_day2$steps, col='blue',breaks=seq(0,25000,2000), 
     main = "Histogram of Total Steps per Day", xlab="Total Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

calculate the mean and median of steps per day of the new data set


```r
mean_steps2   <- mean(steps_per_day2$steps)
median_steps2 <- median(steps_per_day2$steps)
```

the mean is 1.0766189 &times; 10<sup>4</sup> and the median is 1.0766189 &times; 10<sup>4</sup>
the mean remains the same, but the median is slightly higher.

split the data between weekdays and weekends to look for patterns there


```r
library(timeDate)
activity_weekday <- activity_data2
activity_weekday$weekday   <- as.factor(weekdays(activity_weekday$date))
is_weekday <- gsub("TRUE", "weekday", as.character(isWeekday(activity_weekday$date)))
activity_weekday$isweekday <- as.factor(gsub("FALSE", "weekend", is_weekday))
```

plot the data


```r
library(lattice)
intervals_weekday <- aggregate(steps ~ interval + isweekday, data=activity_weekday, FUN = mean)
xyplot(steps ~ interval | isweekday, data = intervals_weekday, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
