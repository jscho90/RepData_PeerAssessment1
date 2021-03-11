---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Reproducible Research Course Project 1

*Junsang Cho  
March 11, 2021*

  
## Loading and preprocessing the data

**1. Load the data (i.e. read.csv())  
2. Process/transform the data (if necessary) into a format suitable for your analysis**

```r
knitr::opts_chunk$set(warning=FALSE)
```


```r
library(ggplot2)
activity <- read.csv("activity.csv")
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity, weekday)
summary(activity)
```

```
##      steps             date               interval        weekday         
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
##  NA's   :2304
```

  
## What is mean total number of steps taken per day?

**1. Calculate the total number of steps taken per day  
2. Make a histogram of the total number of steps taken each day**

```r
total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(total_steps) <- c("date", "steps")
hist(total_steps$steps, main = "Total number of steps per day", xlab = "Total steps per day", ylim = c(0,20), breaks = seq(0,25000, by=2500), col = "darkorange")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


**3. Calculate and report the mean and median total number of steps taken per day**

```r
mean(total_steps$steps)
```

```
## [1] 9354.23
```

```r
median(total_steps$steps)
```

```
## [1] 10395
```

  
## What is the average daily activity pattern?

**1. Make a time series plot (i.e. type = "l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```r
interval_average <- aggregate(activity$steps, by=list(activity$interval), FUN = mean, na.rm = TRUE)
names(interval_average) <- c("interval", "mean")
with(interval_average, plot(interval, mean, type = "l", main = "Average number of steps by interval", xlab = "Interval", ylab = "Average number of steps", col = "darkgreen", lwd = 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
max_steps <- max(interval_average$mean)
interval_average[interval_average$mean == max_steps, ]
```

```
##     interval     mean
## 104      835 206.1698
```

  
## Imputing missing values

**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

```r
imputed_data <- interval_average$mean[match(activity$interval, interval_average$interval)]
```

**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**

```r
imputed_activity <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_data, no = activity$steps))
total_steps_imputed <- aggregate(steps ~ date, imputed_activity, sum)
names(total_steps_imputed) <- c("date", "steps")
```

**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

```r
hist(total_steps_imputed$steps, main = "Total number of steps per day (missing data imputed)", xlab = "Total steps per day", ylim = c(0,30), breaks = seq(0,25000, by=2500), col = "darkorange")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


```r
mean(total_steps_imputed$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_imputed$steps)
```

```
## [1] 10766.19
```
  
## Are there differences in activity patterns between weekdays and weekends?

**1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**

```r
activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
                {y <- "Weekend"} else
                {y <- "Weekday"}
                y
        })
```

**2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.**

```r
activity_datetype <- aggregate(steps ~ interval + datetype, activity, mean, na.rm = TRUE)
ggplot(activity_datetype, aes(x = interval, y = steps, color = datetype)) +
        geom_line() +
        facet_wrap(~datetype, nrow = 2) +
        xlab("Interval") +
        ylab("Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

