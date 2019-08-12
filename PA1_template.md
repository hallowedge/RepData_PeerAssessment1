---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Load the data (i.e. read.csv())


```r
thedata <- read.csv(unzip('activity.zip', 'activity.csv'), sep=",", header = TRUE)
```

Process/transform the data (if necessary) into a format suitable for your analysis.

```r
dailysteps <- by(thedata['steps'], thedata$date, colSums)
intervalstepsavg <- aggregate(list(steps = thedata$steps), list(interval = thedata$interval), mean, na.rm=TRUE)
```


## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day 

```r
hist(dailysteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day

```r
mean(dailysteps, na.rm=TRUE)
```

```
## [1] 10766.19
```


```r
median(dailysteps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis): 

```r
plot(x=intervalstepsavg$interval, y=intervalstepsavg$steps, type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalstepsavg[intervalstepsavg$steps==max(intervalstepsavg$steps),1]
```

```
## [1] 835
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(thedata$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We will be using the average for the particular 5 minute interval to replace NA values. If no average exists, we will replace it with 0.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newdata <- thedata
for (i in 1:nrow(newdata)) {
  if(is.na(newdata[i,1])) {
    newdata[i,1] <- mean(thedata[thedata$interval==newdata[i,3],1], na.rm=TRUE)
  }
  if(is.na(newdata[i,1])) newdata[i,1] <- 0
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newdaily <- by(newdata['steps'], newdata$date, colSums)
hist(newdaily)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(newdaily)
```

```
## [1] 10766.19
```

```r
median(newdaily)
```

```
## [1] 10766.19
```

The impact of replacing as I did was to move the median closer to the actual mean value. This is because I essentially created additional full day values matching the mean daily value (many of the NA records were complete days).

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
newdata$typeofday <- factor(ifelse(weekdays(as.Date(newdata$date), abbreviate = TRUE)%in%c('Sat', 'Sun'), 'weekend', 'weekday'))
```

Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
newintervalstepsavg <- aggregate(list(steps = newdata$steps), list(interval = newdata$interval, typeofday = newdata$typeofday), mean)
xyplot(steps~interval | typeofday , data = newintervalstepsavg, layout = (c(1,2)), type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
