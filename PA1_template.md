---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

The downloaded file repdata-data-activity.zip was unzipped into the working directory.


```r
unzip(zipfile = "activity.zip")
activity = read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

Rows with missing values were omitted.


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity_without_NA = filter (activity, !is.na(activity$steps))
steps_day = summarise (group_by(activity_without_NA, date),total = sum(steps))

hist(steps_day$total)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Mean total number of steps taken per day:

```r
mean(steps_day$total)
```

```
## [1] 10766
```

Median total number of steps taken per day:

```r
median(steps_day$total)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
steps_interval = summarise(group_by(activity_without_NA,interval), avg = mean(steps))

plot(steps_interval, type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Interval, that contains the maximum number of steps: 835

## Imputing missing values

Number of missing values: 

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

The missing values were replaced by the nearest previous value.


```r
activity_imputated = activity

last_value = 0
for (i in seq(from = 1, to = nrow(activity_imputated), by = 1)) {
  if  (is.na(activity_imputated[i,1])) 
    activity_imputated[i,1]=last_value
  else last_value = activity_imputated[i,1]
     
}

steps_day_imputated = summarise (group_by(activity_imputated, date),total = sum(steps))
hist(steps_day_imputated$total)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Mean total number of steps taken per day:

```r
mean(steps_day_imputated$total)
```

```
## [1] 9354
```

Median total number of steps taken per day:

```r
median(steps_day_imputated$total)
```

```
## [1] 10395
```

The values are different. They are lower, because a most of the imputed values are 0. The mean is more affected then the median.

## Are there differences in activity patterns between weekdays and weekends?

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
activity_imputated$WD = ifelse(test = weekdays(x = as.Date(activity_imputated$date), abbreviate = T) %in% c("Sun","Sat" ), yes = "weekend", no = "weekday")
 
library(lattice)
steps_interval_imputated = summarise(group_by(activity_imputated ,interval, WD), avg = mean(steps))
xyplot(avg~interval|WD, data=steps_interval_imputated, type = "l", strip = T, layout=(c(1,2)))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
