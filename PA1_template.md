# Reproducible Research: Peer Assessment 1

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. The data 
consists of two months of data from an anonymous individual collected during 
the months of October and November, 2012 and include the number of steps taken 
in 5 minute intervals each day.

## Loading and preprocessing the data


```r
setwd("C:/Users/Cherie.freedom/RepData_PeerAssessment1")
dat <- read.csv(unzip("activity.zip"))
library(lubridate)
dat$date <- ymd(dat$date)
```

## What is mean total number of steps taken per day?


```r
library(plyr)
```

```
## 
## Attaching package: 'plyr'
## 
## The following object is masked from 'package:lubridate':
## 
##     here
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
dat1 <- dat[complete.cases(dat),]
results <- dat1 %>%
        group_by(date) %>%
        summarize(tot_stp = sum(steps))
hist(results$tot_stp, main="Total number of steps taken each day", xlab="")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean <- mean(results$tot_stp)
print("The average (mean) number of steps is:") 
```

```
## [1] "The average (mean) number of steps is:"
```

```r
mean 
```

```
## [1] 10766.19
```

```r
median <- median(results$tot_stp)
print("The median number of steps is:") 
```

```
## [1] "The median number of steps is:"
```

```r
median
```

```
## [1] 10765
```

## What is the average daily activity pattern?

This graph presents a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):

```r
dat2 <- dat1 %>%
        group_by(interval) %>%
        summarize(aver_stp= mean(steps))
plot(dat2, type="l", main = "the average number of steps taken, averaged across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
na <- is.na(dat)
sum(na)
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. 

Missing numbers will be subsituted by the the mean for that 5-minute interval of all days.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dat3 <- cbind(dat,dat2$aver_stp)
dat3 <- dat3[na,]
colnames(dat3) <- c("steps2", "date", "interval", "steps")
dat3 <- select(dat3, steps,date,interval)
dat4 <- dat[complete.cases(dat),]
dat5 <- rbind(dat3, dat4)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
results2 <- dat5 %>%
        group_by(date) %>%
        summarize(tot_stp = sum(steps))
hist(results2$tot_stp, main="Total number of steps taken each day", xlab="")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
mean2 <- mean(results2$tot_stp)
print("The average (mean) number of steps is:") 
```

```
## [1] "The average (mean) number of steps is:"
```

```r
mean2 
```

```
## [1] 10766.19
```

```r
median2 <- median(results2$tot_stp)
print("The median number of steps is:") 
```

```
## [1] "The median number of steps is:"
```

```r
median2
```

```
## [1] 10766.19
```

```r
print("The estimates from the 1st and 2nd part differ by the following %:")
```

```
## [1] "The estimates from the 1st and 2nd part differ by the following %:"
```

```r
(mean2-mean)/mean*100
```

```
## [1] 0
```

```r
(median2-median)/median *100
```

```
## [1] 0.01104207
```

## Are there differences in activity patterns between weekdays and weekends?


```r
library(timeDate)
```

```
## Warning: package 'timeDate' was built under R version 3.1.3
```

```r
results3 <- dat5 %>%
        mutate(day=wday(date, label=FALSE)) %>%
        mutate(type=isWeekday(date, wday = 1:5)) %>%
        group_by(type, interval) %>%
        summarize(mean(steps))

results3$type <- factor(results3$type, levels=c("TRUE", "FALSE"), labels=c("Weekday", "Weekend"))
colnames(results3) <- c("type", "interval", "steps")
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
p <- ggplot(results3, aes(interval, steps)) + geom_point() + facet_grid(type ~ .)
p + geom_line() + xlab("Interval") + ylab("Number of steps")            
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
