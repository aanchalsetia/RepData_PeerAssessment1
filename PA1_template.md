---
title: "PA1_template"
author: "Aanchal Setia"
date: "26/07/2020"
output: 
  html_document: 
    keep_md: yes
---



## R Markdown

This is an R Markdown document. 
Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
setwd("RR")

file <- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "RR", method = "curl")

data1 <- read.csv("./activity.csv", header = F, sep = ",", skip = 1)

names(data1) <- c("steps", "date", "interval")
data1$date <- ymd(data1$date)
```

Total number of steps taken per day


```r
totalsteps <- group_by(data1, date) %>% summarise(totalsteps = sum(steps, na.rm = T))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(totalsteps$totalsteps, col = "blue", main = "Total number of steps taken per day", xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Mean and median of the total number of steps taken per day


```r
mean(totalsteps$totalsteps, na.rm = T)
```

```
## [1] 9354.23
```

```r
median(totalsteps$totalsteps, na.rm = T)
```

```
## [1] 10395
```

Average daily activity pattern


```r
stepsperinterval <- aggregate(steps~interval, data = data1, FUN = mean)

plot(steps~interval, data=stepsperinterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Max number of steps


```r
stepsperinterval[which.max(stepsperinterval$steps),]$interval
```

```
## [1] 835
```

Total number of missing values in the dataset


```r
sum(is.na(data1))
```

```
## [1] 2304
```

Filling in all of the missing values in the dataset by mean

```r
imputed_steps <- stepsperinterval[match(data1$interval, stepsperinterval$interval),]$steps
```

New dataset that is equal to the original dataset but with the missing data filled in


```r
activity_imputed <- transform(data1, steps = ifelse(is.na(data1$steps), yes = imputed_steps, no = data1$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")
```

Histogram of the total number of steps taken each day 

```r
hist(total_steps_imputed$daily_steps, col = "blue", main = "Total number of steps taken per day", xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Mean and median total number of steps taken per day

```r
mean(total_steps_imputed$daily_steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(total_steps_imputed$daily_steps, na.rm = T)
```

```
## [1] 10766.19
```
Mean remained the same but median changed a bit

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data1$day <- weekdays(data1$date)

data1$day <- ifelse(data1$day == "Saturday" | data1$day == "Sunday", yes = "Weekend", no = "Weekday")
```
Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
activity_by_date <- aggregate(steps~interval + day, data1, mean, na.rm = TRUE)
data1 <- transform(data1, day = factor(day))

g<- ggplot(activity_by_date, aes(x = interval , y = steps, color = day)) +
     geom_line() +
     labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
     facet_wrap(~day, ncol = 1, nrow=2)
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
