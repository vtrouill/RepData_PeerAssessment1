---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
The zip of the data  is present on the root directory
Unzip it and read the data

```r
unzip("activity.zip")
data<-read.csv("activity.csv")
```

Have a quick look at the data using summary

```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

And the str

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Looks good like : keep the data like that

## What is mean total number of steps taken per day?

In this section we keept the missing values.
Aggregate the the steps versus date and sum it to have a set of data with just the total number of steps

```r
totalSteps<-aggregate(steps~date,data=data,sum,na.rm=TRUE)
head(totalSteps,5)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
```

Make an histogram of the total number of steps per day

```r
hist(totalSteps$steps, xlab="Total Steps",  main =" Histogram of total number of steps per day")
```

![](PA1_files/figure-html/Histo-1.png)<!-- -->

Calculate the mean and the median value of the total number of steps.

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
Caculate the mean steps of daily activity

```r
AverageActivity<-aggregate(steps~interval,data=data,mean,na.rm=TRUE)
head(AverageActivity,5)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
```

Plot the pattern found

```r
plot(steps~interval,data=AverageActivity,type="l",main="Average Activity Pattern",xlab="activity by 5mn",ylab="Average steps")
```

![](PA1_files/figure-html/PlotAv-1.png)<!-- -->

Retrieve the interval which contain the max of mean step.

```r
Interval_MaxMeanSteps = AverageActivity[which.max(AverageActivity[, 2]), 1]
Interval_MaxMeanSteps
```

```
## [1] 835
```

## Imputing missing values

there are a number of days/intervals where there are missing values (coded as 𝙽�A).

The presence of missing days may introduce bias into some calculations or summaries of the data.

calculate the number of rows with NA

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
strategy to fill in NA value: 

create a new data set (data2) set with the NA replace by mean of t 5 mn (already computed in the previous question)

```r
Newdata = data
for (i in 1:nrow(Newdata)) {
  if (is.na(Newdata$steps[i])) {
    for (j in 1:nrow(AverageActivity)) {
      if (Newdata$interval[i] == AverageActivity[j, 1]) {
        Newdata$steps[i] = AverageActivity[j, 2]
      }
    } 
  }    
}
```
Verify the set is not conatining NA now

```r
sum(is.na(Newdata$steps))
```

```
## [1] 0
```

```r
head(Newdata,5)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
```

Recalculate the total steps by day, plot histogram, calculate mean and median

```r
NewtotalSteps<-aggregate(steps~date,data=Newdata,sum,na.rm=TRUE)

hist(NewtotalSteps$steps, xlab="Total Steps",  main =" Histogram of total number of steps per day")
```

![](PA1_files/figure-html/PlotasFirst-1.png)<!-- -->

```r
mean(NewtotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(NewtotalSteps$steps)
```

```
## [1] 10766.19
```
We obtained an histogram nearly smilar to the previous ( some value change but not the form of it)

we obtained the same mean as previous but the median value is now egal to the mean. This impact is  due by the replacement of the 2304 missing value by the 5-min interval obtained from the original dataset.

## Are there differences in activity patterns between weekdays and weekends?

fisrt we have to add a new column in the previous dataset (name day).

(local set to have english name of day to be reproduced by anyone.

```r
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
#Sys.setenv(LANG="en")
Newdata$day_of_week <- weekdays(as.Date(Newdata$date), abbreviate = TRUE)
head(Newdata) 
```

```
##       steps       date interval day_of_week
## 1 1.7169811 2012-10-01        0         Mon
## 2 0.3396226 2012-10-01        5         Mon
## 3 0.1320755 2012-10-01       10         Mon
## 4 0.1509434 2012-10-01       15         Mon
## 5 0.0754717 2012-10-01       20         Mon
## 6 2.0943396 2012-10-01       25         Mon
```

Assiociate day with day type (weekday or weekend) and add it to the dataset

```r
days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
day_types <- c(rep("weekday", 5), rep("weekend", 2))
days_of_week <- data.frame(days, day_types, stringsAsFactors = FALSE)
colnames(days_of_week) <- c("day_of_week", "day_type")
Newdata <- merge(Newdata, days_of_week, by = "day_of_week")
Newdata$day_type <- as.factor(Newdata$day_type)
head(Newdata)
```

```
##   day_of_week steps       date interval day_type
## 1         Fri     0 2012-10-05     2220  weekday
## 2         Fri     0 2012-11-16      430  weekday
## 3         Fri     0 2012-10-05     2235  weekday
## 4         Fri     0 2012-11-16      435  weekday
## 5         Fri     0 2012-10-05     2315  weekday
## 6         Fri     0 2012-11-16      445  weekday
```

use lattice for a similar look as the example

recalculate the daily activity pattern 

Check the average data frame

plot the two figure for weekdays and weekend

```r
library(lattice)

AverageActivity2=aggregate(steps~interval+day_type,Newdata,mean)
head(AverageActivity2,3)
```

```
##   interval day_type     steps
## 1        0  weekday 2.2511530
## 2        5  weekday 0.4452830
## 3       10  weekday 0.1731656
```

```r
xyplot(steps~interval|factor(day_types),data=AverageActivity,aspect=1/2,type="l",main="Average Activity Pattern",xlab="activity by 5mn",ylab="Average steps")
```

![](PA1_files/figure-html/2plots-1.png)<!-- -->
