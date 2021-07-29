---
title: "PA1_template.Rmd"
author: "Vu Tuan Manh"
date: "7/28/2021"
output: html_document
---



## I. Loading and preprocessing the data

### 1.1. Load the data


```r
# Load data in to data frame
activityData = read.csv("Data/activity.csv", header = TRUE)
# Add libraries
library(ggplot2)
library(dplyr)
```

## II. Calculation and plot

<!-- Ignore the missing values in the dataset. -->

### 2.1 Calculate the total number of steps taken per day


```r
stepPerDay <- aggregate(activityData$steps, list(activityData$date), FUN=sum)
colnames(stepPerDay) <- c("Date", "Steps")
```

Information about data


```r
str(stepPerDay)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ Date : chr  "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
##  $ Steps: int  NA 126 11352 12116 13294 15420 11015 NA 12811 9900 ...
```

```r
head(stepPerDay)
```

```
##         Date Steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

### 2.2 Make a histogram of the total number of steps taken each day

```r
g <- ggplot(stepPerDay, mapping=aes(x=Steps,label=..count..))
g+geom_histogram(boundary=0, binwidth=2500, col="black", fill="lightblue")+
    ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency by day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

### 2.3 Calculate and report the mean and median of the total number of steps taken per day

```r
# Mean
mean(stepPerDay$Steps, na.rm=TRUE)
```

```
## [1] 10766.19
```


```r
# Median
median(stepPerDay$Steps, na.rm=TRUE)
```

```
## [1] 10765
```

## III. What is the average daily activity pattern?
### 3.1 Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
# Calculate average steps by interval
groupData <- group_by(activityData,interval)
StepsPerInterval <- summarize(groupData, Steps = mean(steps, na.rm = TRUE))
colnames(StepsPerInterval) <- c("interval", "steps")

# Another option for calculate steps per interval
# StepsPerInterval <- aggregate(steps~interval,data=activityData,FUN=mean,na.action=na.omit)

# draw the line plot
g <- ggplot(StepsPerInterval, aes(interval, steps))
g + geom_line(col="brown")+ggtitle("Average steps per time interval")+
    xlab("Interval")+ylab("Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

### 3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
subset(StepsPerInterval, steps==max(StepsPerInterval$steps))
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```

## IV. Imputing missing values
### 4.1 Calculate and report the total number of missing values in the dataset

```r
# Number of rows with N/A value 
nrow(subset(activityData,is.na(activityData)))
```

```
## [1] 2304
```

### 4.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
### We will fill missing value by using the mean steps in each interval
### 4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
# clone data frame
activityDataComplete <- activityData
# Matching and update missing steps with mean value of appropriate interval
activityDataComplete$steps <- ifelse(is.na(activityDataComplete$steps),
    round(StepsPerInterval$steps[match(activityDataComplete$interval,
    StepsPerInterval$interval)],0), activityDataComplete$steps)
```

### 4.4 Make a histogram of the total number of steps taken each day and Calculate 

```r
# Calculate new mean step per day
stepPerDayNew <- aggregate(activityDataComplete$steps, list(activityDataComplete$date), FUN=sum)
colnames(stepPerDayNew) <- c("Date", "Steps")
# draw the line histogram for new step per day data (after fill missing value)
g <- ggplot(stepPerDayNew, mapping=aes(x=Steps,label=..count..))
g+geom_histogram(boundary=0, binwidth=2500, col="black", fill="lightblue")+
    ggtitle("Histogram of steps per day (no missing value)")+xlab("Steps")+ylab("Frequency by day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

### And report the Mean and Median total number of steps taken per day

```r
# Calculate and report the mean and median total number of steps taken per day
# Mean steps per day (no missing value)
mean(stepPerDayNew$Steps)
```

```
## [1] 10765.64
```

```r
# Mean steps per day (with missing value)
mean(stepPerDay$Steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
# Median steps per day (no missing value)
median(stepPerDayNew$Steps)
```

```
## [1] 10762
```

```r
# Mean steps per day (with missing value)
median(stepPerDay$Steps, na.rm=TRUE)
```

```
## [1] 10765
```

### Compare total steps per day after imputing missing value

```r
totalstepPerDay <- sum(stepPerDay$Steps, na.rm=TRUE)
totalstepPerDayNew <- sum(stepPerDayNew$Steps)
paste("Total steps (with missing value): ",totalstepPerDay," steps",sep="")
```

```
## [1] "Total steps (with missing value): 570608 steps"
```

```r
paste("Total steps (no missing value): ",totalstepPerDayNew," steps",sep="")
```

```
## [1] "Total steps (no missing value): 656704 steps"
```

```r
paste("Deviation of steps (after - before imputing: ", totalstepPerDayNew- totalstepPerDay, sep="")
```

```
## [1] "Deviation of steps (after - before imputing: 86096"
```

## V.Are there differences in activity patterns between weekdays and weekends?
### 5.1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```r
# Convert to date format
activityData$date <- as.Date(activityData$date, format= "%Y-%M-%d")
# Create weekday variable
activityData$weekday <- weekdays(activityData$date)
# Create day type for weekday and weekend
activityData$dayType <- ifelse(activityData$weekday=='Saturday' | 
    activityData$weekday=='Sunday', 'weekend','weekday')
# preview head data
head(activityData)
```

```
##   steps       date interval weekday dayType
## 1    NA 2012-07-01        0  Sunday weekend
## 2    NA 2012-07-01        5  Sunday weekend
## 3    NA 2012-07-01       10  Sunday weekend
## 4    NA 2012-07-01       15  Sunday weekend
## 5    NA 2012-07-01       20  Sunday weekend
## 6    NA 2012-07-01       25  Sunday weekend
```

### 5.2 Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
# Create data frame with steps per time interval for weekday and weekend
stepPerTimeDayTye <- aggregate(steps~interval+dayType,data=activityData,FUN=mean,na.action=na.omit)
# draw the line plot
g <- ggplot(stepPerTimeDayTye, aes(interval, steps))
g + geom_line(col="blue")+ggtitle("Average steps per time interval: weekdays vs. weekends")+
    xlab("Interval")+ylab("Steps")+
    theme(plot.title = element_text(face="bold", size=12))+
    facet_grid(dayType ~ .)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)







