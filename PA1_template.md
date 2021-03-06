# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
unzip ("activity.zip", exdir = "./")
data <- read.csv("./activity.csv", header = TRUE)
```
Here ***data*** is a 17568x3 dimensional data frame.  
The variables included in this dataset are:  

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

## What is mean total number of steps taken per day?
To calculate the total number of steps I grouped the dataset by date and calculated sum over each date.
By plotting histogram I find out the frequency of 'total steps taken each day'.

```r
library(plyr)
TotalSteps <- ddply(data, .(date), function(x) sum(x[, "steps"], na.rm = TRUE))
hist(TotalSteps$V1, xlab = "Steps", ylab = "Frequency", main = "Histogram of total steps taken each day", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

###Mean and median

```r
mean(TotalSteps$V1)
```

```
## [1] 9354.23
```

```r
median(TotalSteps$V1)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
To calculate the daily activity pattern I grouped the dataset by interval and calculated mean over each interval.  
***Activity pattern*** is a data frame of dimensions 288x2.  
The variables included in this dataset are: 

* V1: Mean of number of steps taken in a 5-minute interval  
* interval: Identifier for the 5-minute interval in which measurement was taken


```r
ActivityPattern <- ddply(data, .(interval), function(x) mean(x[, "steps"], na.rm = TRUE))
library(ggplot2)
```

###Plot

```r
qplot(interval, V1, data = ActivityPattern, geom = "line", xlab = "Intervals", ylab = "Steps", main = "Activity pattern of steps as per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

###5-minute interval with maximum steps

```r
ActivityPattern[which.max(ActivityPattern$V1), "interval"]
```

```
## [1] 835
```
## Imputing missing values
### Number of missing values

```r
MissingValuesLogicalVector <- is.na(data$steps)
NumberOfMissingValues <- length(which(MissingValuesLogicalVector))
NumberOfMissingValues
```

```
## [1] 2304
```
### Filling missing values
Missing values are imput by using the mean for the 5-minute interval  
Here ***dataNew*** is a 17568x3 dimensional data frame.   
The variables included in this dataset are: 

* steps: Number of steps taking in a 5-minute interval (missing values are filled using mean for the 5-minute interval )
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

***MeanSteps*** is a vector of length 17568 which holds the mean of the interval. One way to look at it is values of 288 intervals being repeated.  

The steps in dataNew are filled with this mean if they were NA earlier.

```r
dataNew <- data
dataNew$steps[which(MissingValuesLogicalVector)] <- 0
MeanSteps <- ActivityPattern[match(dataNew$interval, ActivityPattern$interval), "V1"]
dataNew$steps <- dataNew$steps + as.numeric(MissingValuesLogicalVector)*MeanSteps
```
***TotalStepsNew*** is a data frame similar to TotalSteps. This is calculated after filling NA values.

```r
TotalStepsNew <- ddply(dataNew, .(date), function(x) sum(x[, "steps"], na.rm = TRUE))
hist(TotalStepsNew$V1, xlab = "Steps", ylab = "Frequency", main = "Histogram of total steps taken each day", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

###Mean and median
As you can see because of filling NA values both mean and median increases.

```r
mean(TotalStepsNew$V1)
```

```
## [1] 10766.19
```

```r
median(TotalStepsNew$V1)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
Modified dataNew to have one more column *wDay* which has 2 labels weekend and weekday.

```r
dataNew$date <- as.Date(dataNew$date)
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
dataNew$wDay <- factor((weekdays(dataNew$date) %in% weekdays), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```
From this we get ***ActivityPatternNew*** which is a 576x3 dimensional data frame.  
The variables included in this dataset are: 

* V1: Mean of number of steps taken in a 5-minute interval  
* interval: Identifier for the 5-minute interval in which measurement was taken
* wDay: Identifier for the weekends and weekdays

```r
ActivityPatternNew <- ddply(dataNew, .(interval, wDay), function(x) mean(x[, "steps"], na.rm = TRUE))
qplot(interval, V1, data = ActivityPatternNew, geom = "line", xlab = "Intervals", ylab = "Steps", facets = wDay~., main = "Activity pattern of steps as per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

The average activity on weekend seems to be more than average activity on weekdays. Though the peak is in weekdays which may relate to activity like going to office or jogging.

#The End. Thank you!
