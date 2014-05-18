# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("repdata-data-activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```


## What is mean total number of steps taken per day?

```r
sumofdays <- aggregate(steps ~ date, data = data, FUN = sum)
barplot(sumofdays$steps, names.arg = sumofdays$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r

mean(sumofdays$steps)
```

```
## [1] 10766
```

```r
median(sumofdays$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?


```r
meansteps <- aggregate(steps ~ interval, data = data, FUN = mean)
plot(meansteps, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

meansteps$interval[which.max(meansteps$steps)]
```

```
## [1] 835
```




## Imputing missing values


```r
sum(is.na(data))
```

```
## [1] 2304
```

```r
data <- merge(data, meansteps, by = "interval", suffixes = c("", ".m"))
missing <- is.na(data$steps)
data$steps[missing] <- data$steps.m[missing]
data <- data[, c(1:3)]
sumsteps <- aggregate(steps ~ date, data = data, FUN = sum)
barplot(sumsteps$steps, names.arg = sumsteps$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(sumsteps$steps)
```

```
## [1] 10766
```

```r
median(sumsteps$steps)
```

```
## [1] 10766
```



## Are there differences in activity patterns between weekdays and weekends?


```r
weekday <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday", "Cumartesi", "Pazar")) {
        "weekend"
    } else {
        "weekday"
    }
}
data$daytype <- as.factor(sapply(data$date, weekday))

par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    stepfactor <- aggregate(steps ~ interval, data = data, subset = data$daytype == 
        type, FUN = mean)
    plot(stepfactor, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

