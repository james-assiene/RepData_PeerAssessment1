# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
```


```r
df = read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```



## What is mean total number of steps taken per day?


```r
gdf = group_by(df, date)
summ = summarise(gdf, mean.steps = sum(steps, na.rm = TRUE))
print(summ)
```

```
## # A tibble: 61 x 2
##          date mean.steps
##        <date>      <dbl>
##  1 2012-10-01          0
##  2 2012-10-02        126
##  3 2012-10-03      11352
##  4 2012-10-04      12116
##  5 2012-10-05      13294
##  6 2012-10-06      15420
##  7 2012-10-07      11015
##  8 2012-10-08          0
##  9 2012-10-09      12811
## 10 2012-10-10       9900
## # ... with 51 more rows
```

```r
library(ggplot2)
qplot(mean.steps, data = summ)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


Mean of total number of steps taken per day

```r
mean(summ$mean.steps)
```

```
## [1] 9354.23
```


Median of total number of  steps taken per day

```r
median(summ$mean.steps)
```

```
## [1] 10395
```



## What is the average daily activity pattern?

```r
gdfTimeIntervals = group_by(df, interval)
summTimeIntervals = summarize(gdfTimeIntervals, avgSteps = mean(steps, na.rm = TRUE))
qplot(interval, avgSteps, data = summTimeIntervals, geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
summTimeIntervals[which.max(summTimeIntervals$avgSteps),]
```

```
## # A tibble: 1 x 2
##   interval avgSteps
##      <dbl>    <dbl>
## 1      835 206.1698
```



## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(df) - sum(complete.cases(df))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
library(Hmisc)
```

```
## Loading required package: grid
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```


Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
idf = df
idf$steps = impute(df$steps, fun=mean)
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
gdf = group_by(idf, date)
summ = dplyr::summarise(gdf, totalPerDay = sum(steps))
qplot(totalPerDay, data = summ)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Mean of total number of steps taken per day

```r
mean(summ$totalPerDay)
```

```
## [1] 10766.19
```


Median of total number of  steps taken per day

```r
median(summ$totalPerDay)
```

```
## [1] 10766.19
```

Yes there is a difference between these median/mean and those from the first part


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
idf$weekdayType = ifelse(as.POSIXlt(idf$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
gdf = group_by(idf, interval, weekdayType)
summ = dplyr::summarise(gdf, mean.steps = mean(steps))
ggplot(summ, aes(interval, mean.steps)) + 
    geom_line() + 
    facet_grid(weekdayType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

