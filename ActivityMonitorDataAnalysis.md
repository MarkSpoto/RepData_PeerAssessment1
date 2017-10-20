# Reproducible Research: Peer Assessment 1
#### Load Libraries

```r
packages <- c("lubridate", "dplyr", "ggplot2", "RcolorBrewer", "mice", "VIM")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")  
}

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(mice)
library(VIM)
```

## Loading and preprocessing the data
#### Load the Data containing activity data for October and November 2012

```r
if (!file.exists('./activity.csv')) {
    unzip('./activity.zip')
}

data <- read.csv("./activity.csv", sep = ",", header = TRUE, fill = TRUE)
```

#### Transform Data - Add day of week

```r
data <- data %>% mutate(monthday = format(as.Date(data$date, "%Y-%m-%d"), format = "%m-%d"),
                        dayofweek = weekdays(as.Date(data$date)))
```
## What is mean total number of steps taken per day?

#### Aggregate data by Date

```r
stepsPerDay <- select(data, steps, monthday) %>% 
  filter(steps >= 0) %>% 
  group_by(monthday = as.factor(monthday)) %>% 
  summarize(count = n(), 
            mean = mean(steps, na.rm = TRUE),
            median = median(steps),
            totalSteps = sum(steps, na.rm = TRUE))
```

#### Plot the Histogram of the total number of steps taken each day
###### *Note 10/1 was excluded because it contained no values*

```r
ggplot(data = stepsPerDay) + 
  geom_histogram(binwidth = 750, color = "white", aes(totalSteps, fill=totalSteps)) +
  labs(x="Total Number of Steps Taken Each Day", y="Frequency",
       title="Histogram of the Total Number of Steps Taken Each Day") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(face="bold")) +
  scale_fill_brewer(palette = "Dark2")
```

![](ActivityMonitorDataAnalysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
###### Calculate the mean and median for the total number of steps taken

```r
stepsPerDayMean = format(mean(stepsPerDay$totalSteps), digits = 8)
stepsPerDayMedian = format(median(stepsPerDay$totalSteps), digits = 8)
```
####### **Mean:** 10766.189 steps
####### **Median:** 10766.189 steps

## What is the average daily activity pattern?

#### Aggregate data by time interval (Elimiate Oct 1 due to no values)

```r
stepsPerInterval <- filter(data, steps >= 0) %>%
  group_by(interval) %>%
  summarise(avgStepsPerInterval = mean(steps, na.rm = TRUE)) %>%
  mutate(hourlyInterval = interval / 100) %>%
  mutate(hour = floor(hourlyInterval), minute = (hourlyInterval - floor(hourlyInterval)) * 100)
```

#### Time series plot of the average number of steps per 5-Min interval

```r
ggplot(data = stepsPerInterval, aes(x = hourlyInterval, y = avgStepsPerInterval)) + 
  geom_line() +
  labs(x="Steps By 5-Min Time Interval (hourly scale)", y="Total Number of Steps",
       title="Average Number of Steps Taken of the 5-Minute Interval") + 
  scale_x_continuous(breaks = scales::pretty_breaks(12)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(face="bold")) +
  scale_fill_brewer(palette = "Dark2")
```

![](ActivityMonitorDataAnalysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Imputing missing values
#### Quickly analize the data to see what the values look like

```r
summary(data)
```

```
##      steps                date          interval        monthday        
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   Class :character  
##  Median :  0.00   2012-10-03:  288   Median :1177.5   Mode  :character  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5                     
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2                     
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0                     
##  NA's   :2304     (Other)   :15840                                      
##   dayofweek        
##  Length:17568      
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

#### Give the percentage for each column missing

```r
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
```

```
##     steps      date  interval  monthday dayofweek 
##  13.11475   0.00000   0.00000   0.00000   0.00000
```

#### Using mice for looking at missing data pattern

```r
md.pattern(data)
```

```
##       date interval steps monthday dayofweek      
## 15264    1        1     1        0         0     2
##  2304    1        1     0        0         0     3
##          0        0  2304    17568     17568 37440
```
##### * The results show the following:
+ 15264 are complete
+ 2304 values are missing from **steps** measurement

##### Give a visual representation using VIM
##### This demonstrates that all missing values belong to **steps** and at the beginning of the data set


```r
aggr_plot <- aggr(data, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE, labels=names(data),
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
```

![](ActivityMonitorDataAnalysis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```
## 
##  Variables sorted by number of missings: 
##   Variable     Count
##      steps 0.1311475
##       date 0.0000000
##   interval 0.0000000
##   monthday 0.0000000
##  dayofweek 0.0000000
```

```r
marginplot(data[c(1,2)])
```

![](ActivityMonitorDataAnalysis_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

##### Now that we understand missing values are at the top and scattered for steps
##### We can assume the person did not start until Oct 2.  Oct 2nd was a startup day and the
##### other missing values are days not used.

##### Impute the missing values

```r
workerImputedData <- mice(data,m=5, maxit=5, meth='pmm',seed=500)
```

```r
head(workerImputedData$imp$steps, 30)
```

```
##      1   2   3   4   5
## 1    0   0   0   0   0
## 2    0   0   0   0   0
## 3    0  55   0   0   0
## 4    0   0   0   0  17
## 5  138  40   0   0   0
## 6   71   0  74   0   0
## 7    0   0   1   0  21
## 8    0   0   1   0  48
## 9    0 731   0  18   0
## 10   0   0   0   0   0
## 11   0   0   0  40  24
## 12   0   0   0   0  24
## 13 276   0   0   0   0
## 14  63   0   0   0   0
## 15   0   0  19   0 110
## 16   0   0   0   0   0
## 17   0  69   0   0   0
## 18   0   0   0   0  31
## 19   0   0   0   0   0
## 20   0   0  77   0 284
## 21   2   0   0   0   0
## 22   0   0   0   0   0
## 23   0 188   0   0   0
## 24   0  41   0   0   0
## 25   0 571   0   0   0
## 26 116  19   0 104   0
## 27   0  22  56   0   0
## 28   0  50   0   0   0
## 29  47   0 137   0   0
## 30   0   0   0   0   7
```

```r
imputedData <- complete(workerImputedData, 1)
```
#### Aggregate the imputed data by time interval

```r
stepsPerDayImputed <- select(imputedData, steps, monthday) %>% 
  filter(steps >= 0) %>% 
  group_by(monthday = as.factor(monthday)) %>% 
  summarize(count = n(), 
            mean = mean(steps, na.rm = TRUE),
            median = median(steps),
            totalSteps = sum(steps, na.rm = TRUE))
```
#### Time series plot of the average number of steps per 5-Min interval (imputed data)

```r
ggplot(data = stepsPerDayImputed) + 
  geom_histogram(binwidth = 750, color = "white", aes(totalSteps, fill=totalSteps)) +
  labs(x="Total Number of Steps Taken Each Day", y="Frequency",
       title="Histogram of the Total Number of Steps Taken Each Day",
       subtitle="with Imputed Values") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold")) +
  theme(axis.title = element_text(face="bold")) +
  scale_fill_brewer(palette = "Dark2")
```

![](ActivityMonitorDataAnalysis_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

###### Calculate the mean and median for the total number of steps taken on Imputed data

```r
stepsPerDayMeanImputed = format(mean(stepsPerDayImputed$totalSteps), digits = 8)
stepsPerDayMedianImputed = format(median(stepsPerDayImputed$totalSteps), digits = 8)
```
####### **Imputed Mean:** 10666.115 steps
####### **Imputed Median:** 10571 steps

## Are there differences in activity patterns between weekdays and weekends?

##### Transform data by adding the day type for both data and imputed data

```r
data$daytype <- ifelse(as.POSIXlt(data$date)$wday %in% c(0,6), 'weekend','weekday')
imputedData$daytype <- ifelse(as.POSIXlt(imputedData$date)$wday %in% c(0,6), 'weekend','weekday')
```

##### Aggregate data by interval and daytype and plot showing weekday verses weekend

```r
avgByDaytype <- data %>%
  filter(steps >= 0) %>%
  group_by(interval, daytype) %>%
  summarise(avgStepsPerInterval = mean(steps, na.rm = TRUE)) %>%
  mutate(hourlyInterval = interval / 100) %>%
  mutate(hour = floor(hourlyInterval), minute = (hourlyInterval - floor(hourlyInterval)) * 100)

ggplot(data = avgByDaytype, aes(hourlyInterval, avgStepsPerInterval)) + 
  geom_line() +
  facet_grid(daytype ~ .) +
  scale_x_continuous(breaks = scales::pretty_breaks(12)) +
  labs(x="5-Minute Time Interval (24 hour scale)", y="Avg Number of Steps",
       title="Time Series of Steps Taken",
       subtitle = "Weekday verses Weekend") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold")) +
  theme(axis.title = element_text(face="bold")) +
  scale_fill_brewer(palette = "Dark2")
```

![](ActivityMonitorDataAnalysis_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
avgByDaytypeImputed <- imputedData %>%
  filter(steps >= 0) %>%
  group_by(interval, daytype) %>%
  summarise(avgStepsPerInterval = mean(steps, na.rm = TRUE)) %>%
  mutate(hourlyInterval = interval / 100) %>%
  mutate(hour = floor(hourlyInterval), minute = (hourlyInterval - floor(hourlyInterval)) * 100)

ggplot(data = avgByDaytypeImputed, aes(hourlyInterval, avgStepsPerInterval)) + 
  geom_line() +
  facet_grid(daytype ~ .) +
  scale_x_continuous(breaks = scales::pretty_breaks(12)) +
  labs(x="5-Minute Time Interval (24 hour scale)", y="Avg Number of Steps",
       title="Time Series of Steps Taken (Imputed values)",
       subtitle = "Weekday verses Weekend") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold")) +
  theme(axis.title = element_text(face="bold")) +
  scale_fill_brewer(palette = "Dark2")
```

![](ActivityMonitorDataAnalysis_files/figure-html/unnamed-chunk-19-2.png)<!-- -->
