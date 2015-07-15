# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
#Load the activity.csv file data using read.csv()
df <- read.csv("activity.csv")

#Format date column to date format 
df$date <- as.Date(df$date)
```

## What is mean total number of steps taken per day?

```r
#Load required libraries 
library(ggplot2)

#Calculate Total steps per day using aggregate function 
total.steps.by.day <- aggregate(x = df$steps , by = list(df$date), FUN = sum ,na.rm=TRUE)

#Change column names apprpriately 
names(total.steps.by.day) <- c("date","steps")

#Plot Histogram using derived data 
histplot <- ggplot(total.steps.by.day,aes(x = steps)) +
            ggtitle("Histogram of daily steps") +
            xlab("Steps (binwidth 2000)") +
            geom_histogram(binwidth = 2000)
histplot
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#Calculate Mean Total Number of Steps Taken per day
mean(total.steps.by.day$steps , na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
#Calculate Median Total Number of Steps Taken per day
median(total.steps.by.day$steps , na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
#Average steps by period 
average.steps.by.interval  <- aggregate(x = df$steps , by = list(df$interval), FUN = mean ,na.rm=TRUE)

#Change Column names accordingly 
names(average.steps.by.interval) <- c("interval","steps")

#Time series plot of 5-minute interval and the average number of steps taken, averaged across all days
avg.step.line <- ggplot(average.steps.by.interval,aes(interval,steps)) +
                 ggtitle("Time Series Plot of Average Steps by Interval") +
                 geom_line()
avg.step.line  
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#The 5-min time interval contains the maximum number of steps?
average.steps.by.interval[which.max(average.steps.by.interval$steps),c("interval")]
```

```
## [1] 835
```

## Imputing missing values

```r
#Total Number of Missing Values in the dataset
nrow(df[is.na(df$steps),])
```

```
## [1] 2304
```

```r
#Imputing Missing Step Values with Mean Step at Time Interval
df.imputed <- merge(x = df, y = average.steps.by.interval, by = "interval", all.x = TRUE)
df.imputed[is.na(df.imputed$steps.x),c("steps.x")] <- df.imputed[is.na(df.imputed$steps.x),c("steps.y")]

#Prepare Clean Data
df.imputed$date <- as.Date(df.imputed$date)
df.imputed$date.x <- NULL
df.imputed$Group.1 <- NULL
df.imputed$steps <- df.imputed$steps.x
df.imputed$steps.x <- NULL
df.imputed$steps.y <- NULL

#Histogram with new dataframe
total.steps.by.day <- aggregate(x = df.imputed$steps , by = list(df.imputed$date), FUN = sum ,na.rm=TRUE)

#Change column names accordingly 
names(total.steps.by.day) <- c("date","steps")


#Histogram of Daily steps after imputation 
histplot <- ggplot(total.steps.by.day,aes(x = steps)) +
            ggtitle("Histogram of daily steps after imputation") +
            xlab("Steps (binwidth 2000)") +
            geom_histogram(binwidth = 2000)
histplot 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#Mean Total Number of Steps Taken per Day
mean(total.steps.by.day$steps , na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
#Median Total Number of Steps Taken per Day
median(total.steps.by.day$steps , na.rm = TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
#Factor variable with Two levels indicating a Weekday or Weekend.
df.imputed$weekday <- as.factor(ifelse(weekdays(df.imputed$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday")) 

#Average steps by interval 
average.steps.by.interval.and.weekday  <- aggregate(x = df.imputed$steps , 
                                                    by = list(df.imputed$interval,df.imputed$weekday), FUN = mean ,na.rm=TRUE)

#Change column names accordingly 
names(average.steps.by.interval.and.weekday) <- c("interval","weekday","steps")

#Time Series plot of the 5-minute Interval and Average Number of Steps Taken 
#Average data across all weekday days or weekend days.
avg.step.line <- ggplot(average.steps.by.interval.and.weekday,aes(interval,steps)) +
                 ggtitle("Time Series Plot of Average Steps by Interval after Imputation") +
                 facet_grid(. ~ weekday) +
                 geom_line(size = 1)
avg.step.line  
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
