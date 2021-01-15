# Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:
        
*  Dataset: Activity monitoring data [52K]
The variables included in this dataset are:
        
      * **steps**: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

# Libraries required

```{r}
library(dplyr)
library(ggplot2)
library(Hmisc)
```

# Loading and the preprocessing data

```{r}
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "data.zip")
unzip(zipfile = "data.zip")
raw <- read.csv("activity.csv", stringsAsFactors = F)
```

## Processing the data

```{r}
raw$date <- as.POSIXct(raw$date, format = "%Y-%m-%d")
```
# What is mean total number of steps taken per day?

Calculate the total number of steps taken per day

```{r}
q1data <- group_by(raw[complete.cases(raw), ], date) %>% summarise(total = sum(
        steps, na.rm = T))
```

Make a histogram of the total number of steps taken each day

```{r}
ggplot(q1data, aes(x = total)) + geom_histogram(bins = 20) + 
        xlab("Total Steps Per Day") + 
        ylab("Frequency (Count)") + scale_y_continuous(expand = c(0, 0)) + 
        theme_classic()
```

Calculate and report the mean and median of the total number of steps taken per day

```{r}
cat("Mean of the total number of steps taken per day: ", mean(q1data$total))
cat("\n")
```

# What is the average daily activity pattern?

Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
q2data <- group_by(raw, interval) %>% summarise(mean = mean(steps, na.rm = T))
ggplot(q2data, aes(x = interval, y = mean)) + geom_line() + 
        xlab("5-Minute Interval") + 
        ylab("Average Steps per Day") + scale_y_continuous(expand = c(0, 0)) + 
        theme_classic()
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
cat("The", q2data[which.max(q2data$mean), ]$interval, "th interval contains the
    maximum mean number of steps: ", q2data[which.max(q2data$mean), ]$mean)
```

# Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```{r}
sum(is.na(raw$steps))
sum(is.na(raw$date))
sum(is.na(raw$interval))

cat("There are", sum(is.na(raw$steps)), "NAs in the steps column")
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r}
q3dataDayMean <- group_by(raw, date) %>% summarise(meanStepsPerDay = mean(steps))

cat("There are", sum(is.na(q3dataDayMean$meanStepsPerDay)), "missing values")

table(is.na(q2data$mean))
fillWithMean <- function(query, ref){
        imputedData <- c() #empty vector to store the data
        for(i in 1:nrow(query)){
                if(is.na(query[i, "steps"]) == T){
                        imputedData[i] <- ref[ref$interval == 
                                                      query[i, "interval"], ]$mean
                } else {
                        imputedData[i] <- query[i, "steps"]
                }
                
        }
        return(imputedData)
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
q3data <- raw
q3data <- mutate(q3data, steps = fillWithMean(q3data, q2data))
q3data$imp5MinIntMean <- impute(q3data$steps, fun = rep(q2data$mean, sum(
        is.na(q3data$steps)) / nrow(q2data)))

q3data <- q3data[, -4]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
hist <- group_by(q3data, date) %>% summarise(total = sum(steps))
hist$Total <- as.integer((hist$total))
ggplot(hist, aes(x = total)) + geom_histogram(bins = 20) + 
        xlab("Total Number of Steps Taken each Day") + 
        ylab("Frequency (Count)") + scale_y_continuous(expand = c(0, 0)) + 
        theme_classic()


cat("Mean of total number of steps taken per day after filling NAs: ",
    mean(hist$total))
cat("Median of total number of steps taken per day after filling NAs: ",
    median(hist$total))
```

# Are there differences in activity patterns between weekdays and weekends?

```{r}
q4data <- q3data
```

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
q4data$day <- ifelse(as.POSIXlt(q4data$date)$wday %in% c(1:5), "weekday", 
                     "weekend")
head(q4data, 10)
```

Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
q4dataAve <- q4data %>% group_by(day, interval) %>% summarise(meanSteps = 
                                                                      mean(steps))
ggplot(q4dataAve, aes(x = interval, y = meanSteps)) + geom_line() + 
        facet_grid(day ~ .) +
        xlab("5-Minute Interval") + 
        ylab("Average Number of Steps Taken") + 
        scale_y_continuous(expand = c(0, 0)) + 
        theme_classic()
```