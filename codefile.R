#libraries required
library(dplyr)
library(ggplot2)
library(Hmisc)

#Loading data
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "data.zip")
unzip(zipfile = "data.zip")
raw <- read.csv("activity.csv", stringsAsFactors = F)

#Processing data
raw$date <- as.POSIXct(raw$date, format = "%Y-%m-%d")

#Mean total number of steps taken per day

q1data <- group_by(raw[complete.cases(raw), ], date) %>% summarise(total = sum(
        steps, na.rm = T))
ggplot(q1data, aes(x = total)) + geom_histogram(bins = 20) + 
        xlab("Total Steps Per Day") + 
        ylab("Frequency (Count)") + scale_y_continuous(expand = c(0, 0)) + 
        theme_classic()

cat("Mean of the total number of steps taken per day: ", mean(q1data$total))
cat("\n")
    
#Average daily activity pattern

q2data <- group_by(raw, interval) %>% summarise(mean = mean(steps, na.rm = T))
ggplot(q2data, aes(x = interval, y = mean)) + geom_line() + 
        xlab("5-Minute Interval") + 
        ylab("Average Steps") + scale_y_continuous(expand = c(0, 0)) + 
        theme_classic()

cat("The", q2data[which.max(q2data$mean), ]$interval, "th interval contains the
    maximum mean number of steps: ", q2data[which.max(q2data$mean), ]$mean)

#Imputing missing values

sum(is.na(raw$steps))
sum(is.na(raw$date))
sum(is.na(raw$interval))

cat("There are", sum(is.na(raw$steps)), "NAs in the steps column")

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


q3data <- raw
q3data <- mutate(q3data, steps = fillWithMean(q3data, q2data))
q3data$imp5MinIntMean <- impute(q3data$steps, fun = rep(q2data$mean, sum(
        is.na(q3data$steps)) / nrow(q2data)))
q3data <- q3data[, -4]

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

#Difference in activity patterns between weekdays and weekends

q4data <- q3data
q4data$day <- ifelse(as.POSIXlt(q4data$date)$wday %in% c(1:5), "weekday", 
                     "weekend")
head(q4data, 10)

q4dataAve <- q4data %>% group_by(day, interval) %>% summarise(meanSteps = 
                                                                 mean(steps))
ggplot(q4dataAve, aes(x = interval, y = meanSteps)) + geom_line() + 
    facet_grid(day ~ .) +
    xlab("5-Minute Interval") + 
    ylab("Average Number of Steps Taken") + 
    scale_y_continuous(expand = c(0, 0)) + 
    theme_classic()
