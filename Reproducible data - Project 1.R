---
  title: "PA1_template"
author: "me"
date: "18/06/2020"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Download File, unzip and read 
```temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp, mode="wb")
unzip(temp, "activity.csv")
activity <- read.csv("activity.csv",header=T)
unlink(temp
       ```
       #Number of steps taken per day
       ```{,echo=1}
       totalstepsperday <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
       head(totalstepsperday)
       ```
       
       Histogram
       ```{,echo=1}
       activity$date <- as.Date(activity$date, "%Y-%m-%d")
       hist(totalstepsperday$steps, 
            main="Total Steps per Day", 
            xlab="Number of Steps per Day", 
            ylab = "Interval",
            col="red",
            breaks=50)
       ```
       ##Mean steps per day
       ```{,echo=1}
       msteps <- mean(totalstepsperday$steps)
       msteps
       ```
       Median steps per day
       ```{,echo=1}
       medsteps <- median(totalstepsperday$steps)
       medsteps
       ```
       #Avergae daily activity pattern
       ```{,echo=1}
       fivemin <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
       plot(x = fivemin$interval, 
            y = fivemin$steps, 
            type = "l", 
            col = "blue",
            xlab = "5-minute Intervals",
            ylab = "Mean Steps Taken ~ Days",
            main = "Mean Daily Activity Pattern")
       ```
       ## which dataset for 5min intervals contaons maximum number of sets
       ```{,echo=1}
       maxsteps <- fivemin$interval[which.max(fivemin$steps)]
       maxsteps
       ```
       ##Input missing values - create new dataset with missing data filled in.
       ```{,echo=1}
       activity2 <- activity
       nas <- is.na(activity2$steps)
       avg_interval <- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify = TRUE)
       activity2$steps[nas] <- avg_interval[as.character(activity2$interval[nas])]
       names(activity2)
       ```
       ##Histogram of total steps with new dataframe
       ```{,echo=1}
       par(mfrow=c(1,2))
       totalstepsperday2 <- aggregate(steps ~ date, data = activity2, FUN = sum, na.rm = TRUE)
       ##Histogram with missing values and histogram without 
       hist(totalstepsperday2$steps, 
            main = "Total Steps per Day (no-NA)", 
            xlab = "Number of Steps per Day", 
            ylab = "Interval",
            col="red",
            breaks=50)
       hist(totalstepsperday$steps, 
            main="Total Steps per Day (Original)", 
            xlab="Number of Steps per Day", 
            ylab = "Interval",
            col="blue",
            breaks=50)
       ```
       ##Compare means and medians with and without values
       ```{,echo=1}
       par(mfrow=c(1,1))
       summary(totalstepsperday)
       summary(totalstepsperday)
       ```
       ##Differences between weekdays and weekend
       ```{,echo=1}
       head(activity2)
       activity2<- activity2%>%
         mutate(typeofday= ifelse(weekdays(activity2$date)=="Saturday" | weekdays(activity2$date)=="Sunday", "Weekend", "Weekday"))
       head(activity2)
       ```
       ##Plot of step taken in weekdays or weekend 
       ```{,echo=1}
       fivemin2<- aggregate(steps ~ interval, data = activity2, FUN = mean, na.rm = TRUE)
       head(fivemin2)
       ggplot(activity2, aes(x =interval , y=steps, color=typeofday)) +
         geom_line() +
         labs(title = "Ave Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
         facet_wrap(~ typeofday, ncol = 1, nrow=2)
       ```
       
       