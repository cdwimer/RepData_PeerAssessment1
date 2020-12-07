### Loading and preprocessing the data

    filename <- "repdata_data_activity.zip"
    if(!file.exists(filename)){
        fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileurl, "repdata_data_activity.zip")
    }
    if(!file.exists("activity.csv")) {
        unzip(filename)
    }
    activity <- read.csv("activity.csv")
    library(ggplot2)

### What is the average total number of steps taken per day?

    #calculate total steps taken each day
    dailySteps <- aggregate(steps ~ date, activity, sum)
    #histogram of the total number of steps taken each day
    graph <- ggplot(dailySteps, aes(steps))
    graph + geom_histogram (binwidth = 2000, col = "blue", fill= "light grey") + labs(title = "Histogram of Total Daily Steps", x = "Steps", y = "Frequency") + 
        theme_bw()

(PA1_template_files/figure-markdown_strict/average%20total%20steps-1.png)

    #calculate mean number of steps taken each day
    paste("Mean number of steps:", mean (dailySteps$steps))

    ## [1] "Mean number of steps: 10766.1886792453"

    #calculate median number of steps taken each day
    paste("Median number of steps:", median (dailySteps$steps))

    ## [1] "Median number of steps: 10765"

### What is the average daily activity pattern?

    #plot time series of the average number of steps taken
    timeSteps<- aggregate(steps ~ interval, activity, mean)
    graph <- ggplot(timeSteps, aes(interval, steps))
    graph + geom_line() + labs(title = "Time Series of Average Number of Steps Taken", x = "5 minute Interval", y = "number of steps") + 
        theme_bw()

(PA1_template_files/figure-markdown_strict/average%20daily%20activity-1.png)

    #calculate 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps
    paste("interval with max # steps:", timeSteps$interval[which.max(timeSteps$steps)])

    ## [1] "interval with max # steps: 835"

### Imputing missing values

    #calculate total number of missing values in the dataset
    paste("Number of missing values:",sum(is.na(activity$steps)))

    ## [1] "Number of missing values: 2304"

    # fill in all of the missing values with mean of the interval
    activityCleaned <- activity
    activityCleaned$steps <- ifelse(is.na(activityCleaned$steps), timeSteps$steps[match(activityCleaned$interval, timeSteps$interval)], activityCleaned$steps)
    dailyStepsCleaned <- aggregate(steps ~ date, activityCleaned, sum) 
    #plot histogram of the total number of steps taken each day using imputed table
    graph <- ggplot(dailyStepsCleaned, aes(steps))
    graph + geom_histogram (binwidth = 2000, col = "blue", fill= "light grey") + labs(title = "Histogram of Total Daily Steps", x = "number of steps", y = "Frequency") + 
        theme_bw()

(PA1_template_files/figure-markdown_strict/missing%20values%20histogram-1.png)

    #calculate mean number of steps taken each day using imputed table
    paste("Mean number of steps:", mean (dailyStepsCleaned$steps))

    ## [1] "Mean number of steps: 10766.1886792453"

    #calculate median number of steps taken each day using imputed table
    paste("Median number of steps:", median (dailyStepsCleaned$steps))

    ## [1] "Median number of steps: 10766.1886792453"

### Are there differences in activity patterns between weekdays and weekends?

    #panel plot time series of the average number of steps taken weekdays vs weekends
    activityCleaned$day <- weekdays(as.Date(activityCleaned$date))
    activityCleaned$daytype<- ifelse(activityCleaned$day == c("Sunday", "Saturday"), "weekend", "weekday")
    timeStepsCleaned<- aggregate(steps ~ interval + daytype, activityCleaned, mean)
    graph <- ggplot(timeStepsCleaned, aes(interval, steps, group=daytype)) + geom_line()
    graph + facet_grid(daytype~.) + labs(title = "Time Series of Average Number of Steps Taken", x = "5 minute Interval", y = "number of steps") + 
        theme_bw()

(PA1_template_files/figure-markdown_strict/weekdays%20vs%20weekends-1.png)
