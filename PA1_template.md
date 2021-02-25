### Loading and Preprocessing the Data

    file = read.csv("activity.csv")
    file$date = as.Date(file[,2])
    data = file[complete.cases(file),]

### What is mean total number of steps taken per day?

    d1 = aggregate(list(steps=data$steps), by=list(date=data$date), FUN=sum)

    ## Histogram of the total number of steps taken each day
    hist(d1$steps, xlab = "Steps", main = "Frequency of Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    ## Mean
    mean(d1$steps)

    ## [1] 10766.19

    ## Median
    median(d1$steps)

    ## [1] 10765

### What is the average daily activity pattern?

    d2 = aggregate(list(steps=data$steps), by=list(interval=data$interval), FUN=mean)

    ## Time Series
    plot(d2, type = "l", main = "Average Number of Steps Taken in a 5-minute Interval",
         sub = "(Note that 500 is 5:00 AM and 1500 is 3:00 PM)")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    ## Interval with max number of steps
    d2[d2$steps==max(d2$steps),]

    ##     interval    steps
    ## 104      835 206.1698

The 5-minute interval at 8:35 AM has the highest average of steps.

### Imputing Missing Data

    sum(!complete.cases(file))

    ## [1] 2304

There are 2304 rows that are missing step data.

    ## Impute missing values
    full = file
    n=1
    for (n in 1:nrow(full)) {
        if (is.na(full[n,1]) == TRUE) {
            i = full[n,3]
            full[n,1] = round(d2[d2$interval == i,2],1)
        }
        n = n + 1
    }
    d3 = aggregate(list(steps=full$steps), by=list(date=full$date), FUN=sum)

    ## Histogram of the total number of steps taken each day
    hist(d3$steps, xlab = "Steps", main = "Frequency of Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    ## Mean
    mean(d3$steps)

    ## [1] 10766.19

    ## Median
    median(d3$steps)

    ## [1] 10766.2

There is barely any change in the mean and median after the missing
values are filled in. Since I used the mean number of steps per interval
to fill in the missing values, the additional days have an average total
number of steps, thus not affecting the mean and median. For the
histogram, imputing the missing values causes there to be more days that
fall into the middle bar on the x axis.

### Are there differences in activity patterns between weekdays and weekends?

    ## Create Weekday/Weekend Data
    full$Day = weekdays(full[,2])
    full$DayType = full$Day
    n=1
    for (n in 1:nrow(full)) {
        if ((full[n,4] %in% c("Saturday","Sunday")) == TRUE) {
            full[n,5] = "Weekend"
        }
        else {
            full[n,5] = "Weekday"
        }
        n = n + 1
    }
    d4 = aggregate(list(steps=full$steps), by=list(interval=full$interval, DayType = full$DayType), FUN=mean)

    ## Facet Plot
    library(ggplot2)
    ggplot(data = d4, aes(x = interval, y = steps)) + geom_line() + facet_grid(DayType~.) + theme_light()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)
