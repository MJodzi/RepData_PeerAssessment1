---
title: 'Activity Monitoring: Peer Assignment 1'
output: html_document
date: "December 15, 2015"
---

## __Introduction__

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activityCSV monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### __Data__

The data for this assignment can be downloaded from the course web site.

The variables included in this dataset are:

* __steps:__ Number of steps taking in a 5-minute interval (missing values are coded as NA)

* __date:__ The date on which the measurement was taken in YYYY-MM-DD format

* __interval:__ Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## __Loading and Processing the Data__

Presuming the reader as set their working directory, the first step is to load the data:

```{r}
activityCSV_raw <- read.csv("C:/Users/Iso/Documents/activity.csv")
```

To process/transform the data into a more suitable format we will start by changing the date format, distinguishing the weekdays from the weekends, and then creating the "cleaned" data version.

```{r}
activityCSV_raw$date <- as.POSIXct(activityCSV_raw$date, format="%Y-%m-%d")


activityCSV_raw <- data.frame(date=activityCSV_raw$date, 
                           weekday=tolower(weekdays(activityCSV_raw$date)), 
                           steps=activityCSV_raw$steps, 
                           interval=activityCSV_raw$interval)


activityCSV_raw <- cbind(activityCSV_raw, 
                      daytype=ifelse(activityCSV_raw$weekday == "saturday" | 
                                     activityCSV_raw$weekday == "sunday", "weekend", 
                                     "weekday"))


activityCSV <- data.frame(date=activityCSV_raw$date, 
                       weekday=activityCSV_raw$weekday, 
                       daytype=activityCSV_raw$daytype, 
                       interval=activityCSV_raw$interval,
                       steps=activityCSV_raw$steps)

```

To view the raw data we can type:

```{r}
head(activityCSV_raw)
```


To view the processed data we type:

```{r}
head(activityCSV)
```



## __What is Mean Total Number of Steps Taken Per Day?__


First, we are asked to total the number of steps taken each day (and to ignore the missing values):

```{r}
sumData <- aggregate(activityCSV$steps, by=list(activityCSV$date), FUN=sum, na.rm=TRUE)
```

It would be better if we could rename the attributes for convenience:

```{r}
names(sumData) <- c("date", "total")
```

To display the first few rows:

```{r}
head(sumData)
```


Next, we are asked to produce a histogram:

```{r}
hist(sumData$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="red", 
     xlab="Total Number of Steps", 
     ylim=c(0, 20), 
     main="Histogram of the Total Number of Steps Taken Each Day\n(NA Removed)")
```


The mean and median of the total number of steps taken per day can be found by:

```{r}
mean(sumData$total)
median(sumData$total)
```


## __What is the Average Daily activityCSV Pattern?__


To make a time series plor, we type in:

```{r}
meanData <- aggregate(activityCSV$steps, 
                       by=list(activityCSV$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

names(meanData) <- c("interval", "mean")

plot(meanData$interval, 
     meanData$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```

Check your work by displaying:

```{r}
head(meanData)
```

We can find out which 5-minute interval contains the maximum number of steps byusing the following code:

```{r}
max_pos <- which(meanData$mean == max(meanData$mean))

max_interval <- meanData[max_pos, 1]
```


## __Inputing the Missing Values__

Note that there are a number of days/intervals where there are missing values (coded as 'NA'). The presence of missing days may introduce bias into some calculations or summaries of the data.

We are asked to:

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


We begin by counting how many NA fields are present, locating them and then replacing them with the mean:

```{r}
NA_count <- sum(is.na(activityCSV$steps))

na_pos <- which(is.na(activityCSV$steps))

mean_Rep <- rep(mean(activityCSV$steps, na.rm=TRUE), times=length(na_pos))
```

Next, we want to create a new dataset. We do this by using the last code we created above:

```{r}
activityCSV[na_pos, "steps"] <- mean_Rep
```

Lastly, we display the histogram:

```{r}
sumData <- aggregate(activityCSV$steps, by=list(activityCSV$date), FUN=sum)

names(sumData) <- c("date", "total")

hist(sumData$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="red", 
     xlab="Total Number of Steps", 
     ylim=c(0, 30), 
     main="Histogram of the Total Number of Steps Taken Each Day\n(NA Replaced by Mean Value)")
```


## __Are There Differences in Activity Patterns Between Weekdays and Weekends?__

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

We actually already made the new factgor variable:

```{r}
head(activityCSV)
```

To create the panel plot, we need to install the lattice package. I find the easiest way to do this is by going to the "Packages" icon on your Files, Plots, Packages, etc helper window. Top left is an "Install" option, select that and type in "lattice". 

Once you have the new package installed, load the package using the `library(lattice)` command (you can ignore the warning that follows).

```{r}
library(lattice)
```

Next, we can calculate the average number of steps taken by using the code:

```{r}
meanData <- aggregate(activityCSV$steps, 
                       by=list(activityCSV$daytype, 
                               activityCSV$weekday, activityCSV$interval), mean)

names(meanData) <- c("daytype", "weekday", "interval", "mean")
```

We check our work:

```{r}
head(meanData)
```


Finally, we make the plot:

```{r}
xyplot(mean ~ interval | daytype, meanData, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of Steps", 
       layout=c(1,2))
```


