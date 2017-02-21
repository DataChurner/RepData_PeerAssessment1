# Reproducible Research: Peer Assessment 1

## Libraries needed

```r
library(stringr)
library(ggplot2)
library(knitr)
library(dplyr)
```

## Set Global parameters

Setting the global parametes of echo = TRUE and warning = FALSE
(some commands generate warning with missing values)


## Loading and preprocessing the data

```r
setwd("~/Course 5 -Reproducible Research/Week 2/repdata_data_activity")
actdata <- read.csv("activity.csv",na.strings = NA)
```

Make a copy of the original dataframe for future use.

```r
newactdata <- actdata
```
We will now be converting the combination of the date and interval variable into a datetime variable, in case we need to extract any components of time like hour of the day, day of the week etc.

The str_pad function of the stringr package is used to left pad the variable with 0's and the paste function is used to add a seperator for the hour and minute, and strftime to bind it all together, and finally convert it into a POSIXct format.

```r
actdata$datetime <- as.POSIXct(strftime(paste(actdata$date,paste(substr(str_pad(actdata$interval,4,"0",side="left"),1,2),substr(str_pad(actdata$interval,4,"0",side="left"),3,4),sep = ":"),format = "%Y-%m-%d %H:%M")))
```

## What is mean total number of steps taken per day?

### Get the sum(step) by date
We will now calculate steps per day into another dataframe,
then add a new variable called datecount for better display. 

```r
stepssum <- actdata %>% group_by(date) %>% summarise(steps = sum(steps)) %>% arrange(date) 
stepssum$datecount <- c(1:61)
```
### Plotting the total steps by day histogram
Lets start off by creating a histogram of the steps taken per day for the two months
and then calculate the mean and the median.

```r
qplot(datecount,steps,data=stepssum)+geom_bar(colour="black",stat = "identity")+geom_point(na.rm = TRUE)+scale_x_continuous(breaks = round(seq(min(stepssum$datecount), max(stepssum$datecount), by = 2),1))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-6-1.png" width="950px" />

### Getting the mean
The Average number of steps as calculated by the mean function ignoring the NA values in a day is :

```r
mean(stepssum$steps,na.rm = TRUE)
```

```
## [1] 10766.19
```
### Getting the median
The Median observation of steps in the given dataset as calculated by the median function, ignoring the NA values is :

```r
median(stepssum$steps,na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### Get the mean(step) of all days by interval
A timeseries plot of the dataset will generate a line plot of the average of the steps for a given interval across all days.

we will now calculate average steps per interval into another dataframe.


```r
stepsavg <- actdata %>% group_by(interval) %>% summarise(steps = mean(steps,na.rm = TRUE)) %>% arrange(interval) 
```
### Plotting the average activity by interval line curve
The plot below shows the average number of steps for all days for a given interval.
we can determine the activity pattern of the person by analyzing the average daily activity.

```r
qplot(interval,steps,data=stepsavg)+geom_line(colour="black",stat = "identity")+geom_point(na.rm = TRUE)+scale_x_continuous(breaks = round(seq(min(stepsavg$interval), max(stepsavg$interval), by = 200),1))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-10-1.png" width="950px" />

### Calculate the interval which has max avg steps
The interval having the max average steps is obtained by executing this code :

```r
unclass(stepsavg[(stepsavg$steps == max(stepsavg$steps)),1])[1]
```

```
## $interval
## [1] 835
```



## Imputing missing values

### Cleanup the data
There are some observations that have NA as the number of steps for intervals in a day.

We can list out the total number of missing values in the dataset.

Internally the logical value of TRUE is represented by a number 1 and FALSE by 0
Thus, just by adding all the TRUE's, we can obtain a count of the missing values when the dataset is run thru the is.na function.

```r
sum(is.na(actdata$steps))
```

```
## [1] 2304
```
### Cleanup strategy
Now, the dataset presented to us, had missing values represented by NA
Since steps is an integer count of the human action of walking, we can fill in the missing values with an integer default of 0. We can do that by executing the following code.

Use the copy of the activity data performed earlier, for this step.

```r
newactdata[is.na(newactdata$steps),"steps"] <- 0
```
We will now calculate steps per day into another dataframe,
then add a new variable called datecount for better display. 

```r
newstepssum <- newactdata %>% group_by(date) %>% summarise(steps = sum(steps)) %>% arrange(date) 
newstepssum$datecount <- c(1:61)
```
### Plot the histogram
The Histogram of the average steps to date is as below.
(note: the dates that has no data point in the earlier histogram, has a data point at 0 steps)

```r
qplot(datecount,steps,data=newstepssum)+geom_bar(colour="black",stat = "identity")+geom_point(na.rm = TRUE)+scale_x_continuous(breaks = round(seq(min(newstepssum$datecount), max(newstepssum$datecount), by = 2),1))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-15-1.png" width="950px" />

### Getting the mean
The Average number of steps as calculated by the mean function in a day is :

```r
mean(newstepssum$steps)
```

```
## [1] 9354.23
```
### Getting the median
The Median observation of steps in the given dataset as calculated by the median function is :

```r
median(newstepssum$steps)
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?

### weekday or weekend
Make a list of weekends, and also make a list of day labels.

```r
wknd <- c("Saturday","Sunday")
daylbl <- c("weekday","weekend")
```
### A new variable day in the dataset
Create a new variable, day and assign it a value "weekday" or "weekend" by performing a test if the output of the function weekdays() of the date in the dataset is in the wknd list, if it does not exist pick the first element, which is weekday else, pick the second element, which is weekend.
(note: again the logical output of true or false will be considered 1 or 0 from internal representataion)

```r
newactdata$day <- daylbl[(weekdays(as.POSIXct(strftime(newactdata$date,format = "%Y-%m-%d"))) %in% wknd)+1]
```
### Calculate the average steps per weekday/weekend per interval
A timeseries plot of the dataset will generate a line plot of the average of the steps for a given interval across all weekdays in one panel, and weekends in another.

we will now calculate average steps per day per interval into another dataframe.


```r
newstepsavg <- newactdata %>% group_by(day,interval) %>% summarise(steps = mean(steps)) %>% arrange(day,interval) 
```
### Plotting the average activity by interval line curve in two panels
The plot below shows the average number of steps for all weekdays and weekends as a comparison for a given interval.
we can now compare and contrast the activity pattern of the persons weekday and weekend activities. 

```r
qplot(interval,steps,data=newstepsavg,facets = day~.)+geom_line(colour="black",stat = "identity")+scale_x_continuous(breaks = round(seq(min(newstepsavg$interval), max(newstepsavg$interval), by = 200),1))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-21-1.png" width="950px" />

## Findings

#### It looks like the person has a less intense more even spreadout activity on the weekends, but a high intensity activity in the mornings during weekdays. 

