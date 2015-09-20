# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Data is loaded using read.csv and stored in the dataframe mydata.

```r
library(ggplot2)
mydata = read.csv("activity/activity.csv")
head(mydata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
First we filter out the rows which don't have missing values in the "steps" column. Then the aggregate function is used to calculate the average number of steps per day.

```r
mydataf = mydata[!is.na(mydata$steps),]
dailystepscount = aggregate(mydataf$steps,by=list(mydataf$date), FUN=sum, na.rm=TRUE)
names(dailystepscount)=c("Date","Total Steps")
head(dailystepscount)
```

```
##         Date Total Steps
## 1 2012-10-02         126
## 2 2012-10-03       11352
## 3 2012-10-04       12116
## 4 2012-10-05       13294
## 5 2012-10-06       15420
## 6 2012-10-07       11015
```

The histogram for total number of steps per day is plotted.

```r
hist(dailystepscount$`Total Steps`, 
     breaks=20, 
     col="green",
     xlab="Total Steps",
     main="Histogram of Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Mean and median of the total steps per day:

```r
mean(dailystepscount$`Total Steps`)
```

```
## [1] 10766.19
```

```r
median(dailystepscount$`Total Steps`)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
avgstepts = aggregate(mydata$steps,by=list(mydata$interval), FUN=mean, na.rm=TRUE)
names(avgstepts) = c("interval","Average Steps")
ggplot(avgstepts, aes(x=interval, y=`Average Steps`))+
  geom_line(col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Interval with Maximum number of average steps:

```r
avgstepts[which.max(avgstepts$`Average Steps`),]
```

```
##     interval Average Steps
## 104      835      206.1698
```

#Inputting Missing Values
Total number of rows with missing values:

```r
nrow(mydata[is.na(mydata$steps),])
```

```
## [1] 2304
```
Missing values are filled by using the **Mean for the Interval**

```r
missingvals <- mydata[is.na(mydata$steps),]
nomissingvals <- mydata[!is.na(mydata$steps),]
names(avgstepts)=c('interval','steps')
mergevals <- merge(avgstepts, missingvals, by='interval')
filledmissingvals <- mergevals[,c('steps.x','date','interval')]
names(filledmissingvals)[1]='steps'
```
New data set with the filled missing values:

```r
mydatafilled <- rbind(nomissingvals,filledmissingvals)
nrow(is.na(mydatafilled$steps))
```

```
## NULL
```

Daily total number of steps for the new data set:

```r
dailystepscount = aggregate(mydatafilled$steps,by=list(mydatafilled$date), FUN=sum, na.rm=TRUE)
names(dailystepscount)=c("Date","Total Steps")
head(dailystepscount)
```

```
##         Date Total Steps
## 1 2012-10-01    10766.19
## 2 2012-10-02      126.00
## 3 2012-10-03    11352.00
## 4 2012-10-04    12116.00
## 5 2012-10-05    13294.00
## 6 2012-10-06    15420.00
```

Histogram of the new data set:

```r
hist(dailystepscount$`Total Steps`, 
     breaks=20, 
     col="blue",
     xlab="Total Steps",
     main="Histogram of Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

```r
mean(dailystepscount$`Total Steps`)
```

```
## [1] 10766.19
```

```r
median(dailystepscount$`Total Steps`)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
mydatafilled$date <- as.Date(mydatafilled$date)
mydatafilled$day = weekdays(mydatafilled$date)
mydatafilled$daytype = ''
mydatafilled[mydatafilled$day %in% c("Saturday","Sunday"),'daytype']="Weekend"
mydatafilled[!mydatafilled$day %in% c("Saturday","Sunday"),'daytype']="Weekday"
mydatafilled$daytype <- factor(mydatafilled$daytype)
avgstepsdaytype = aggregate(mydatafilled$steps, by=list( mydatafilled$interval, mydatafilled$daytype), FUN=mean)
names(avgstepsdaytype)=c("Interval","DayType","Average Steps")
ggplot(data=avgstepsdaytype, aes(x=Interval, y=`Average Steps`))+
  geom_line(color="blue")+
  facet_grid(.~ DayType)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

It can be observed that the movemnt i.e. the number of steps are more uniform across the day during the weekends. 
