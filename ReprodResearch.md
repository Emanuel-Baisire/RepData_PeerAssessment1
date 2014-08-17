####Loading and preprocessing the data
 
```{r dataprocessing, echo=FALSE}

#download and unzip file
setInternet2(TRUE)
if(file.exists("dataset")==F){
        dir.create("dataset")  #check if "dataset" folder exist. If doesnt, create it.
}
downloadurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(downloadurl, "dataset/dlfile.zip")
unzip("dataset/dlfile.zip", exdir="dataset")
```


####Loading and preprocessing the data
```{r, echo=TRUE}
activity<- read.csv("./activity.csv")
str(activity)
head(activity)
clean_activity <-  activity[!is.na (activity)]
summary(clean_activity)
```
#### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day
```{r,echo=TRUE}
steps_date <- aggregate(steps ~ date, data=activity, FUN=sum)
str(steps_date)

barplot(steps_date$steps, names.arg=steps_date$date, xlab="date", ylab="steps",col="blue")
```

2. Calculate and report the **mean** and **median** total number of steps taken per day

```{r,echo=TRUE}
mean(steps_date$steps)
median(steps_date$steps)
```
#### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r, echo=TRUE}
steps_interval <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(steps_interval, type="l")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
steps_interval$interval[which.max(steps_interval$steps)]
```

####Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

```{r}
which(is.na(activity))
sum(is.na(activity))

```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For
example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

 Application of a mean of the underlying activities.

3. Create a new dataset that is equal to the original dataset but withthe missing data filled in.

```{r}
activity <- merge(activity, steps_interval, by="interval", suffixes=c("",".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[,c(1:3)]
str(activity)
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
steps_date <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(steps_date$steps, names.arg=steps_date$date, xlab="date", ylab="steps",col="orange")
mean(steps_date$steps)
median(steps_date$steps)
```
The impact of the missing data seems rather low, at least when
estimating the total number of steps per day.

####Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --"weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.

```{r, cache=TRUE}

daytype <- function(date) {
    
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) 
      {"weekend"
       
    } 
    
    else { "weekday"}
    
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days
   (y-axis).

```{r}
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval,
                            data=activity,
                            subset=activity$daytype==type,
                            FUN=mean)
    plot(steps.type, type="l",col="red", main=type)
}
```
