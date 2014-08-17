
Objective
---------

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

The purpose of this analysis is to make use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day.

<style type="text/css">

        /*  CSS chunck 1  */
        th {  background-color:#E0E0E0 ;
                border-bottom:1px solid black;
                padding:5px;}

        td{
        border-bottom:1px dotted black;
        padding:10px;}

        table{ 
                border-collapse:collapse;
                margin:left;
                border: 1px solid black;} 
 
</style>

```{r dataprocessing, echo=FALSE}

#download and unzip file
setInternet2(TRUE)
if(file.exists("dataset")==F){
        dir.create("dataset")  #check if "dataset" folder exist. If doesnt, create it.
}
downloadurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(downloadurl, "dataset/dlfile.zip")
unzip("dataset/dlfile.zip", exdir="dataset")

dlfile <- read.csv("dataset/activity.csv", header=T)   #read csv file
dlfile$date <- as.Date(as.character(dlfile$date))    #convert date to "Date" class

#determine total steps taken in each day
library(plyr)
dailysteps <- ddply(dlfile, "date", summarise, total.steps=sum(steps, na.rm=T))    
```

A sample of the file being processed is illustrated below:
```{r, echo=F}
head(dlfile)

```

Total number of steps taken per day
---------------------------------------

A histrogram of the number of steps taken each day is shown below

```{r dailystep.sum, echo=FALSE}
library(RColorBrewer)
attach(dailysteps)

total.steps.mean <- paste("Mean Steps: ",round(mean(total.steps)))  #mean number of steps
total.steps.median <- paste("Median Steps: ",round(median(total.steps)))  #median number of steps
mycolor <- brewer.pal(6, "Pastel1")   #set color used

#plot histogram
hist(total.steps, col=mycolor[2], 
     main="Histogram of total steps taken in each day",
     xlab="Total Steps taken a day")
abline(v=mean(total.steps), lty=2, lwd=3, col="darkgreen")
abline(v=median(total.steps), lty=3, lwd=3, col="darkorange")
legend("topright", lty=c(2,3), col=c("darkgreen","darkorange"), 
       legend=c(total.steps.mean, total.steps.median))

```

A basic five number summary of the total steps taken in each day:
```{r, echo=FALSE}
summary(total.steps)
detach(dailysteps)

```
<!--- comment: insert line breaks -->
<br />
<br />


Averge daily activity pattern
-------------------------------------------
In an attempt to find out what is the average daily activity pattern of a person, a time series plot of the average steps made in each 5 minute interval is made. 

```{r averageactivity, echo=FALSE}

#average steps in each interval
intervalmean <- ddply(dlfile, "interval", summarise, mean.steps=mean(steps, na.rm=T))

#plot time series
with(data=intervalmean, plot(interval,mean.steps, type="l", ylim=c(0,250),
                             xlab="Interval", ylab="Mean number of steps",
                             main="Average Daily Activity Pattern", lwd=2, 
                             col="dodgerblue4"))
maxactivity <- intervalmean[intervalmean$mean.steps==max(intervalmean$mean.steps),]
maxactivity.txt <- paste("Maximum activity at interval", maxactivity$interval)
text(maxactivity.txt, x=maxactivity$interval, y=maxactivity$mean.steps+20)
points(x=maxactivity$interval, maxactivity$mean.steps, cex=3)
```


Clearly, interval **`r maxactivity$interval`** has the greatest average number of steps **(`r maxactivity$mean.steps`)**.

<br />
<br />


New dataset with "NA"" accounted
-------------------------------------------
In the previous calculation, we had ignored the effects of NA in the calculations. Here, we assume the NA values take on the average mean value of that particular interval. Example, if interval 53 at 2012-10-31 has a NA value, we would use the average steps made in the 53th interval (seen in the "Averge daily activity pattern" plot).

A sample of the new dataset is illustrated below:


```{r fillNA, echo=FALSE}

test <- which(is.na(dlfile))   #determine the rows of NA values

#insert mean steps for the interval with NA values
dlfile.na <- merge(dlfile[test,],intervalmean, by.x="interval", by.y="interval") 

dlfile.na <- dlfile.na[,-2]
names(dlfile.na) <- c("interval", "date", "steps")  #rename
dlfile.na <- with(data=dlfile.na, data.frame(steps,date,interval))

dlfile1.1 <- subset(dlfile, !is.na(steps))   #subset with NA removed
dlfile2 <- rbind(dlfile1.1,dlfile.na)
dlfile2 <- dlfile2[order(dlfile2$date,dlfile2$interval),]

```

### Old dataset:
```{r, echo=FALSE} 
head(dlfile)
```


### New dataset:
```{r,echo=FALSE} 
head(dlfile2)
```


Total number of steps taken per day (New Dataset)
------------------------------------------------

A histrogram of the number of steps taken each day is shown below:

```{r dailystep2.sum, echo=FALSE}

dailysteps2 <- ddply(dlfile2, "date", summarise, total.steps=sum(steps, na.rm=T))
attach(dailysteps2)

total.steps.mean <- paste("Mean Steps: ",round(mean(total.steps)))  #mean number of steps
total.steps.median <- paste("Median Steps: ",round(median(total.steps)))  #median number of steps
mycolor <- brewer.pal(6, "Pastel1")   #set color used

#plot histogram
hist(total.steps, col=mycolor[2], 
     main="Histogram of total steps taken in each day",
     xlab="Total Steps taken a day")
abline(v=mean(total.steps), lty=2, lwd=3, col="darkgreen")
abline(v=median(total.steps), lty=3, lwd=3, col="darkorange")
legend("topright", lty=c(2,3), col=c("darkgreen","darkorange"), 
       legend=c(total.steps.mean, total.steps.median))

```

Comparing the means of both the old and new datasets, we saw that 

     | Mean                                      | Median
-----|-------------------------------------------|-----------------
 Old | `r round(mean(dailysteps$total.steps))`   | `r round(median(dailysteps$total.steps))`
 New | `r round(mean(dailysteps2$total.steps))`  | `r round(median(dailysteps2$total.steps))`

A deeper look into the reason for this change, revealed that there are days in the old dataset having all NA readings. It could mean that the user might have switched off the device for that day. Hence, by auto-filling the days with the mean values for the 5 minute intervals will significantly change the mean and media values.

<br />
<br />

Day of the week activity
---------------------------------

Let us now look at the difference in activity between weekdays and weekends.

```{r dayofweek, echo=FALSE}

dayofweek <- ifelse(weekdays(dailysteps2$date)=="Saturday" | weekdays(dailysteps2$date)=="Sunday","weekend","weekday")

dlfile2$week <- dayofweek

#day of week activity 
dowactivity <- ddply(dlfile2, c("interval","week"), summarise, mean.steps=mean(steps))

library(ggplot2)

p <- qplot(interval, mean.steps, data=dowactivity, facets=week~., geom=c("point","line"), 
           xlab="Interval", ylab="Mean number of steps")
p + theme(strip.text.y=element_text(size=20)) + geom_line(aes(colour=week))


```

Clearly from the plots, the activities in both the weekdays and weekends are very similar. 
