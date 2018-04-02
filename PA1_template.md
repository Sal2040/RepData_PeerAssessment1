---
title: "Assignment5/2"
output: 
  html_document: 
    keep_md: yes
---
#Assignment: Reproducible Research / Week 2
##Loading a preprocessing data
I'm going to set my working directory where the "activity.csv" file is saved and load the file into R. It will be handy to save it as a tibble so I can manipulate the data through the dplyr package. I will also use the lubridate package to format the "date" column. 


```r
library(dplyr)
setwd("~/Coursera/ProgrammingAssignment5_Markdown")
data <- read.csv("activity.csv")
```

##What is mean total number of steps taken per day?
Let's remove the NA values and use dplyr to calculate the sums of steps per day:

```r
data <- na.omit(data)
sums <- data %>% group_by(date) %>% summarise(sum_of_steps = sum(steps))
```
Now let's use the data stored in the "sums" variable to make a histogram:

```r
library(ggplot2)
ggplot(sums, aes(x=date, y=sum_of_steps)) + geom_histogram(stat = "identity", col = "black", fill = "red") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Finally, let's show mean and median of the steps taken per day:

```r
a <- data.frame(Mean = mean(sums$sum_of_steps), Median = median(sums$sum_of_steps))
a
```

```
##       Mean Median
## 1 10766.19  10765
```
##What is the average daily activity pattern?
This time we need to group the data by intervals and count the mean value of the steps:

```r
ints <- data %>% group_by(interval) %>% summarise(average_steps = mean(steps))
```
And use a line plot:

```r
ggplot(ints, aes(x=interval, y=average_steps, group = 1)) + geom_line(col = "red", size = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The interval with the maximum average steps is:

```r
ints %>% filter(average_steps == max(average_steps)) %>% select(interval)
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```
#Imputing missing values
To find out about missing values, we need to reload the orinial data file:

```r
data <- read.csv("activity.csv")
```
Now let's see the number of observations having missing values:

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```
The missing values should only be in the step count, but let's check if they are not in the other variables just to be sure:

```r
a <- lapply(data, is.na)
lapply(a, sum)
```

```
## $steps
## [1] 2304
## 
## $date
## [1] 0
## 
## $interval
## [1] 0
```
We can see that the missing values are indeed only in the step count.

Two basic simple options for imputing missing values seem to be using either interval means or day means. The interval means are shown above. Let's see what the day means look like:


```r
days <- na.omit(data) %>% group_by(date) %>% summarise(average_steps = mean(steps))
ggplot(days, aes(x=date, y=average_steps, group = 1)) + geom_line(col = "red", size = 1)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

It seems like the interval means are the better option. They show a clearer pattern and capture higher range than the daily means. Those seem to oscilate quite randomly.
Let's replace the NA values by the interval means and create a new dataset:

```r
data2 <- data
ints <- as.data.frame(ints)
for(i in 1:nrow(data2)) {
     if(data2[i,1] %in% NA) {
       x <- data2[i,3]
       y <- ints[ints$interval == x, 2]
       data2[i,1] <- y
      }
}
```
Let's now count the total steps per day and calculate mean and median using the new dataset:

```r
sums2 <- data2 %>% group_by(date) %>% summarise(sum_of_steps = sum(steps))
ggplot(sums2, aes(x=date, y=sum_of_steps)) + geom_histogram(stat = "identity", col = "black", fill = "red") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
b <- data.frame(Mean = mean(sums2$sum_of_steps), Median = median(sums2$sum_of_steps))
b
```

```
##       Mean   Median
## 1 10766.19 10766.19
```

It seems like the imputed values did not substantially change the overall characyeristics of the dataset. Let's look closer just to be sure:

```r
summary(sums$sum_of_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
summary(sums2$sum_of_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
ggplot(data = sums, aes(x=factor("original"), y=sum_of_steps)) + geom_boxplot() + geom_boxplot(data = sums2, aes(x=factor("new"), y=sum_of_steps))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

So mean and median are practically identical, as well as the outliers. The imputed values reduce the range between the 1st and the 3rd quartile, not significantly though.
#Are there differences in activity patterns between weekdays and weekends?
First we need to format the date column in the new dataset to the actual date:

```r
library(lubridate)
data2$date <- ymd(data2$date)
```
Now let's create a column showing categories "weekday" and "weekend":

```r
data2 <- data2 %>% mutate(wday = wday(data2$date))
data2[data2$wday %in% c(2:6),]$wday <- "weekday"
data2[data2$wday %in% c(1,7),]$wday <- "weekend"
data2$wday <- as.factor(data2$wday)
```
...and now plot time series of average steps per interval, average being taken separately for weekdays and weekends:

```r
ints2 <- data2 %>% group_by(wday, interval) %>% summarise(average_steps = mean(steps))
ggplot(ints2, aes(x = interval, y = average_steps)) + geom_line(col = "red", size = 1) + facet_grid(wday~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

So the weekend patter seems to be a little smoother and the walking activity is distributed more evenly within the day. But the difference does not seem to be very substantial.
