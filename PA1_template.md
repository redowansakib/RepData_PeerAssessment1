---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



``` r
library(dplyr)
library(ggplot2)
library(VIM)
```


## Loading and preprocessing the data
<br>
First load the data into `activity` variable. 

``` r
activity <- read.csv(unzip("activity.zip","activity.csv"))
head(activity)
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
From the data we can see that there is a variable called date. It would be very 
convenient if it would be a **Date** class.


``` r
class(activity$date)
```

```
## [1] "character"
```

We can see `acitiviy$date` variable is not a **Date** class rather a **character** 
class. So, we need to convert `acitiviy$date` into **Date** class.


``` r
activity$date <- as.Date(activity$date)
```
Now we can use this data for our analysis properly.
<br>
<br>

## What is mean total number of steps taken per day?


``` r
total_steps_per_day <- group_by(activity,date) %>%
                       summarise(total_steps=sum(steps,na.rm = TRUE))
g_tot <- ggplot(total_steps_per_day,aes(x=date,y=total_steps))
g_tot + 
  geom_col(fill="deeppink") + 
  labs(title= "Total Number of Steps Taken per Day",x="",y="Total Steps")+
  theme(plot.title = element_text(hjust=0.5))
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->

#### Mean Steps per Day

``` r
mean_steps_per_day <- group_by(activity,date) %>%
                      summarise(mean_steps=mean(steps,na.rm = TRUE))
head(mean_steps_per_day)
```

```
## # A tibble: 6 × 2
##   date       mean_steps
##   <date>          <dbl>
## 1 2012-10-01    NaN    
## 2 2012-10-02      0.438
## 3 2012-10-03     39.4  
## 4 2012-10-04     42.1  
## 5 2012-10-05     46.2  
## 6 2012-10-06     53.5
```

#### Median Steps per Day

``` r
median_steps_per_day <- group_by(activity,date) %>%
                        summarise(median_steps=median(steps,na.rm = TRUE))
head(median_steps_per_day)
```

```
## # A tibble: 6 × 2
##   date       median_steps
##   <date>            <dbl>
## 1 2012-10-01           NA
## 2 2012-10-02            0
## 3 2012-10-03            0
## 4 2012-10-04            0
## 5 2012-10-05            0
## 6 2012-10-06            0
```

## What is the average daily activity pattern?

``` r
daily_avg <- group_by(activity,interval) %>% 
             summarise(avg_steps=mean(steps,na.rm = T))

max_step <- daily_avg[which.max(daily_avg$avg_steps),]
print(max_step)
```

```
## # A tibble: 1 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```

``` r
g_daily <- ggplot(daily_avg,aes(x=interval,avg_steps))
g_daily +
  geom_line() +
  labs(title="Daily Average Steps",x="",y="Average Steps") +
  theme(plot.title = element_text(hjust=0.5))
```

![](PA1_template_files/figure-html/average daily activity pattern-1.png)<!-- -->

Maximum mean daily interval is 835  with 206. steps.


## Imputing missing values

For imputing strategy used here is K-Nearest Neighbor. Nearest 6 row of each row is calculated by euclidean method and then the most likely value has been taken from the nearest rows. Imputed version of total dataset is assigned to new variable `activity_imp`.


``` r
activity_imp <- kNN(activity,variable = c("steps"),k=6,imp_var = F)

total_imp <- group_by(activity_imp,date) %>%
                      summarise(total_steps=sum(steps,na.rm = TRUE))
g_tot_imp <- ggplot(total_imp,aes(x=date,y=total_steps))
g_tot_imp + 
  geom_col(fill="deeppink") + 
  labs(title= "Total Number of Steps Taken per Day",x="",y="Total Steps")+
  theme(plot.title = element_text(hjust=0.5))
```

![](PA1_template_files/figure-html/Imputin Missing Values-1.png)<!-- -->

``` r
mean_imp <- group_by(activity_imp,date) %>%
                      summarise(mean_steps=mean(steps,na.rm = TRUE))
head(mean_imp)
```

```
## # A tibble: 6 × 2
##   date       mean_steps
##   <date>          <dbl>
## 1 2012-10-01      9.76 
## 2 2012-10-02      0.438
## 3 2012-10-03     39.4  
## 4 2012-10-04     42.1  
## 5 2012-10-05     46.2  
## 6 2012-10-06     53.5
```

``` r
median_imp <- group_by(activity_imp,date) %>%
                        summarise(median_steps=median(steps,na.rm = TRUE))
head(median_imp)
```

```
## # A tibble: 6 × 2
##   date       median_steps
##   <date>            <dbl>
## 1 2012-10-01            0
## 2 2012-10-02            0
## 3 2012-10-03            0
## 4 2012-10-04            0
## 5 2012-10-05            0
## 6 2012-10-06            0
```

After imputation we can see that the mean steps per date has changed. Before imputation we could observe some NA values despite `na.rm = T`. But after imputation the NA values are populated. But in case of median the values are still zero. The plot of total steps before imputation has some missing column but after imputation those columns are visible and has some values.



## Are there differences in activity patterns between weekdays and weekends?



``` r
days <- weekdays(activity_imp$date)
day_names <- weekdays(as.Date(4,"1970-01-01",tz="GMT")+0:6)
activity_imp$day <- factor(days,levels=day_names,
            labels =c(rep("weekday",5),rep("weekend",2)))

days_activity <- group_by(activity_imp,day,interval) %>%
                  summarise(mean_steps=mean(steps))
g_days <- ggplot(days_activity,aes(x=interval,y=mean_steps))
g_days +
  geom_line()+
  labs(title="Activity Patterns Between Weekdays and Weekends",x="",y="Mean steps")+
  theme(plot.title = element_text(hjust=0.5))+
  facet_wrap(vars(day),nrow=2,ncol=1,strip.position="top")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

From the plot we can see there is a clear contrast in average steps between weekday and weekend.
