# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("D:/DataScience/Coursera/1 - Data Science - Specialization/RWD")
data <- read.csv(file = "repdata_data_activity/activity.csv")
dataWithoutNAs <- data[!is.na(data$steps),]
dateLevels <- levels(dataWithoutNAs$date)
nR <- length(dateLevels)
```


## What is mean total number of steps taken per day?


```r
m<-tapply(dataWithoutNAs$steps, dataWithoutNAs$date, mean, simplify = FALSE)
n<-tapply(dataWithoutNAs$steps, dataWithoutNAs$date, median, simplify = FALSE)
s<-tapply(dataWithoutNAs$steps, dataWithoutNAs$date, sum, simplify = FALSE)
df <- data.frame(date=names(m), mean=numeric(nR), median=numeric(nR), sum=numeric(nR), stringsAsFactors=FALSE)
if(is.null(m[[1]])){
  x <- c(NA)
}else{
  x <- m[[1]]
}
if(is.null(n[[1]])){
  y <- c(NA)
}else{
  y <- n[[1]]
}
if(is.null(s[[1]])){
  z <- c(NA)
}else{
  z <- s[[1]]
}

for(i in 2:nR){
  if(is.null(m[[i]])){
    x <- c(x,NA)
  }else{
    x <- c(x,m[[i]])
  }
  if(is.null(n[[i]])){
    y <- c(y,NA)
  }else{
    y <- c(y,n[[i]])
  }
  if(is.null(s[[i]])){
    z <- c(z,NA)
  }else{
    z <- c(z,s[[i]])
  }
}
  df$date <- strptime(df$date,"%Y-%m-%d")
  df$mean <- x
  df$median <- y
  df$sum <- z
  print(df)
```

```
##          date       mean median   sum
## 1  2012-10-01         NA     NA    NA
## 2  2012-10-02  0.4375000      0   126
## 3  2012-10-03 39.4166667      0 11352
## 4  2012-10-04 42.0694444      0 12116
## 5  2012-10-05 46.1597222      0 13294
## 6  2012-10-06 53.5416667      0 15420
## 7  2012-10-07 38.2465278      0 11015
## 8  2012-10-08         NA     NA    NA
## 9  2012-10-09 44.4826389      0 12811
## 10 2012-10-10 34.3750000      0  9900
## 11 2012-10-11 35.7777778      0 10304
## 12 2012-10-12 60.3541667      0 17382
## 13 2012-10-13 43.1458333      0 12426
## 14 2012-10-14 52.4236111      0 15098
## 15 2012-10-15 35.2048611      0 10139
## 16 2012-10-16 52.3750000      0 15084
## 17 2012-10-17 46.7083333      0 13452
## 18 2012-10-18 34.9166667      0 10056
## 19 2012-10-19 41.0729167      0 11829
## 20 2012-10-20 36.0937500      0 10395
## 21 2012-10-21 30.6284722      0  8821
## 22 2012-10-22 46.7361111      0 13460
## 23 2012-10-23 30.9652778      0  8918
## 24 2012-10-24 29.0104167      0  8355
## 25 2012-10-25  8.6527778      0  2492
## 26 2012-10-26 23.5347222      0  6778
## 27 2012-10-27 35.1354167      0 10119
## 28 2012-10-28 39.7847222      0 11458
## 29 2012-10-29 17.4236111      0  5018
## 30 2012-10-30 34.0937500      0  9819
## 31 2012-10-31 53.5208333      0 15414
## 32 2012-11-01         NA     NA    NA
## 33 2012-11-02 36.8055556      0 10600
## 34 2012-11-03 36.7048611      0 10571
## 35 2012-11-04         NA     NA    NA
## 36 2012-11-05 36.2465278      0 10439
## 37 2012-11-06 28.9375000      0  8334
## 38 2012-11-07 44.7326389      0 12883
## 39 2012-11-08 11.1770833      0  3219
## 40 2012-11-09         NA     NA    NA
## 41 2012-11-10         NA     NA    NA
## 42 2012-11-11 43.7777778      0 12608
## 43 2012-11-12 37.3784722      0 10765
## 44 2012-11-13 25.4722222      0  7336
## 45 2012-11-14         NA     NA    NA
## 46 2012-11-15  0.1423611      0    41
## 47 2012-11-16 18.8923611      0  5441
## 48 2012-11-17 49.7881944      0 14339
## 49 2012-11-18 52.4652778      0 15110
## 50 2012-11-19 30.6979167      0  8841
## 51 2012-11-20 15.5277778      0  4472
## 52 2012-11-21 44.3993056      0 12787
## 53 2012-11-22 70.9270833      0 20427
## 54 2012-11-23 73.5902778      0 21194
## 55 2012-11-24 50.2708333      0 14478
## 56 2012-11-25 41.0902778      0 11834
## 57 2012-11-26 38.7569444      0 11162
## 58 2012-11-27 47.3819444      0 13646
## 59 2012-11-28 35.3576389      0 10183
## 60 2012-11-29 24.4687500      0  7047
## 61 2012-11-30         NA     NA    NA
```

```r
  hist(df$sum, main="Histogram", xlab = "Total number of steps taken each day", col = "red")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

## What is the average daily activity pattern?

```r
  data$date <- strptime(data$date,"%Y-%m-%d")
  data2 <- data[!is.na(data$steps),]
  f <- tapply(data2$steps, data2$interval, mean, simplify = FALSE)
  intervalLevels <- levels(as.factor(data2$interval))
  nR <- length(intervalLevels)
  df2 <- data.frame(interval=intervalLevels, mean=numeric(nR), stringsAsFactors=FALSE)

  if(is.null(f[[1]])){
    x <- c(NA)
  }else{
    x <- f[[1]]
  }

for(i in 2:nR){
  if(is.null(f[[i]])){
    x <- c(x,NA)
  }else{
    x <- c(x,f[[i]])
  }
}
  df2$mean <- x
  plot(df2$interval, df2$mean, type="l", xlab="Intervals", ylab="AVG")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
  print(df2[df2$mean == max(df2$mean), ]) 
```

```
##     interval     mean
## 104      835 206.1698
```


## Imputing missing values
Records with NAs

```r
print(nrow(data) - nrow(data2))
```

```
## [1] 2304
```

```r
##filling NAs with mean values
##check for NAs values in the original data.frame
##For each NA value corresponding to an interval
##Get the avg steps for that specific interval which we have already calculated before
##Update NA with the AVG value
nR <- nrow(data)
for(i in 1:nR){
  if(is.na(data$steps[i])){
    nx <- nrow(df2)
    for(j in 1:nx){
      if(data$interval[i] == df2$interval[j]){
        data$steps[i] <- df2$mean[j]
      }
    }
  }
}
```


## Are there differences in activity patterns between weekdays and weekends?

```r
data <- data.frame(data, weekday=weekdays(data$date))
#print(data)
```
