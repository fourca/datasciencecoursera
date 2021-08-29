# 1- Load packages and setwd
library(knitr)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/camil/Desktop/DataScience/ReproducibleResearch-Week2")

# 2- Download file
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, "amd.zip")

# First check the files that will be unzipped
unzip("amd.zip", list=T)

# Unzip file
unzip("amd.zip")

# 3- read table
amd<-read.csv("activity.csv")

# 4- Number of steps taken per day 
## 4-1- Total number of steps taken each days
amd$date<-as.factor(amd$date)
steps_sum=list()
date<-unique(amd$date)
for(i in date) {
    steps_sum[[i]]<-sum(amd[amd$date == i, c("steps")])}
steps_sum<-do.call(cbind,steps_sum)

StepsDaySum<-tapply(amd$steps, amd$date, sum)
StepsDaySum<-as.data.frame(StepsDaySum)
colnames(StepsDaySum)<-c("steps")
StepsDaySum$date<-rownames(StepsDaySum)
rownames(StepsDaySum)<-seq_along(StepsDaySum$steps)

## 4-2- Histogram of total number of steps taken each day
StepsDaySum_hist<-hist(StepsDaySum$steps, main="Total number of steps taken each day", xlab="Number of steps")

## 4-3- Mean and median of total numer of steps taken each day
StepsDaySum_mean<-mean(StepsDaySum$steps, na.rm=T)
StepsDaySum_median<-median(StepsDaySum$steps, na.rm = T)

# 5- Average daily activity pattern
## 5-1- Time series plot (5-min interval x average number of steps taken across all days)
StepsIntMean<-tapply(amd$steps, amd$interval, mean, na.rm=T)
StepsIntMean<-as.data.frame(StepsIntMean)
colnames(StepsIntMean)<-c("steps_mean")
StepsIntMean$interval<-rownames(StepsIntMean)
StepsIntMean$interval<-as.numeric(StepsIntMean$interval)
rownames(StepsIntMean)<-seq_along(StepsIntMean$interval)


StepsIntMean_plot<-plot(x=StepsIntMean$interval, y=StepsIntMean$steps_mean, type="l", 
                         main="Mean number of steps per 5min interval", xlab="Time (minutes)", ylab="Mean number of steps")

StepsIntMean_ggplot<-ggplot(data=StepsIntMean, aes(x=interval, y=steps_mean))+
    geom_point()+geom_line()+
    ggtitle("Mean numer of steps per 5 minutes interval")+
    xlab("Time (minutes)")+ylab("Mean number of steps")+
    theme(panel.background = element_rect(fill = "white",colour = "white"),
          panel.grid.major = element_line(size = 0.2, linetype = 3,colour = "black"),
          panel.border = element_rect(fil=NA, colour="black"))

## 5-2- Which 5-min interval, on average across all the days in the dataset, contains the maximum number of steps ? 
StepsIntMean[StepsIntMean$steps_mean==max(StepsIntMean$steps_mean),]

# 6- Inputing missing values
## 6-1- Calculate and report total number of missing values in the dataset
sum(is.na(amd$steps)==TRUE)

## 6-2- Strategy for filling in 
#First look where the NA are mostly represented: 
is.na(StepsDaySum$steps)
#It looks like entire days are missing values, how many days is it ?
sum(is.na(StepsDaySum$steps)==TRUE)
#How many obersvations are there per day again ?
length(unique(amd$interval))
#Are all the missing informations coming from single days missing values ?
sum(is.na(StepsDaySum$steps)==TRUE)*length(unique(amd$interval))

## 6-3- Create a new dataset with filled in missing values
StepsIntMean$steps_median<-as.numeric(tapply(amd$steps, amd$interval, median, na.rm=T))
StepsIntMelt<-melt(as.data.table(StepsIntMean), id.vars=c("interval"))

StepsInt_ggplot<-ggplot(data=StepsIntMelt)+
    geom_line(aes(x=interval,y=value,linetype=variable))+
    ggtitle("Mean and median number of steps per 5 minutes interval")+
    xlab("Time (minutes)")+ylab("Number of steps")+
    theme(panel.background = element_rect(fill = "white",colour = "white"),
          panel.grid.major = element_line(size = 0.2, linetype = 3,colour = "black"),
          panel.border = element_rect(fil=NA, colour="black"))
StepsInt_ggplot

#with median
amd2<-copy(amd)
for (i in amd2$interval) {
    amd2[amd2$interval==i & is.na(amd2$steps)==T, c("steps")]<-StepsIntMean[StepsIntMean$interval==i,c("steps_median")]
}

#with mean
amd3<-copy(amd)
for (i in amd3$interval) {
    amd3[amd3$interval==i & is.na(amd3$steps)==T, c("steps")]<-StepsIntMean[StepsIntMean$interval==i,c("steps_mean")]
}

## 6-4- Histogram of total number of steps taken each day

#With median: 
StepsDaySum2<-tapply(amd2$steps, amd2$date, sum)
StepsDaySum2<-as.data.frame(StepsDaySum2)
colnames(StepsDaySum2)<-c("steps")
StepsDaySum2$date<-rownames(StepsDaySum2)
rownames(StepsDaySum2)<-seq_along(StepsDaySum2$steps)

StepsDaySum2_hist<-hist(StepsDaySum2$steps, main="Total number of steps taken each day with NA filled with median value of the same interval",
                        xlab="Number of steps")
StepsDaySum2_hist

#With mean: 
StepsDaySum3<-tapply(amd3$steps, amd3$date, sum)
StepsDaySum3<-as.data.frame(StepsDaySum3)
colnames(StepsDaySum3)<-c("steps")
StepsDaySum3$date<-rownames(StepsDaySum3)
rownames(StepsDaySum3)<-seq_along(StepsDaySum3$steps)

StepsDaySum3_hist<-hist(StepsDaySum3$steps, main="Total number of steps taken each day with NA filled with mean value of the same interval",
                        xlab="Number of steps")
StepsDaySum3_hist

## 6-5- Mean and median of total number of steps taken each day

#With median
StepsDaySum2_mean<-mean(StepsDaySum2$steps, na.rm=T)
StepsDaySum2_median<-median(StepsDaySum2$steps, na.rm = T)
StepsDaySum2_mean
StepsDaySum2_median

#With mean
StepsDaySum3_mean<-mean(StepsDaySum3$steps, na.rm=T)
StepsDaySum3_median<-median(StepsDaySum3$steps, na.rm = T)
StepsDaySum3_mean
StepsDaySum3_median

### 6-5-1- Do these values differ from the first estimates ?

### 6-5-2- What is the impact of imputing missing data on the estimates of the total daily number of steps ? 

# 7-Differences in activity patterns between weekdays and weekends
## 7-1- New factor variable 
amd$day<-weekdays(as.Date(amd$date))
amd$we<-weekdays(as.Date(amd$date))
weekdays<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
amd[is.element(amd$we, weekdays)==F,c("we")]<-c("weekend")
amd[is.element(amd$we, weekdays)==T,c("we")]<-c("weekday")

## 7-2- Time series plot

amdwd<-amd[is.element(amd$day, weekdays)==T,]

StepsIntWD<-tapply(amdwd$steps, amdwd$interval, mean, na.rm=T)
StepsIntWD<-as.data.frame(StepsIntWD)
colnames(StepsIntWD)<-c("steps_mean")
StepsIntWD$interval<-rownames(StepsIntWD)
StepsIntWD$interval<-as.numeric(StepsIntWD$interval)
rownames(StepsIntWD)<-seq_along(StepsIntWD$interval)

StepsIntWD_ggplot<-ggplot(data=StepsIntWD, aes(x=interval, y=steps_mean))+
    geom_point()+geom_line()+
    ggtitle("Mean numer of steps per 5 minutes interval on weekdays")+
    xlab("Time (minutes)")+ylab("Mean number of steps")+
    theme(panel.background = element_rect(fill = "white",colour = "white"),
          panel.grid.major = element_line(size = 0.2, linetype = 3,colour = "black"),
          panel.border = element_rect(fil=NA, colour="black"))

amdwe<-amd[is.element(amd$day, weekdays)==F,]                                  

StepsIntWE<-tapply(amdwe$steps, amdwe$interval, mean, na.rm=T)
StepsIntWE<-as.data.frame(StepsIntWE)
colnames(StepsIntWE)<-c("steps_mean")
StepsIntWE$interval<-rownames(StepsIntWE)
StepsIntWE$interval<-as.numeric(StepsIntWE$interval)
rownames(StepsIntWE)<-seq_along(StepsIntWE$interval)

StepsIntWE_ggplot<-ggplot(data=StepsIntWE, aes(x=interval, y=steps_mean))+
    geom_point()+geom_line()+
    ggtitle("Mean numer of steps per 5 minutes interval on weekends")+
    xlab("Time (minutes)")+ylab("Mean number of steps")+
    theme(panel.background = element_rect(fill = "white",colour = "white"),
          panel.grid.major = element_line(size = 0.2, linetype = 3,colour = "black"),
          panel.border = element_rect(fil=NA, colour="black"))

StepsIntTOTAL_ggplot<-grid.arrange(StepsIntWD_ggplot, StepsIntWE_ggplot)
