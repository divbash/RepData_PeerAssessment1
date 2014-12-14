---
title: " Reproducible Research : Assignment1"
output: html_document
---
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  



```{r,echo=FALSE}

csv.data<-read.csv("C:\\Users\\shenoy\\Desktop\\John Hopkins Data Science Course\\5 Reproducible Research\\repdata_data_activity\\activity.csv")
tap<-tapply(csv.data$steps,csv.data$date,sum)
hist(tap,main = paste("Histogram of total number of  \n steps taken each day"),xlab="Total steps per day",col="dark blue")
tap<-tap[!is.na(tap)]
```
    
Mean total number of steps per day
```{r,echo=FALSE}
print(mean(tap))
```
  
Median total number of steps per day 
```{r,echo=FALSE}
print(median(tap))

```

### **Average Daily activity pattern**  
```{r,echo=TRUE}
fdata<-csv.data[!is.na(csv.data$steps),]
interval<- substr(as.POSIXct(sprintf("%04.0f", fdata$interval), format='%H%M'), 12, 16)

mydata<-cbind(fdata[,1:2],interval)
agg<- aggregate(mydata$steps,by=list(mydata$interval),FUN="mean",na.action=na.omit)
plot(agg$Group.1,agg$x,type="n",xlab="Time Interval",ylab="Average number of steps",main="Time series plot \n Average daily activity pattern")
lines(agg$Group.1,agg$x,type="l")

```
  
**Interval with maximum number of  Daily average steps**  
```{r,echo=TRUE}
agg[which.max(agg$x),]
```


On average across all days,the time interval from 8:35 to 8:40 has the maximum number steps.

```{r,echo=FALSE}
agg[103:105,]
```

### **Imputing missing values**    
   
**Calculating the total number of "NA" values in the dataset  **
```{r}
sum(is.na(csv.data$steps))
```
    
  **Strategy**  
  
   The strategy implemented here is  
   Step 1. All the rows with NA's have been replaced with 0  
   Step 2. Average number of steps for each day is calculated and filled into the steps column for the corresponding dates.  
   Step 3. The new  dataset is created similar to the original one but with the values calculated in step 2 are filled in the "steps" column of the new dataset.The Code chunk implementing the strategy is as follows:  
   
 **Replacing NA with 0**       
```{r,echo=TRUE}

csv.data[is.na(csv.data)]<-0
```

**Creating new dataset with filled in values**  
```{r,echo=TRUE}
meandaily<-tapply(csv.data$steps,csv.data$date,mean) 
data1<-cbind(0,csv.data)  
colnames(data1)<-c("meanSteps","steps","date","interval")
```

**Implementing the steps in the Strategy and imputing the values by using the daily average number of steps**
```{r,echo=TRUE}
dates <- unique(csv.data$date)
for(i in 1:length(dates)){
   data1[data1$date==dates[i],1]<-meandaily[i]
}

datanew<-data1[,c(1,3,4)]##New dataset is created
 totalsteps<-tapply(datanew$meanSteps,datanew$date,sum)
 hist(totalsteps,main = paste("Histogram of total number of  \n steps taken each day after imputing"),xlab="Total steps per day",col="red")
```
  
**Mean and Median values of total steps taken each day after imputing**    
```{r,echo=TRUE}
mean(totalsteps)
median(totalsteps)
```
  
  The values of mean and Median do change after imputing as compared to the first part. If we compare both the histograms plotted, we can see the change in shape .  
  Earlier, the data with "NA's" was dropped from the calculationsof mean/sum/median.After imputing with the daily mean, where the missing data has been filled with O, is considered as an observation and is not dropped from the calculations of mean/sum/median. 
  
### Differences in activity patterns between weekdays and weekends

```{r,echo=TRUE}
csv.data$day<-weekdays(as.Date(csv.data$date))
csv.data$day<-ifelse(csv.data$day %in% c("Saturday","Sunday"),"Weekend","Weekday")
aggdat<-aggregate(csv.data$steps,by=list(csv.data$interval,csv.data$day),FUN="mean")
aggdat$Group.2<-as.factor(aggdat$Group.2)
```
  

**Panel plot for weekdays and weekends**  
```{r,echo=TRUE}
library(lattice)

xyplot(aggdat$x~aggdat$Group.1|aggdat$Group.2,layout=c(1,2),type="l",xlab="Interval",ylab="Number of Steps")
```

  
  
   
  
   
   

