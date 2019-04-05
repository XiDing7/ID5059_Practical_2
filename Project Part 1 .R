#Libraries
library(tidyverse)

#Importing the data 
Data<- read.csv("./2008.csv", header = TRUE)
Dat <- Data%>%select(-CRSDepTime,-ArrTime,-CRSArrTime,-TailNum,-ActualElapsedTime,
                     -CRSElapsedTime,-AirTime,-TaxiIn,-TaxiOut,-Diverted)%>%
  mutate(FlightNum = as.factor(FlightNum))
attach(Dat)

#Check the number of rows and columns and print column names.
dim(Dat)
colnames(Dat)

#Print first 5 rows of the dataset.
head(Dat,n = 5)

#Transpose the frame to see all features at once.
t(head(Dat,n = 5))

#Examine data types of all features and total dataframe size in memory.
str(Dat)

#Get basic statistics of each feature.
summary(Dat)

#Count unique Carriers and plot their relative share of flights:
levels(UniqueCarrier)
freqTable <- as.data.frame(table(UniqueCarrier))
ggplot(freqTable, aes(x=UniqueCarrier, y=Freq, fill=UniqueCarrier)) +
  geom_bar(stat="identity") + theme(legend.position = "none")

#For example, finding top-3 flight codes, that have the largest total distance 
#traveled in year 2008.
m <- Dat%>%group_by(UniqueCarrier, FlightNum)%>%summarise(Sum_Dist =sum(Distance),
                                                         Sum_Can = sum(Cancelled),
                                                         count = n())
arrange(m, desc(Sum_Dist))[1:3,]

#Number of flights by days of week and months:
table(Month,DayOfWeek)

#It can also be handy to color such tables in order to easily notice outliers:
plot(table(Month,DayOfWeek)) 

#Flight distance histogram:
ggplot(Dat)+ geom_histogram(aes(Distance), bins = 20)

#Making a histogram of flight frequency by date.


#We'll need a new column in our dataset - departure hour, let's create it.
Dat$DepHour <- Dat$DepTime/100
Dat$DepHour[Dat$DepHour == 24]<- 0 
summary(Dat$DepHour)

#1. How many unique carriers are there in our dataset?
#20
levels(UniqueCarrier)

#2. We have both cancelled and completed flights in the dataset. 
#Check if there are more completed or cancelled flights. What is the difference? 
#Completed overweights cancelled by 6734860 flights
nrow(Dat) - sum(Cancelled) - sum(Cancelled) 
  
#3. Find a flight with the longest departure delay and a flight with the longest
#arrival delay. Do they have the same destination airport, and if yes, what is its code?
#yes, MSP
Dat[which(Dat$DepDelay == 2467),]
Dat[which(Dat$ArrDelay == 2461),]

#4. Find the carrier that has the greatest number of cancelled flights.
#MQ
n <- Dat%>%group_by(UniqueCarrier)%>% summarise(Sum = sum(Cancelled))
arrange(n, desc(Sum))

