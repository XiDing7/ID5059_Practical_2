#Libraries
library(tidyverse)

#Importing the data 
Data<- read.csv("./2008.csv", header = TRUE)
Dat <- Data%>%select(-CRSDepTime,-ArrTime,-CRSArrTime,-TailNum,
                     -ActualElapsedTime, -CRSElapsedTime,-AirTime,-TaxiIn,
                     -TaxiOut,-Diverted)%>%mutate(FlightNum =
                                                    as.factor(FlightNum))
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
TotalDistance <- Dat%>%group_by(UniqueCarrier, FlightNum)%>%summarise(Sum_Dist =
                                                            sum(Distance),
                                                         Sum_Can = 
                                                           sum(Cancelled),
                                                         count = n())
arrange(TotalDistance, desc(Sum_Dist))[1:3,]

#Number of flights by days of week and months:
TableHM<-as.data.frame(table(Month,DayOfWeek))

#It can also be handy to color such tables in order to easily notice outliers:
ggplot(TableHM) + geom_tile(aes(x= Month, y = DayOfWeek, fill = Freq)) +
  scale_fill_gradient(low="yellow", high="red") 

#Flight distance histogram:
ggplot(Dat)+ geom_histogram(aes(Distance), bins = 20)

#Making a histogram of flight frequency by date.
toDate <- function(year, month, day) {
  ISOdate(year, month, day)
}
Dat$Date <- toDate(Dat$Year,Dat$Month,Dat$DayofMonth)
TableDate <- as.data.frame(table(Date = Dat$Date))
TableDate$Date <- as.Date(TableDate$Date)
ggplot(TableDate)+geom_line(aes(x = Date, y = Freq))+
  scale_x_date(date_labels = "%b")

#Do you see a weekly pattern above? And below?
Dat$Week <- as.Date(cut(Dat$Date,
                        breaks = "week",
                        start.on.monday = FALSE))
TableWeek<- as.data.frame(table(Week =Dat$Week))[2:52,]
TableWeek$Week <- as.Date(TableWeek$Week)
ggplot(TableWeek)+geom_line(aes(x = Week, y = Freq)) +
  scale_x_date(date_labels = "%b")

#We'll need a new column in our dataset - departure hour, let's create it.
Dat$DepHour <-floor(Dat$DepTime/100)
Dat$DepHour[Dat$DepHour == 24]<- 0 
summary(Dat$DepHour)

#1. How many unique carriers are there in our dataset?
#20
levels(UniqueCarrier)

#2. We have both cancelled and completed flights in the dataset. 
#Check if there are more completed or cancelled flights. What is the difference? 
#Completed overweights cancelled by 6734860 flights
Dat%>%filter(Cancelled==0 & !is.na(Cancelled))%>%summarise(n()) -
  Dat%>%filter(Cancelled==1 & !is.na(Cancelled))%>%summarise(n())
  
#3. Find a flight with the longest departure delay and a flight with the longest
#arrival delay. Do they have the same destination airport, and if yes, what is 
#its code?
#yes, MSP
Dat[which(Dat$DepDelay == max(na.omit(Dat$DepDelay))),]
Dat[which(Dat$ArrDelay == max(na.omit(Dat$ArrDelay))),]

#4. Find the carrier that has the greatest number of cancelled flights.
#MQ
GTCancelled <- Dat%>%group_by(UniqueCarrier)%>% summarise(Sum = sum(Cancelled))
arrange(GTCancelled, desc(Sum))

#5. Let's examine departure time and consider distribution by hour (column
#DepHour that we've created earlier). Which hour has the highest percentage 
#of flights?
#8am
DH <- Dat%>%group_by(DepHour)%>%summarise(n=n())
arrange(DH, desc(n))

#6. OK, now let's examine cancelled flight distribution by time. 
#Which hour has the least percentage of cancelled flights?
#2am
Time1 <- Dat%>%group_by(DepHour)%>%summarise(Sum = sum(Cancelled))
arrange(Time1, Sum)

#7. Is there any hour that didn't have any cancelled flights at all?
#Check all that apply.
#3am

#8. Find the busiest hour, or in other words, the hour when the number of
#departed flights reaches its maximum.
#7am
arrange(Time1, desc(Sum))

#9. Since we know the departure hour, it might be interesting to examine the 
#average delay for corresponding hour. Are there any cases, when the planes on 
#average departed earlier than they should have done? And if yes, at what
#departure hours did it happen?
#Yes,5-6am 
AD <-Dat%>% group_by(DepHour)%>%summarise(Mean = mean(DepDelay))
arrange(AD, Mean)

#10. Considering only the completed flights by the carrier, that you have found 
#in Question 4, find the distribution of these flights by hour. At what time 
#does the greatest number of its planes depart?
Completed <- Dat%>%filter(Cancelled == 0)
TableComp <- as.data.frame(table(Month = Completed$Month,
                                 Completed = Completed$Cancelled))
ggplot(TableComp)+geom_bar(aes(x =Month,y = Completed, fill= Month))

#11. Find top-10 carriers in terms of the number of completed flights 
#(UniqueCarrier column)?
#VE
NotCancelled <- Dat %>% group_by(UniqueCarrier)%>%filter(Cancelled == 0)%>% 
  summarise(n=n())
arrange(NotCancelled, desc(n))

#12. Plot distributions of flight cancellation reasons (CancellationCode).
#What is the most frequent reason for flight cancellation?
#Weather
ForCC <- Dat%>%filter(Cancelled != 0)
TableCode <- as.data.frame(table(CancellationCode = ForCC$CancellationCode))
ggplot(TableCode, aes(x=CancellationCode, y=Freq, fill=CancellationCode)) +
  geom_bar(stat="identity") + theme(legend.position = "none")

#13. Which route is the most frequent, in terms of the number of flights?
#San-Francisco – Los-Angeles (SFO-LAX)
Frequ <-  Dat%>%group_by(Origin, Dest)%>%summarise(Number = n())
arrange(Frequ, desc(Number))

#14.Find top-5 delayed routes (count how many times they were delayed on
#departure).From all flights on these 5 routes, count all flights with weather
#conditions contributing to a delay.

Delay <- Dat%>%group_by(Origin, Dest)%>%filter(count(!is.na(DepDelay)))
Delay%>%filter(!is.na(WeatherDelay))%>% summarise(n = n())

#15. Examine the hourly distribution of departure times. 
#Choose all correct statements:
#Flights are uniformly distributed within time interval [0-23] and
#In the period from 0 am to 4 am there are considerably less 
#flights than from 7 pm to 8 pm.
ggplot(Dat)+geom_histogram(aes(DepTime), bins = 20)

#16. Show how the number of flights changes through time (on the 
#daily/weekly/monthly basis) and interpret the findings.
Dat$month <- as.Date(cut(Dat$Date,
                        breaks = "month"))
Tablemonth<- as.data.frame(table(month =Dat$month))
Tablemonth$month <- as.Date(Tablemonth$month)
ggplot(Tablemonth)+geom_line(aes(x = month, y = Freq)) +
  scale_x_date(date_labels = "%b")


#17. Examine the distribution of cancellation reasons with time. 
#Make a bar plot of cancellation reasons aggregated by months.
TableCR <- as.data.frame(table(Month,CancellationCode)) %>% filter(!CancellationCode == "")
ggplot(TableCR, aes(x=Month, y=Freq, fill=CancellationCode)) +
  geom_bar(stat="identity")

#18. Which month has the greatest number of cancellations due to Carrier?
#April
CD<- Dat%>%group_by(Month)%>%filter(CancellationCode == 'A')%>%summarise(n = n())
arrange(CD,desc(n))

#19. Identify the carrier with the greatest number of cancellations due to 
#carrier in the corresponding month from the previous question.
#AA
PlotCD<- Dat%>%group_by(Month)%>%filter(CancellationCode == 'A' & Month == 4)
TableCD <- as.data.frame(table(UniqueCarrier = PlotCD$UniqueCarrier))
ggplot(TableCD, aes(x=UniqueCarrier, y=Freq, fill=UniqueCarrier)) +
  geom_bar(stat="identity") + theme(legend.position = "none")

#20. Examine median arrival and departure delays (in time) by carrier. Which 
#carrier has the lowest median delay time for both arrivals and departures?
#Leave only non-negative values of delay times ('ArrDelay', 'DepDelay'). 
#(Boxplots can be helpful in this exercise, as well as it might be a good idea 
#to remove outliers in order to build nice graphs. You can exclude delay time 
#values higher than a corresponding .95 percentile).
#Removing all flights with no delays
#9E
NewData <- Dat%>%filter(!is.na(ArrDelay), !is.na(DepDelay))
NewData%>%group_by(UniqueCarrier)%>%
  summarise(Med_Dep = median(DepDelay),Med_Arr = median(ArrDelay))
#Removing outliers for delays by using the rule mean +-2*sd
pick1 <- which(NewData$ArrDelay > mean(NewData$ArrDelay) + 2*sd(NewData$ArrDelay)|
                 NewData$ArrDelay < mean(NewData$ArrDelay) - 2*sd(NewData$ArrDelay))
ArrData <- NewData[-pick1,]
pick2 <- which(NewData$DepDelay > mean(NewData$DepDelay) + 2*sd(NewData$DepDelay)|
                 NewData$DepDelay < mean(NewData$DepDelay) - 2*sd(NewData$DepDelay))
DepData <- NewData[-pick2,]
#Boxplot of values
ggplot(ArrData)+geom_boxplot(aes(x = UniqueCarrier, y = ArrDelay))
ggplot(DepData)+geom_boxplot(aes(x = UniqueCarrier, y = DepDelay))
