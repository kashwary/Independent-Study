library(ggplot2) # graphics library
library(gridExtra) #arranging plot in ggplot
library(tidyverse)
library(scales)
library(RColorBrewer)
library(stringr)

#importing data
setwd("C:/Kumar/CMU Course/CMU Course/Independent Study")
#working with 2020 and 2021 data
data1 <- read.csv("FULL DOWNLOAD.csv")
#adding cleaned date column
data1$date3 <- substr(data1$Date,start = 1,stop = 7)
data1$date3 <- as.factor(data1$date3)
data2 <- data1[-c(2,3,8,11,12)]



#importing 3 2019 data files
data20191 <- read.csv("2019.CY.SW data_export_Carnegie.csv")
#removing month column, the extra column
data20191 <- data20191[-c(2)]

data20192 <- read.csv("2019.CY.NW data_export1_Carnegie.csv")
data20193 <- read.csv("CY SE Export 19_Carnegie.csv")

#combining the three datasets. 
data2019 <- rbind(data20191,data20192,data20193)


colnames(data2019)
colnames(data1)

data2019 <- data2019[c(1,3,5,10,16,17,20,23,26)]
data2019 <- data2019[-c(3)]

#changing date type for the date column
data2019$Contact..System.Create.Date <- as.Date(data2019$Contact..System.Create.Date, "%m/%d/%Y")


#changing column names
colnames(data2019) <- c("date3",
                        "Record.Owner",
                        "Type.of.Call",
                        "Transaction.ID",
                        "Search.Age",
                        "Search.County",
                        "Zip",
                        "Search.Gender")



data2019$date3 <- substr(data2019$date3,start = 1,stop = 7)



col_order <- c("Transaction.ID","Type.of.Call","Record.Owner","Search.Gender","Zip","Search.Age","Search.County","date3")

data2019 <- data2019[, col_order]


finaldata <- rbind(data2019,data2)
#total calls
n_distinct(finaldata$Transaction.ID,na.rm =TRUE)
#total referrals
nrow(na.omit(finaldata))
#average referrals
nrow(na.omit(finaldata))/n_distinct(finaldata$Transaction.ID,na.rm =TRUE)
#getting distinct entries to analyse call data
finaldata1 <-  distinct(finaldata,Transaction.ID,Type.of.Call,Search.Gender)
#adding codes for male and female
finaldata1 <- finaldata1 %>% mutate(Gender = case_when(endsWith(Search.Gender, "Female")~'F',
                                                       endsWith(Search.Gender, "Male")~'M',
                                                       endsWith(Search.Gender, "M")~'M',
                                                       endsWith(Search.Gender, "D")~'D',
                                                       endsWith(Search.Gender, "F")~'F',
                                                       endsWith(Search.Gender, "G")~'G',
                                                       endsWith(Search.Gender, "T")~'T',
                                                       endsWith(Search.Gender, "C")~'C',
                                                       endsWith(Search.Gender, "Undetermined")~'Undetermined',
                                                       endsWith(Search.Gender, "Declined")~'Declined',))
finaldata1 <- finaldata1 %>% group_by(Gender) %>% mutate(TotalCalls = length(unique(Transaction.ID)))
#keeping only distinct entries
finaldata1 <-  na.omit(distinct(finaldata1,Gender,TotalCalls))
sum(finaldata1$TotalCalls)
finaldata1$TC <- round(finaldata1$TotalCalls/232856,2)
#creating gender graph
ggplot(finaldata1, aes(y=TC,x=Gender)) +  
  geom_bar(position="dodge",stat="identity") + 
  geom_text(aes(label= TC),vjust=-1, color="Black", size=3, hjust = 1)+ 
  ggtitle("Gender Distribution For Calls") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle = 90))+
  theme(legend.position = "none")

#creating age bins
labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))

finaldata$AgeGroup <- cut(finaldata$Search.Age, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

finaldata$AgeGroup <- as.factor(finaldata$AgeGroup)


#now creating dataset for age

mean(finaldata$Search.Age,na.rm = TRUE)
median(finaldata$Search.Age,na.rm = TRUE)

#getting call distribution for age
#getting distinct entries to analyse call data
AgeData1 <-  distinct(finaldata,Transaction.ID,Type.of.Call,AgeGroup)
#caclculating the number of calls
AgeData2 <- AgeData1 %>% group_by(AgeGroup) %>% mutate(TotalCalls = length(unique(Transaction.ID)))
#getting only distinct values
AgeData2 <-  na.omit(distinct(AgeData2,AgeGroup,TotalCalls))

sum(AgeData2$TotalCalls)

AgeData2$TC <-  round((AgeData2$TotalCalls/155899)*100,2)

#creating age graph

#creating gender graph
ggplot(AgeData2, aes(y=TC,x=AgeGroup)) +  
  geom_bar(position="dodge",stat="identity") + 
  geom_text(aes(label= TC),vjust=0, color="Black", size=4, hjust = 1, angle=90)+ 
  ggtitle("Age Distribution For Calls") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle = 90))+
  theme(legend.position = "none")


#getting average call time
#getting distinct entries to analyse call data
data3 <-  distinct(data1,Transaction.ID,Total.Time,Search.Age)
#calculating average call time
mean(data3$Total.Time,na.rm = TRUE)/60
#creating age bins

#labs is already created earlier 

data3$AgeGroup <- cut(data3$Search.Age, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)

data3$AgeGroup <- as.factor(data3$AgeGroup)

#now totaltime for age
data4 <- data3 %>% group_by(AgeGroup) %>% mutate(TotalCalls = length(unique(Transaction.ID)),TotalTime= sum(Total.Time))
#keeping distinct values
data4 <-  na.omit(distinct(data4,AgeGroup,TotalCalls,TotalTime))
#calculating average time per call
data4$AvgTime <- round((data4$TotalTime/data4$TotalCalls)/60,2)
#now creating the graph for age distribution and average call time
#creating gender graph
ggplot(data4, aes(y=AvgTime,x=AgeGroup)) +  
  geom_bar(position="dodge",stat="identity") + 
  geom_text(aes(label= AvgTime),vjust=0, color="Black", size=4, hjust = 1, angle=90)+ 
  ggtitle("Age Distribution For Average Call Time") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle = 90))+
  theme(legend.position = "none")

#now counting total calls for each month
Datamonth <-  distinct(finaldata,Transaction.ID,Search.Gender,date3,Type.of.Call)

#changing gender tagging 
#adding codes for male and female
Datamonth <- Datamonth %>% mutate(Gender = case_when(endsWith(Search.Gender, "Female")~'F',
                                                       endsWith(Search.Gender, "Male")~'M',
                                                       endsWith(Search.Gender, "M")~'M',
                                                       endsWith(Search.Gender, "D")~'D',
                                                       endsWith(Search.Gender, "F")~'F',
                                                       endsWith(Search.Gender, "G")~'G',
                                                       endsWith(Search.Gender, "T")~'T',
                                                       endsWith(Search.Gender, "C")~'C',
                                                       endsWith(Search.Gender, "Undetermined")~'Undetermined',
                                                       endsWith(Search.Gender, "Declined")~'Declined',))
#now counting calls for each month
Datamonth1 <- Datamonth %>% group_by(date3) %>% mutate(TotalCalls = length(unique(Transaction.ID)))

Datamonth1 <-  na.omit(distinct(Datamonth1,date3,TotalCalls))

ggplot(data=Datamonth1,aes(y=TotalCalls,x=date3,group=1)) + geom_line() + geom_point()+
ggtitle("Call Distribution Over Month") + xlab("Date")+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle = 90))


#average calls over months
sum(Datamonth1$TotalCalls)/27

#now calculating things over period of time with type of call as a distribution
Datamonth2 <- Datamonth %>% group_by(date3,Type.of.Call) %>% mutate(TotalCalls = length(unique(Transaction.ID)))

#keeping only distinct values and removing na values
Datamonth2 <-  na.omit(distinct(Datamonth2,date3,TotalCalls,Type.of.Call))

Datamonth2<- Datamonth2[Datamonth2$Type.of.Call != "#N/A"& Datamonth2$Type.of.Call != "null" & Datamonth2$Type.of.Call != "", ] 
#now creating a stacked chart
ggplot(Datamonth2,aes(fill=Type.of.Call, y=TotalCalls, x=date3,na.rm=TRUE))+ 
  geom_bar(position="fill", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Call Type")+
  ylab("Call Distribution By Call Type")
                                                                              