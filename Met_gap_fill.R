rm(list=ls(all=TRUE)) #give R a blank slate
#First, you need to install packages car, date, and reshape2

#Code for indexing data to fill gaps________________________________________________

setwd("C:/Users/Jennifer Rudgers/Desktop/SEV Data Course/Met data")

library(car)
library("reshape2")
library(date)
#Upload all data
#data <- read.csv("/Users/Jennifer Rudgers/Desktop/SEV Data Course/Met data/Met_all.csv")
data<-read.csv("Met_all.csv")

#Add column for Julian day: Given a month, day, and year, returns the number of days since January 1, 1960
data$Jul_Day <-mdy.date(data$Month, data$Day, data$Year, nineteen= TRUE, fillday = FALSE, fillmonth = FALSE)


#Subset data by Sta (station)
data01<-subset(data, Sta==1)#only 1 (starts 1/1/92)
data40<-subset(data, Sta==40)#only 40 (starts 1/1/88)
data41<-subset(data, Sta==41)#only 41 (starts 2/15/89)
data42<-subset(data, Sta==42)#only 42 (starts 2/22/89)
data43<-subset(data, Sta==43)#only 43 (starts 2/17/89)
data44<-subset(data, Sta==44)#only 44 (starts 3/31/89)
data45<-subset(data, Sta==45)#only 45 (starts 3/31/89)
data48<-subset(data, Sta==48)#only 48 (starts 10/8/98)
data49<-subset(data, Sta==49)#only 49 (starts 1/1/99)
data50<-subset(data, Sta==50)#only 50 (starts 1/26/02)

#Make the substituted data a new dataframe for QA/QC purposes
#Also, that way the old data subset can be used to gap-fill other data
newdata01<-subset(data, Sta==01)
newdata40<-subset(data, Sta==40)
newdata41<-subset(data, Sta==41)
newdata42<-subset(data, Sta==42)
newdata43<-subset(data, Sta==43)
newdata44<-subset(data, Sta==44)
newdata45<-subset(data, Sta==45)
newdata48<-subset(data, Sta==48)
newdata49<-subset(data, Sta==49)
newdata50<-subset(data, Sta==50)

#Gap-filling algorithm for met40 from met49--------------------------------------
#Also keep track of error rates for met40
errors40<-data.frame(Variable=names(data40)[6:(length(data40[1,])-1)],Error=rep(0,length(data40[1,])-6))
#Start loop
for (j in 6:(length(newdata40[1,])-1)){#loop only for measured vars
  k<-0
for (i in 1:length(newdata40[,1])){
  if(newdata40[i,j]>(-500)) next #don't do anything to good data
  else {
    k<-k+1
    if (newdata40$Jul_Day[i]%in%data49$Jul_Day) {
      fillIndex<-which(data49$Jul_Day==newdata40$Jul_Day[i])#finds matching row in data49
      if(data49[fillIndex,j]>(-500)){
        newdata40[i,j]<-data49[fillIndex,j]
      }
      else newdata40[i,j]<-NA
    }
    else newdata40[i,j]<-NA
  }
}
errors40$Error[j-5]<-k
}
errors40#error rates of instruments vary


#Gap-filling algorithm for met41 from met49---------------------------------------
#Also keep track of error rates for met41
errors41<-data.frame(Variable=names(data41)[6:(length(data41[1,])-1)],Error=rep(0,length(data41[1,])-6))
#Start loop
for (j in 6:(length(newdata41[1,])-1)){#loop only for measured vars
  k<-0
  for (i in 1:length(newdata41[,1])){
    if(newdata41[i,j]>(-500)) next #don't do anything to good data
    else {
      k<-k+1
      if (newdata41$Jul_Day[i]%in%data49$Jul_Day) {
        fillIndex<-which(data49$Jul_Day==newdata41$Jul_Day[i])#finds matching row in data49
        if(data49[fillIndex,j]>(-500)){
          newdata41[i,j]<-data49[fillIndex,j]
        }
        else newdata41[i,j]<-NA
      }
      else newdata41[i,j]<-NA
    }
  }
  errors41$Error[j-5]<-k
}
errors41

#Gap-filling algorithm for met42 from met48------------------------------------
#Also keep track of error rates for met42
errors42<-data.frame(Variable=names(data42)[6:(length(data42[1,])-1)],Error=rep(0,length(data42[1,])-6))
#Start loop
for (j in 6:(length(newdata42[1,])-1)){#loop only for measured vars
  k<-0
  for (i in 1:length(newdata42[,1])){
    if(newdata42[i,j]>(-500)) next #don't do anything to good data
    else {
      k<-k+1
      if (newdata42$Jul_Day[i]%in%data48$Jul_Day) {
        fillIndex<-which(data48$Jul_Day==newdata42$Jul_Day[i])#finds matching row in data48
        if(data48[fillIndex,j]>(-500)){
          newdata42[i,j]<-data48[fillIndex,j]
        }
        else newdata42[i,j]<-NA
      }
      else newdata42[i,j]<-NA
    }
  }
  errors42$Error[j-5]<-k
}
errors42


#Gap-filling algorithm for met43 from met45-----------------------------------
#Also keep track of error rates for met43
errors43<-data.frame(Variable=names(data43)[6:(length(data43[1,])-1)],Error=rep(0,length(data43[1,])-6))
#Start loop
for (j in 6:(length(newdata43[1,])-1)){#loop only for measured vars
  k<-0
  for (i in 1:length(newdata43[,1])){
    if(newdata43[i,j]>(-500)) next #don't do anything to good data
    else {
      k<-k+1
      if (newdata43$Jul_Day[i]%in%data45$Jul_Day) {
        fillIndex<-which(data45$Jul_Day==newdata43$Jul_Day[i])#finds matching row in data45
        if(data45[fillIndex,j]>(-500)){
          newdata43[i,j]<-data45[fillIndex,j]
        }
        else newdata43[i,j]<-NA
      }
      else newdata43[i,j]<-NA
    }
  }
  errors43$Error[j-5]<-k
}
errors43


#Gap-filling algorithm for met44 from met01-------------------------------------
#Also keep track of error rates for met44
errors44<-data.frame(Variable=names(data44)[6:(length(data44[1,])-1)],Error=rep(0,length(data44[1,])-6))
#Start loop
for (j in 6:(length(newdata44[1,])-1)){#loop only for measured vars
  k<-0
  for (i in 1:length(newdata44[,1])){
    if(newdata44[i,j]>(-500)) next #don't do anything to good data
    else {
      k<-k+1
      if (newdata44$Jul_Day[i]%in%data01$Jul_Day) {
        fillIndex<-which(data01$Jul_Day==newdata44$Jul_Day[i])#finds matching row in data01
        if(data01[fillIndex,j]>(-500)){
          newdata44[i,j]<-data01[fillIndex,j]
        }
        else newdata44[i,j]<-NA
      }
      else newdata44[i,j]<-NA
    }
  }
  errors44$Error[j-5]<-k
}
errors44

#Gap-filling algorithm for met45 from met43--------------------------------------
#Also keep track of error rates for met45
errors45<-data.frame(Variable=names(data45)[6:(length(data45[1,])-1)],Error=rep(0,length(data45[1,])-6))
#Start loop
for (j in 6:(length(newdata45[1,])-1)){#loop only for measured vars
  k<-0
  for (i in 1:length(newdata45[,1])){
    if(newdata45[i,j]>(-500)) next #don't do anything to good data
    else {
      k<-k+1
      if (newdata45$Jul_Day[i]%in%data43$Jul_Day) {
        fillIndex<-which(data43$Jul_Day==newdata45$Jul_Day[i])#finds matching row in data43
        if(data43[fillIndex,j]>(-500)){
          newdata45[i,j]<-data43[fillIndex,j]
        }
        else newdata45[i,j]<-NA
      }
      else newdata45[i,j]<-NA
    }
  }
  errors45$Error[j-5]<-k
}
errors45

#Gap-filling algorithm for met48 from met42-------------------------------------
#Also keep track of error rates for met48
errors48<-data.frame(Variable=names(data48)[6:(length(data48[1,])-1)],Error=rep(0,length(data48[1,])-6))
#Start loop
for (j in 6:(length(newdata48[1,])-1)){#loop only for measured vars
  k<-0
  for (i in 1:length(newdata48[,1])){
    if(newdata48[i,j]>(-500)) next #don't do anything to good data
    else {
      k<-k+1
      if (newdata48$Jul_Day[i]%in%data42$Jul_Day) {
        fillIndex<-which(data42$Jul_Day==newdata48$Jul_Day[i])#finds matching row in data42
        if(data42[fillIndex,j]>(-500)){
          newdata48[i,j]<-data42[fillIndex,j]
        }
        else newdata48[i,j]<-NA
      }
      else newdata48[i,j]<-NA
    }
  }
  errors48$Error[j-5]<-k
}
errors48


#Gap-filling algorithm for met49 from met40-------------------------------
#Also keep track of error rates for met49
errors49<-data.frame(Variable=names(data49)[6:(length(data49[1,])-1)],Error=rep(0,length(data49[1,])-6))
for (j in 6:(length(newdata49[1,])-1)){#loop only for measured vars
  k<-0
  for (i in 1:length(newdata49[,1])){
    if(newdata49[i,j]>(-500)) next #don't do anythng to good data
    else {
      k<-k+1
      if (newdata49$Jul_Day[i]%in%data40$Jul_Day) {
        fillIndex<-which(data40$Jul_Day==newdata49$Jul_Day[i])#finds matching row in data49
        if(data40[fillIndex,j]>(-500)){
          newdata49[i,j]<-data40[fillIndex,j]
        }
        else newdata49[i,j]<-NA
      }
      else newdata49[i,j]<-NA
    } 
  }
  errors49$Error[j-5]<-k
}
errors49

#Gap-filling algorithm for met50 from met40---------------------------------
#Also keep track of error rates for met50
errors50<-data.frame(Variable=names(data50)[6:(length(data50[1,])-1)],Error=rep(0,length(data50[1,])-6))
for (j in 6:(length(newdata50[1,])-1)){#loop only for measured vars
  k<-0
  for (i in 1:length(newdata50[,1])){
    if(newdata50[i,j]>(-500)) next #don't do anythng to good data
    else {
      k<-k+1
      if (newdata50$Jul_Day[i]%in%data40$Jul_Day) {
        fillIndex<-which(data40$Jul_Day==newdata50$Jul_Day[i])#finds matching row in data50
        if(data40[fillIndex,j]>(-500)){
          newdata50[i,j]<-data40[fillIndex,j]
        }
        else newdata50[i,j]<-NA
      }
      else newdata50[i,j]<-NA
    } 
  }
  errors50$Error[j-5]<-k
}
errors50


#Gap-filling algorithm for met01 from met45------------------------------------
#Also keep track of error rates for met01
errors01<-data.frame(Variable=names(data01)[6:(length(data01[1,])-1)],Error=rep(0,length(data01[1,])-6))
for (j in 6:(length(newdata01[1,])-1)){#loop only for measured vars
  k<-0
  for (i in 1:length(newdata01[,1])){
    if(newdata01[i,j]>(-500)) next #don't do anythng to good data
    else {
      k<-k+1
      if (newdata01$Jul_Day[i]%in%data45$Jul_Day) {
        fillIndex<-which(data45$Jul_Day==newdata01$Jul_Day[i])#finds matching row in data01
        if(data45[fillIndex,j]>(-500)){
          newdata01[i,j]<-data45[fillIndex,j]
        }
        else newdata01[i,j]<-NA
      }
      else newdata01[i,j]<-NA
    } 
  }
  errors01$Error[j-5]<-k
}
errors01



#To combine dataframes into a new all_met dataset when done------------------------
newdata<-rbind.data.frame(newdata01,newdata40,newdata41,newdata42,newdata43,newdata44,newdata45,newdata48,newdata49,newdata50)#can add as many dataframes as needed

#Export new data as csv
write.csv(newdata,"Met_all_gapfill.csv")

#Second round of gap-filling for precip---------------------------
#Filling met01 precip with met49 or met44; neither works
newdata01$Year[which(is.na(newdata01$Precip))]
for (i in 1:length(newdata01[,1])){
  if(is.na(newdata01$Precip[i])) {
       if (newdata01$Jul_Day[i]%in%data49$Jul_Day) {
       fillIndex<-which(data49$Jul_Day==newdata01$Jul_Day[i])#finds matching row in data01
       if(is.na(data49$Precip[fillIndex])|data49$Precip[fillIndex]<(-500)) next
          else {newdata01$Precip[i]<-data49$Precip[fillIndex]}
     }
    else next
  }
  else next
}
newdata01$Year[which(is.na(newdata01$Precip))]#still no change
#These 12/30, 12/31 in 2013 and 2014 dates are fails across the board
fails<-newdata01$Jul_Day[which(is.na(newdata01$Precip))]
newdata01$Precip[which(data01$Jul_Day%in%fails)]<-0

#fill met40 with met50, met1, met45
newdata40$Year[which(is.na(newdata40$Precip))]#mostly before 1992
for (i in 1:length(newdata40[,1])){
  if(is.na(newdata40$Precip[i])) {
    if (newdata40$Jul_Day[i]%in%data50$Jul_Day) {
      fillIndex<-which(data50$Jul_Day==newdata40$Jul_Day[i])#finds matching row in data01
      if(is.na(data50$Precip[fillIndex])|data50$Precip[fillIndex]<(-500)) next
      else {newdata40$Precip[i]<-data50$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata40$Year[which(is.na(newdata40$Precip))]#looks like no change, length=120
for (i in 1:length(newdata40[,1])){
  if(is.na(newdata40$Precip[i])) {
    if (newdata40$Jul_Day[i]%in%data01$Jul_Day) {
      fillIndex<-which(data01$Jul_Day==newdata40$Jul_Day[i])
      if(is.na(data01$Precip[fillIndex])|data01$Precip[fillIndex]<(-500)) next
      else {newdata40$Precip[i]<-data01$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata40$Year[which(is.na(newdata40$Precip))]#length=118
for (i in 1:length(newdata40[,1])){
  if(is.na(newdata40$Precip[i])) {
    if (newdata40$Jul_Day[i]%in%data45$Jul_Day) {
      fillIndex<-which(data45$Jul_Day==newdata40$Jul_Day[i])
      if(is.na(data45$Precip[fillIndex])|data45$Precip[fillIndex]<(-500)) next
      else {newdata40$Precip[i]<-data45$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata40$Year[which(is.na(newdata40$Precip))]#length=89
#fill the 2013/14 dates with zeros
newdata40$Precip[which(data40$Jul_Day%in%fails)]<-0


#fill met41 with met 44
newdata41$Year[which(is.na(newdata41$Precip))]#mostly in 1989; length=54
for (i in 1:length(newdata41[,1])){
  if(is.na(newdata41$Precip[i])) {
    if (newdata41$Jul_Day[i]%in%data44$Jul_Day) {
      fillIndex<-which(data44$Jul_Day==newdata44$Jul_Day[i])#finds matching row in data01
      if(is.na(data44$Precip[fillIndex])|data44$Precip[fillIndex]<(-500)) next
      else {newdata41$Precip[i]<-data44$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata41$Year[which(is.na(newdata41$Precip))]
#length=42; only 1989 left, which cannot be filled

#met42 has no other fill options
newdata42$Year[which(is.na(newdata42$Precip))]#mostly before 1992

#fill met43 with met1
newdata43$Year[which(is.na(newdata43$Precip))]#mostly in 1989;length=178
for (i in 1:length(newdata43[,1])){
  if(is.na(newdata43$Precip[i])) {
    if (newdata43$Jul_Day[i]%in%data01$Jul_Day) {
      fillIndex<-which(data01$Jul_Day==newdata43$Jul_Day[i])#finds matching row in data01
      if(is.na(data01$Precip[fillIndex])|data01$Precip[fillIndex]<(-500)) next
      else {newdata43$Precip[i]<-data01$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata43$Year[which(is.na(newdata43$Precip))]#length=177
#fill the 2013/14 fail dates
newdata43$Precip[which(data43$Jul_Day%in%fails)]<-0


#fill met44 with met41; met49
newdata44$Year[which(is.na(newdata44$Precip))]#mostly before 1992;length=47
for (i in 1:length(newdata44[,1])){
  if(is.na(newdata44$Precip[i])) {
    if (newdata44$Jul_Day[i]%in%data41$Jul_Day) {
      fillIndex<-which(data41$Jul_Day==newdata44$Jul_Day[i])#finds matching row in data01
      if(is.na(data41$Precip[fillIndex])|data41$Precip[fillIndex]<(-500)) next
      else {newdata44$Precip[i]<-data41$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata44$Year[which(is.na(newdata44$Precip))]#only 4 left
for (i in 1:length(newdata44[,1])){
  if(is.na(newdata44$Precip[i])) {
    if (newdata44$Jul_Day[i]%in%data49$Jul_Day) {
      fillIndex<-which(data49$Jul_Day==newdata44$Jul_Day[i])#finds matching row in data01
      if(is.na(data49$Precip[fillIndex])|data49$Precip[fillIndex]<(-500)) next
      else {newdata44$Precip[i]<-data49$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata44$Year[which(is.na(newdata44$Precip))]#still unfilled
#fill the 2013/14 fail dates
newdata44$Precip[which(data44$Jul_Day%in%fails)]<-0

#fill met45 with met1; met44
newdata45$Year[which(is.na(newdata45$Precip))]#5 dates
for (i in 1:length(newdata45[,1])){
  if(is.na(newdata45$Precip[i])) {
    if (newdata45$Jul_Day[i]%in%data01$Jul_Day) {
      fillIndex<-which(data01$Jul_Day==newdata45$Jul_Day[i])#finds matching row in data01
      if(is.na(data01$Precip[fillIndex])|data01$Precip[fillIndex]<(-500)) next
      else {newdata45$Precip[i]<-data01$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata45$Year[which(is.na(newdata45$Precip))]#4 left
for (i in 1:length(newdata45[,1])){
  if(is.na(newdata45$Precip[i])) {
    if (newdata45$Jul_Day[i]%in%data44$Jul_Day) {
      fillIndex<-which(data44$Jul_Day==newdata45$Jul_Day[i])#finds matching row in data01
      if(is.na(data44$Precip[fillIndex])|data44$Precip[fillIndex]<(-500)) next
      else {newdata45$Precip[i]<-data44$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata45$Year[which(is.na(newdata45$Precip))]#still unfilled
#fill the 2013/14 fail dates
newdata45$Precip[which(data45$Jul_Day%in%fails)]<-0

#fill met48 with met50
newdata48$Year[which(is.na(newdata48$Precip))]#length=11
for (i in 1:length(newdata48[,1])){
  if(is.na(newdata48$Precip[i])) {
    if (newdata48$Jul_Day[i]%in%data50$Jul_Day) {
      fillIndex<-which(data50$Jul_Day==newdata48$Jul_Day[i])#finds matching row in data01
      if(is.na(data50$Precip[fillIndex])|data50$Precip[fillIndex]<(-500)) next
      else {newdata48$Precip[i]<-data50$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata48$Year[which(is.na(newdata48$Precip))]#4 unfilled
#fill the 2013/14 fail dates
newdata48$Precip[which(data48$Jul_Day%in%fails)]<-0


#fill met49 with met50; met1
newdata49$Year[which(is.na(newdata49$Precip))]#5 unfilled
for (i in 1:length(newdata49[,1])){
  if(is.na(newdata49$Precip[i])) {
    if (newdata49$Jul_Day[i]%in%data50$Jul_Day) {
      fillIndex<-which(data50$Jul_Day==newdata49$Jul_Day[i])#finds matching row in data01
      if(is.na(data50$Precip[fillIndex])|data50$Precip[fillIndex]<(-500)) next
      else {newdata49$Precip[i]<-data50$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata49$Year[which(is.na(newdata49$Precip))]#still 5 unfilled
for (i in 1:length(newdata49[,1])){
  if(is.na(newdata49$Precip[i])) {
    if (newdata49$Jul_Day[i]%in%data01$Jul_Day) {
      fillIndex<-which(data01$Jul_Day==newdata49$Jul_Day[i])#finds matching row in data01
      if(is.na(data01$Precip[fillIndex])|data01$Precip[fillIndex]<(-500)) next
      else {newdata49$Precip[i]<-data01$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata49$Year[which(is.na(newdata49$Precip))]#4 unfilled
#fill the 2013/14 fail dates
newdata49$Precip[which(data49$Jul_Day%in%fails)]<-0

#fill met50 with met49, met48
newdata50$Year[which(is.na(newdata50$Precip))]#6 unfilled
for (i in 1:length(newdata50[,1])){
  if(is.na(newdata50$Precip[i])) {
    if (newdata50$Jul_Day[i]%in%data49$Jul_Day) {
      fillIndex<-which(data49$Jul_Day==newdata50$Jul_Day[i])#finds matching row in data01
      if(is.na(data49$Precip[fillIndex])|data49$Precip[fillIndex]<(-500)) next
      else {newdata50$Precip[i]<-data49$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata50$Year[which(is.na(newdata50$Precip))]#5 unfilled
for (i in 1:length(newdata50[,1])){
  if(is.na(newdata50$Precip[i])) {
    if (newdata50$Jul_Day[i]%in%data48$Jul_Day) {
      fillIndex<-which(data49$Jul_Day==newdata50$Jul_Day[i])#finds matching row in data01
      if(is.na(data48$Precip[fillIndex])|data48$Precip[fillIndex]<(-500)) next
      else {newdata50$Precip[i]<-data48$Precip[fillIndex]}
    }
    else next
  }
  else next
}
newdata50$Year[which(is.na(newdata50$Precip))]#all filled
#fill the 2013/14 fail dates
newdata50$Precip[which(data50$Jul_Day%in%fails)]<-0

#Combine and output after second round------------------------
newdata<-rbind.data.frame(newdata01,newdata40,newdata41,newdata42,newdata43,newdata44,newdata45,newdata48,newdata49,newdata50)#can add as many dataframes as needed
#Export new data as csv
write.csv(newdata,"Met_all_gapfill.csv")

#Check other anomalies-------------------------------
Met_good <- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV data analysis course/Met_all_gapfill.csv")
Met_good$Jul_Day <-mdy.date(Met_good$Month,Met_good$Day,Met_good$Year, nineteen= TRUE, fillday = FALSE, fillmonth = FALSE)
plot(Met_good$Jul_Day,Met_good$Avg_Temp)
plot(Met_good$Jul_Day,Met_good$Max_Temp)
plot(Met_good$Jul_Day,Met_good$Min_Temp)# a few interesting outliers at -40C
plot(Met_good$Jul_Day,Met_good$Avg_RH)
plot(Met_good$Jul_Day,Met_good$Precip)
plot(Met_good$Jul_Day,Met_good$Avg_VP)

