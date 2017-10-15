#Load required packages
library(forecast)
library(xts)
library(lubridate)

seaice<-read.csv("seaice.csv",header = TRUE,stringsAsFactors = FALSE)
str(seaice)
north_hemi<-seaice[seaice$hemisphere=='north',]

seaice$Date<-as.Date(paste(seaice$Year,seaice$Month,seaice$Day,sep = '-'))
  
#create initial time series object

seaice.xts<-xts(x=seaice$Extent, order.by = seaice$Date)

seaice.monthly<-apply.monthly(seaice.xts,mean)

#split the data into train and test sets
div_index<-floor(0.8*length(seaice.monthly))
seaice.train<-seaice.monthly[1:div_index,]
seaice.test<-seaice.monthly[(div_index+1):length(seaice.monthly),]

#convert xts train/test data to ts objects
?ts

seaice.start<-c(year(start(seaice.train)),month(start(seaice.train)))
seaice.end<-c(year(end(seaice.train)),month(end(seaice.train)))
seaice.train<-ts(as.numeric(seaice.train),seaice.start,seaice.end,frequency = 12)

seaice.start<-c(year(start(seaice.test)),month(start(seaice.test)))
seaice.end<-c(year(end(seaice.test)),month(end(seaice.test)))
seaice.test<-ts(as.numeric(seaice.test),seaice.start,seaice.end,frequency = 12)

#use variable to track forecast horizon
forecast.horizon<-length(seaice.test)

cycle(seaice.train)
plot(aggregate(seaice.train))
boxplot(seaice.test~cycle(seaice.test))
decompose(seaice.test)
plot(decompose(seaice.test))

# Sea ice in May month
seaice.may<-window(seaice.test,start<-c(2008,5),freq=TRUE)
plot(seaice.may)

#forecast seaice extent 
fs<-forecast(seaice.test)
attributes(fs)
plot(fs)

#HoltWinters objects
plot(HoltWinters(seaice.test))
seaice.predict<-predict(HoltWinters(seaice.test), n.ahead = 10*12)
plot(seaice.predict)
ts.plot(seaice.test,seaice.predict,lty=1:2)