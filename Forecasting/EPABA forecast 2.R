library(forecast)
library(fpp)
library(TTR)
food<-read.csv("D:/My Documents/My Dropbox/EEPs/Apollo Hospitals Food.csv", header=T)
head(food)
boccp<-food$BKFST.OCCUP
idly<-food$Idly
dosa<-food$Dosa

#Converting data into ts objects
boccpts<-ts(boccp[4:115], start=c(1), end=c(16), frequency=7)
idlyts <- ts(idly[4:115], start=c(1), end=c(16), frequency=7)
dosats<-ts(dosa[4:115], start=c(1), end=c(16), frequency=7)

#Plotting of data
plot(boccpts)
plot(idlyts)
plot(dosats)


#Moving Average
boccpSMA7 <- SMA(boccp,n=7)
boccpSMA7ts<-ts(boccpSMA7, start=c(1), end=c(17), frequency=7)
plot(boccpSMA7ts)


#Seasonal plots of data 
seasonplot(boccpts)
ggseasonplot(boccpts, col=rainbow(7), year.labels=TRUE)
ggseasonplot(idlyts)
ggseasonplot(dosats)


#Simple Exponential Smoothing
fitboccpts <- ses(boccpts, h=7)
plot(fitboccpts)
summary(fitboccpts)


#Double Exponential Smoothing
fitboccptsde <- holt(boccpts, h=7)
plot(fitboccptsde)
summary(fitboccptsde)

#Holt-Winters Triple Exponential Smoothing
fitboccptshw <- hw(boccpts, h=7)
plot(fitboccptshw)
summary(fitcotqtshw)

#Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th","F","Sa","S","M","T", "W")
dwk<-rep(xq,16)
boccpt<-boccp[4:115]
boccp.df<-data.frame(boccpt,t,dwk)
boccplint<-lm(boccpt ~ t+factor(dwk), data=boccp.df)
summary(boccplint)
newdt<- data.frame(t=c(113:119),dwk=xq)
predict.lm(boccplint,newdata= newdt)
plot(predict.lm(boccplint,newdata= newdt),type="l")

#Decomposition method using STL
stlfit <- stl(boccpts, t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
summary(stlfit)
eeadj <- seasadj(stlfit)
fcast <- forecast(stlfit, method="naive")
plot(fcast)



