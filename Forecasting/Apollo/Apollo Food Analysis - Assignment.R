#Sourcing data in R
apollo<-read.csv("Apollo Hospitals Food.csv", stringsAsFactors = FALSE)

##########################################################################################################
# ENSURE FOLLOWING LIBRARIES ARE INSTALLED : using install.packages() & restart R studio after insallation
##########################################################################################################


#Installing and loading required packages
install.packages("forecast")
install.packages("fpp")
install.packages("TTR")

#Load the data and Activate the libraries

library(forecast)
library(fpp)
library(TTR)
require(forecast)
require(fpp)
require(TTR)

head(apollo)

bf<-apollo$BKFST_OCCUP
date<-apollo$Date
day<-apollo$Day.of.Week
idly<-apollo$Idly
dosa<-apollo$Dosa
ch<-apollo$Chutney
sm<-apollo$Sambar
cont<-apollo$Continental.B.F
north<-apollo$North.Indian.B.F

#Forecasting the occupancy for breakfast

bft<-ts(bf[4:115], start=c(1), end=c(16), frequency=7)

plot(bft, xlab="Weeks", ylab="Occupancy for Breakfast", col='blue')

# Investigating for trend in the data through simple moving averages MA(7)

bfSMA<-SMA(bf,n=7)
bfSMAt<-ts(bfSMA, start=c(1), end=c(17), frequency=7)
plot(bfSMAt, xlab="Weeks", ylab="Occupancy for Breakfast", col='blue')

### Does not show trend

## Investigation for Seasonality in the data

ggseasonplot(bft, col=rainbow(7), year.labels=TRUE)
 
### Seasonal plot does not indicate seasonality


##Applying Single Exponential Smoothening for forecast

fitbft<-ses(bft,h=7)
plot(fitbft)
summary(fitbft)

fcst.fitbft<-forecast(fitbft)

SE.fitbft<-(bft-fcst.fitbft$fitted)^2
MSE.fitbft<-sum(SE.fitbft)/106
RMSE.fitbft<-(MSE.fitbft)^0.5


### Forecasted Value = 254.4685 MSE = 220.9917

##Applying SES with alpha=0.2, initial seed = Y(1)

fitbft1<-ses(bft,h=7, initial="simple", alpha=0.2)
plot(fitbft1)
summary(fitbft1)

fcst.fitbft1<-forecast(fitbft1)

SE.fitbft1<-(bft-fcst.fitbft1$fitted)^2
MSE.fitbft1<-sum(SE.fitbft1)/106
RMSE.fitbft1<-(MSE.fitbft1)^0.5

### Forecasted Value = 238.2296 MSE = 311.5814

##Double Exponential Smoothening (Holt)
fitbftde<-holt(bft,h=7)
plot(fitbftde)  
summary(fitbftde)

fcst.fitbftde<-forecast(fitbftde)

SE.fitbftde<-(bft-fcst.fitbftde$fitted)^2
MSE.fitbftde<-sum(SE.fitbftde)/106
RMSE.fitbftde<-(MSE.fitbftde)^0.5

### Forecasted Value = 254.4333 MSE = 221.7538

##Triple Exponential Smoothening (Holt-Winter)
fitbftte<-hw(bft, h=7)
plot(fitbftte)
summary(fitbftte)

fcst.fitbftte<-forecast(fitbftte)

SE.fitbftte<-(bft-fcst.fitbftte$fitted)^2
MSE.fitbftte<-sum(SE.fitbftte)/106
RMSE.fitbftte<-(MSE.fitbftte)^0.5

### Forecasted Value = 254.2232 MSE = 158.9385


##Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th", "F", "Sa", "S", "M", "T", "W")
dwk<-rep(xq,16)
bft<-bf[4:115]
bf.df<-data.frame(bft,t,dwk)
bflint<-lm(bft~t+factor(dwk),data=bf.df)
summary(bflint)
newdt<-data.frame(t=c(113:119), dwk=xq)
predict.lm(bflint,newdata=newdt)
plot(predict.lm(bflint, newdata=newdt), type="l")

SE.bflint<-(bft-bflint$fitted)^2
MSE.bflint<-sum(SE.bflint)/112

##Decomposition method using STL
stlfit<-stl(bft,t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
summary(stlfit)
eeadj<-seasadj(stlfit)
fcast.stlfit<-forecast(stlfit, method="naive")
plot(fcast.stlfit)

fcast.stlfit$fitted
SE.stlfit<-(bft[2:105]-fcast.stlfit$fitted[2:105])^2
MSE.stlfit<-sum(SE.stlfit)/105

##Ensembled Forecast

ens.fcast<-c(254.4685, 254.4333, 254.2232, 226.7857, 253.9620)
ens.MSE<-c(220.9917, 221.7538, 158.9385, 298.1662, 164.8113)
ens.MSEwt<-ens.MSE/sum(ens.MSE)
ens.MSEwtadj<-1-ens.MSEwt
ens.MSEwtadjstd<-ens.MSEwtadj/sum(ens.MSEwtadj)

ens.fcastvalue<-ens.fcast*ens.MSEwtadjstd
sum(ens.fcastvalue)

### forecasted value = 249.3198


#Forecasting the idlys

idlyts<-ts(idly[4:115], start=c(1), end=c(16), frequency=7)

plot(idlyts, xlab="Weeks", ylab="Idly Consumption", col='blue')

## Investigating for trend in the data through simple moving averages MA(7)

idlySMA7<-SMA(idly,n=7)
idlySMA7ts<-ts(idlySMA7, start=c(1), end=c(17), frequency=7)
plot(idlySMA7ts, xlab="Weeks", ylab="Idly Consumption", col='blue')

### Level of the series decreases after 10th week

## Investigation for Seasonality in the data

ggseasonplot(idlyts, col=rainbow(7), year.labels=TRUE)

### Seasonal plot does not indicate seasonality


##Applying Single Exponential Smoothening for forecast

fitidlyts<-ses(idlyts,h=7)
plot(fitidlyts)
summary(fitidlyts)

fcst.fitidlyts<-forecast(fitidlyts)

SE.fitidlyts<-(idlyts-fcst.fitidlyts$fitted)^2
MSE.fitidlyts<-sum(SE.fitidlyts)/106
RMSE.fitidlyts<-(MSE.fitidlyts)^0.5


### Forecasted Value = 63.67064 MSE = 26.33651

##Applying SES with alpha=0.2, initial seed = Y(1)

fitidlyts1<-ses(idlyts,h=7, initial="simple", alpha=0.2)
plot(fitidlyts1)
summary(fitidlyts1)

fcst.fitidlyts1<-forecast(fitidlyts1)

SE.fitidlyts1<-(idlyts-fcst.fitidlyts1$fitted)^2
MSE.fitidlyts1<-sum(SE.fitidlyts1)/106
RMSE.fitidlyts1<-(MSE.fitidlyts1)^0.5

### Forecasted Value = 62.4291 MSE = 27.60826

##Double Exponential Smoothening (Holt)
fitidlytsde<-holt(idlyts,h=7)
plot(fitidlytsde)  
summary(fitidlytsde)

fcst.fitidlytsde<-forecast(fitidlytsde)

SE.fitidlytsde<-(idlyts-fcst.fitidlytsde$fitted)^2
MSE.fitidlytsde<-sum(SE.fitidlytsde)/106
RMSE.fitidlytsde<-(MSE.fitidlytsde)^0.5

### Forecasted Value = 63.70499 MSE = 26.39755

##Triple Exponential Smoothening (Holt-Winter)
fitidlytste<-hw(idlyts, h=7)
plot(fitidlytste)
summary(fitidlytste)

fcst.fitidlytste<-forecast(fitidlytste)

SE.fitidlytste<-(idlyts-fcst.fitidlytste$fitted)^2
MSE.fitidlytste<-sum(SE.fitidlytste)/106
RMSE.fitidlytste<-(MSE.fitidlytste)^0.5

### Forecasted Value = 63.69671 MSE = 25.66463


##Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th", "F", "Sa", "S", "M", "T", "W")
dwk<-rep(xq,16)
idlyt<-idly[4:115]
idly.df<-data.frame(idlyt,t,dwk)
idlylint<-lm(idlyt~t+factor(dwk),data=idly.df)
summary(idlylint)
newdt<-data.frame(t=c(113:119), dwk=xq)
predict.lm(idlylint,newdata=newdt)
plot(predict.lm(idlylint, newdata=newdt), type="l")

SE.idlylint<-(idlyt-idlylint$fitted)^2
MSE.idlylint<-sum(SE.idlylint)/112

##Decomposition method using STL
stlfit<-stl(idlyts,t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
summary(stlfit)
eeadj<-seasadj(stlfit)
fcast.stlfit<-forecast(stlfit, method="naive")
plot(fcast.stlfit)

fcast.stlfit$fitted
SE.stlfit<-(idlyts[2:105]-fcast.stlfit$fitted[2:105])^2
MSE.stlfit<-sum(SE.stlfit)/105

##Ensembled Forecast

ens.fcast<-c(63.67064, 63.70499, 63.69671, 57.0250, 63.13484)
ens.MSE<-c(26.33651, 26.39755, 25.66463, 41.11838, 33.71333)
ens.MSEwt<-ens.MSE/sum(ens.MSE)
ens.MSEwtadj<-1-ens.MSEwt
ens.MSEwtadjstd<-ens.MSEwtadj/sum(ens.MSEwtadj)

ens.fcastvalue<-ens.fcast*ens.MSEwtadjstd
sum(ens.fcastvalue)

### forecasted value = 62.36311

#Forecasting the dosa

dosats<-ts(dosa[4:115], start=c(1), end=c(16), frequency=7)

plot(dosats, xlab="Weeks", ylab="Dosa Consumption", col='blue')

## Investigating for trend in the data through simple moving averages MA(7)

dosaSMA7<-SMA(dosa,n=7)
dosaSMA7ts<-ts(dosaSMA7, start=c(1), end=c(17), frequency=7)
plot(dosaSMA7ts, xlab="Weeks", ylab="Dosa Consumption", col='blue')

### Outliers in week 14

## Investigation for Seasonality in the data

ggseasonplot(dosats, col=rainbow(7), year.labels=TRUE)

### Seasonal plot does not indicate seasonality


##Applying Single Exponential Smoothening for forecast

fitdosats<-ses(dosats,h=7)
plot(fitdosats)
summary(fitdosats)

fcst.fitdosats<-forecast(fitdosats)

SE.fitdosats<-(dosats-fcst.fitdosats$fitted)^2
MSE.fitdosats<-sum(SE.fitdosats)/106
RMSE.fitdosats<-(MSE.fitdosats)^0.5


### Forecasted Value = 20.51214 MSE = 89.34041

##Applying SES with alpha=0.2, initial seed = Y(1)

fitdosats1<-ses(dosats,h=7, initial="simple", alpha=0.2)
plot(fitdosats1)
summary(fitdosats1)

fcst.fitdosats1<-forecast(fitdosats1)

SE.fitdosats1<-(dosats-fcst.fitdosats1$fitted)^2
MSE.fitdosats1<-sum(SE.fitdosats1)/106
RMSE.fitdosats1<-(MSE.fitdosats1)^0.5

### Forecasted Value = 22.9624 MSE = 102.8809

##Double Exponential Smoothening (Holt)
fitdosatsde<-holt(dosats,h=7)
plot(fitdosatsde)  
summary(fitdosatsde)

fcst.fitdosatsde<-forecast(fitdosatsde)

SE.fitdosatsde<-(dosats-fcst.fitdosatsde$fitted)^2
MSE.fitdosatsde<-sum(SE.fitdosatsde)/106
RMSE.fitdosatsde<-(MSE.fitdosatsde)^0.5

### Forecasted Value = 20.20311 MSE = 89.27177

##Triple Exponential Smoothening (Holt-Winter)
fitdosatste<-hw(dosats, h=7)
plot(fitdosatste)
summary(fitdosatste)

fcst.fitdosatste<-forecast(fitdosatste)

SE.fitdosatste<-(dosats-fcst.fitdosatste$fitted)^2
MSE.fitdosatste<-sum(SE.fitdosatste)/106
RMSE.fitdosatste<-(MSE.fitdosatste)^0.5

### Forecasted Value = 13.38469 MSE = 75.63212


##Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th", "F", "Sa", "S", "M", "T", "W")
dwk<-rep(xq,16)
dosat<-dosa[4:115]
dosa.df<-data.frame(dosat,t,dwk)
dosalint<-lm(dosat~t+factor(dwk),data=dosa.df)
summary(dosalint)
newdt<-data.frame(t=c(113:119), dwk=xq)
predict.lm(dosalint,newdata=newdt)
plot(predict.lm(dosalint, newdata=newdt), type="l")

SE.dosalint<-(dosat-dosalint$fitted)^2
MSE.dosalint<-sum(SE.dosalint)/112

##Decomposition method using STL
stlfit<-stl(dosats,t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
summary(stlfit)
eeadj<-seasadj(stlfit)
fcast.stlfit<-forecast(stlfit, method="naive")
plot(fcast.stlfit)

fcast.stlfit$fitted
SE.stlfit<-(dosats[2:105]-fcast.stlfit$fitted[2:105])^2
MSE.stlfit<-sum(SE.stlfit)/105

##Ensembled Forecast

ens.fcast<-c(20.51214, 20.20311, 13.38469, 33.22143, 15.97456)
ens.MSE<-c(89.34041, 89.27177, 75.63212, 118.6095, 100.0251)
ens.MSEwt<-ens.MSE/sum(ens.MSE)
ens.MSEwtadj<-1-ens.MSEwt
ens.MSEwtadjstd<-ens.MSEwtadj/sum(ens.MSEwtadj)

ens.fcastvalue<-ens.fcast*ens.MSEwtadjstd
sum(ens.fcastvalue)

### forecasted value = 20.43852


#Forecasting the chutney

chts<-ts(ch[4:115], start=c(1), end=c(16), frequency=7)

plot(chts, xlab="Weeks", ylab="Chutney Consumption", col='blue')

## Investigating for trend in the data through simple moving averages MA(7)

chSMA7<-SMA(ch,n=7)
chSMA7ts<-ts(chSMA7, start=c(1), end=c(17), frequency=7)
plot(chSMA7ts, xlab="Weeks", ylab="Chutney Consumption", col='blue')

### Outliers in week 14

## Investigation for Seasonality in the data

ggseasonplot(chts, col=rainbow(7), year.labels=TRUE)

### Seasonal plot does not indicate seasonality


##Applying Single Exponential Smoothening for forecast

fitchts<-ses(chts,h=7)
plot(fitchts)
summary(fitchts)

fcst.fitchts<-forecast(fitchts)

SE.fitchts<-(chts-fcst.fitchts$fitted)^2
MSE.fitchts<-sum(SE.fitchts)/106
RMSE.fitchts<-(MSE.fitchts)^0.5



##Applying SES with alpha=0.2, initial seed = Y(1)

fitchts1<-ses(chts,h=7, initial="simple", alpha=0.2)
plot(fitchts1)
summary(fitchts1)

fcst.fitchts1<-forecast(fitchts1)

SE.fitchts1<-(chts-fcst.fitchts1$fitted)^2
MSE.fitchts1<-sum(SE.fitchts1)/106
RMSE.fitchts1<-(MSE.fitchts1)^0.5


##Double Exponential Smoothening (Holt)
fitchtsde<-holt(chts,h=7)
plot(fitchtsde)  
summary(fitchtsde)

fcst.fitchtsde<-forecast(fitchtsde)

SE.fitchtsde<-(chts-fcst.fitchtsde$fitted)^2
MSE.fitchtsde<-sum(SE.fitchtsde)/106
RMSE.fitchtsde<-(MSE.fitchtsde)^0.5


##Triple Exponential Smoothening (Holt-Winter)
fitchtste<-hw(chts, h=7)
plot(fitchtste)
summary(fitchtste)

fcst.fitchtste<-forecast(fitchtste)

SE.fitchtste<-(chts-fcst.fitchtste$fitted)^2
MSE.fitchtste<-sum(SE.fitchtste)/106
RMSE.fitchtste<-(MSE.fitchtste)^0.5



##Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th", "F", "Sa", "S", "M", "T", "W")
dwk<-rep(xq,16)
cht<-ch[4:115]
ch.df<-data.frame(cht,t,dwk)
chlint<-lm(cht~t+factor(dwk),data=ch.df)
summary(chlint)
newdt<-data.frame(t=c(113:119), dwk=xq)
predict.lm(chlint,newdata=newdt)
plot(predict.lm(chlint, newdata=newdt), type="l")

SE.chlint<-(cht-chlint$fitted)^2
MSE.chlint<-sum(SE.chlint)/112

##Decomposition method using STL
stlfit<-stl(chts,t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
summary(stlfit)
eeadj<-seasadj(stlfit)
fcast.stlfit<-forecast(stlfit, method="naive")
plot(fcast.stlfit)

fcast.stlfit$fitted
SE.stlfit<-(chts[2:105]-fcast.stlfit$fitted[2:105])^2
MSE.stlfit<-sum(SE.stlfit)/105

##Ensembled Forecast

ens.fcast<-c(135.0572, 135.1589, 131.3083, 136.6607, 137.2809)
ens.MSE<-c(97.84371, 97.98157, 83.63473, 94.88828, 108.2023)
ens.MSEwt<-ens.MSE/sum(ens.MSE)
ens.MSEwtadj<-1-ens.MSEwt
ens.MSEwtadjstd<-ens.MSEwtadj/sum(ens.MSEwtadj)

ens.fcastvalue<-ens.fcast*ens.MSEwtadjstd
sum(ens.fcastvalue)

### forecasted value = 135.056


#Forecasting the sambar

smts<-ts(sm[4:115], start=c(1), end=c(16), frequency=7)

plot(smts, xlab="Weeks", ylab="Sambar Consumption", col='blue')

## Investigating for trend in the data through simple moving averages MA(7)

smSMA7<-SMA(sm,n=7)
smSMA7ts<-ts(smSMA7, start=c(1), end=c(17), frequency=7)
plot(smSMA7ts, xlab="Weeks", ylab="Sambar Consumption", col='blue')

### Outliers in week 14

## Investigation for Seasonality in the data

ggseasonplot(smts, col=rainbow(7), year.labels=TRUE)

### Seasonal plot does not indicate seasonality


##Applying Single Exponential Smoothening for forecast

fitsmts<-ses(smts,h=7)
plot(fitsmts)
summary(fitsmts)

fcst.fitsmts<-forecast(fitsmts)

SE.fitsmts<-(smts-fcst.fitsmts$fitted)^2
MSE.fitsmts<-sum(SE.fitsmts)/106
RMSE.fitsmts<-(MSE.fitsmts)^0.5


##Double Exponential Smoothening (Holt)
fitsmtsde<-holt(smts,h=7)
plot(fitsmtsde)  
summary(fitsmtsde)

fcst.fitsmtsde<-forecast(fitsmtsde)

SE.fitsmtsde<-(smts-fcst.fitsmtsde$fitted)^2
MSE.fitsmtsde<-sum(SE.fitsmtsde)/106
RMSE.fitsmtsde<-(MSE.fitsmtsde)^0.5


##Triple Exponential Smoothening (Holt-Winter)
fitsmtste<-hw(smts, h=7)
plot(fitsmtste)
summary(fitsmtste)

fcst.fitsmtste<-forecast(fitsmtste)

SE.fitsmtste<-(smts-fcst.fitsmtste$fitted)^2
MSE.fitsmtste<-sum(SE.fitsmtste)/106
RMSE.fitsmtste<-(MSE.fitsmtste)^0.5



##Decomposition method with linear trend
t<-c(1:112)
xq<-c("Th", "F", "Sa", "S", "M", "T", "W")
dwk<-rep(xq,16)
smt<-sm[4:115]
sm.df<-data.frame(smt,t,dwk)
smlint<-lm(smt~t+factor(dwk),data=sm.df)
summary(smlint)
newdt<-data.frame(t=c(113:119), dwk=xq)
predict.lm(smlint,newdata=newdt)
plot(predict.lm(smlint, newdata=newdt), type="l")

SE.smlint<-(smt-smlint$fitted)^2
MSE.smlint<-sum(SE.smlint)/112

##Decomposition method using STL
stlfit<-stl(smts,t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
summary(stlfit)
eeadj<-seasadj(stlfit)
fcast.stlfit<-forecast(stlfit, method="naive")
plot(fcast.stlfit)

fcast.stlfit$fitted
SE.stlfit<-(smts[2:105]-fcast.stlfit$fitted[2:105])^2
MSE.stlfit<-sum(SE.stlfit)/105

##Ensembled Forecast

ens.fcast<-c(134.9271, 134.9585, 130.0488, 137.3339, 136.5317)
ens.MSE<-c(104.2415, 104.3255, 89.06008, 100.7451, 114.8651)
ens.MSEwt<-ens.MSE/sum(ens.MSE)
ens.MSEwtadj<-1-ens.MSEwt
ens.MSEwtadjstd<-ens.MSEwtadj/sum(ens.MSEwtadj)

ens.fcastvalue<-ens.fcast*ens.MSEwtadjstd
sum(ens.fcastvalue)

### forecasted value = 134.7204








