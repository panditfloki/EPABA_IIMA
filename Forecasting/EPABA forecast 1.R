library(forecast)
library(fpp)
library(TTR)
cotton<-read.csv("D:/My Documents/My Dropbox/Excel sheets/Analytics Nirma/COTTON.csv", header=T)
head(cotton)
cotq<-cotton$COTTONQ
prc<-cotton$WHOPRICE
impr<-cotton$IMPFAB
expr<-cotton$EXPFAB

#Converting data into ts objects
prcts<-ts(prc, start=c(1966, 1), end=c(1972, 4), frequency=4)
cotqts <- ts(cotq, start=c(1966, 1), end=c(1972, 4), frequency=4)
imprts<-ts(impr, start=c(1966, 1), end=c(1972, 4), frequency=4)
exprts<-ts(expr, start=c(1966, 1), end=c(1972, 4), frequency=4)

#Plotting of data
plot(prcts)
plot(cotqts)
plot(imprts)
plot(exprts)

#Moving Average
prcSMA4 <- SMA(prc,n=4)
prcSMA4ts<-ts(prcSMA4, start=c(1966, 1), end=c(1972, 4), frequency=4)
plot(prcSMA4ts)

cotqSMA4 <- SMA(cotq,n=4)
plot.ts(cotqSMA4)

#Linear trend fit
t<-c(1:28)
prclinfit<-lm(prc~t)
summary(prclinfit)
newt<- data.frame(t=c(29:32))
predict.lm(prclinfit,newdata= newt)

#Quadratic trend fit
t<-c(1:28)
prclinfit2<-lm(prc~poly(t,2))
summary(prclinfit2)
newt<- data.frame(t=c(29:32))
predict.lm(prclinfit2,newdata= newt)

#Seasonal plots of data 
seasonplot(cotqts)
ggseasonplot(cotqts, col=rainbow(7), year.labels=TRUE)
seasonplot(prcts)
seasonplot(imprts)
seasonplot(exprts)

#Simple Exponential Smoothing
fitexprts <- ses(exprts, h=1)
plot(fitexprts)
summary(fitexprts)

fitimprts <- ses(imprts, h=1)
plot(fitimprts)
summary(fitimprts)

#Double Exponential Smoothing
fitprcts <- holt(prcts, h=1)
plot(fitprcts)
summary(fitprcts)

#Holt-Winters Triple Exponential Smoothing
fitcotqts <- hw(cotqts, h=4)
plot(fitcotqts)
summary(fitcotqts)

#Decomposition method with linear trend
t<-c(1:28)
xq<-c("Q1","Q2","Q3","Q4")
qtr<-rep(xq,7)
cotq.df<-data.frame(cotq,t,qtr)
cotqlint<-lm(cotq~t+factor(qtr))
summary(cotqlint)
newdt<- data.frame(t=c(29:32),qtr=xq)
predict.lm(cotqlint,newdata= newdt)

#Decomposition Method
cotqdecomp<-decompose(cotqts)
plot(cotqdecomp)
cotqdecomp$trend
cotqdecomp$seasonal

#Decomposition method using STL
stlfit <- stl(cotqts, t.window=15, s.window="periodic", robust=TRUE)
plot(stlfit)
summary(stlfit)
eeadj <- seasadj(stlfit)
fcast <- forecast(stlfit, method="naive")
plot(fcast)



