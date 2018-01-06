#Sourcing data in R
cotton<-read.csv("cotton.csv", stringsAsFactors = FALSE)


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

head(cotton)

#Prepare Time Series Plot to identify the patterns over time

## Cotton Quantity

cotq<-cotton$COTTONQ
cotqts<-ts(cotq, start=c(1966,1), end=c(1972,4), frequency=4)
plot(cotqts)
plot(cotqts, xlab = "Years", ylab="Cotton Production (in million lb)", pch=10, col="blue")

dev.copy(pdf,"CotQ Trend.pdf")
dev.off()

### Time series plot shows decreasing trend, and a hint of seasonality
### as troughs occurring at equal intervals but peaks are not

#### Investigation on Decreasing Trend

cotqSMA4<-SMA(cotq,n=4)
cotqSMA4ts<-ts(cotqSMA4,start=c(1966,1), end=c(1972,4), frequency=4)
plot(cotqSMA4ts)

##### Trend shows a definite decreasing trend

t<-c(1:28)
cotqlinfit<-lm(cotq~t)
summary(cotqlinfit)

##### R-square value of 0.9019 shows linealy decreasing trend

#### Investigation on Seasonality

ggseasonplot(cotqts, col=rainbow(7), year.lables=TRUE)

##### Seasonal Plot shows high degree of seasonality barring some 
##### data points like Q2 of 1970

## Wholesale price

prc<-cotton$WHOPRICE
prcts<-ts(prc, start=c(1966,1), end=c(1972,4), frequency=4)
plot(prcts)
plot(prcts, xlab = "Years", ylab="Whole Sale Cotton Price (in $ /lb)", pch=10, col="red")



### Time series plot shows increasing trend of price, and no certainity of 
### seasonality

#### Investigation on Increasing Trend

t<-c(1:28)
prclinfit<-lm(prc~t)
summary(prclinfit)

##### R-square value of 0.9846 shows linealy increasing trend

#### Investigation on Seasonality

ggseasonplot(prcts, col=rainbow(7), year.lables=TRUE)

##### Seasonal Plot does not show seasonality as price is an 
##### increasing linear trend and prices of Q4>Q3>Q2>Q1 as
##### depicted through parallel lines

## Import Quantity

impr<-cotton$IMPFAB
imprts<-ts(impr, start=c(1966,1), end=c(1972,4), frequency=4)
plot(imprts)

### Time series plot does not show trend or seasonality, however, the level 
### of series decreases drastically from 1969

#### Confirmation of absence of trend through SMA 4

imprSMA4<-SMA(impr,n=4)
imprSMA4ts<-ts(imprSMA4,start=c(1966,1), end=c(1972,4), frequency=4)
plot(imprSMA4ts)

#### Confirmation of absence of Seasonality

ggseasonplot(imprts, col=rainbow(7), year.lables=TRUE)

## Export Quantity

expr<-cotton$EXPFAB
exprts<-ts(expr, start=c(1966,1), end=c(1972,4), frequency=4)
plot(exprts)

### Time series plot does not show trend or seasonality 

#### Confirmation of absence of trend

exprSMA4<-SMA(expr,n=4)
exprSMA4ts<-ts(exprSMA4,start=c(1966,1), end=c(1972,4), frequency=4)
plot(exprSMA4ts)

#### Confirmation of absence of Seasonality

ggseasonplot(exprts, col=rainbow(7), year.lables=TRUE)

Identifying relationship between cotton production and wholesale price

plot(prc, cotq, xlab="Price ($ per lb)", ylab="Cotton Quantity (in million lb)", pch=10, col='blue')

plot(prcSMA4, cotqSMA4, xlab="Price ($ per lb)", ylab="Cotton Quantity (in million lb)", pch=10, col='blue')


## Scatter Plot shows inverse relationship between Quantity and Price
##Cotton production quantity decreases with increase in prices

#Estimating the relationship between cotton production and wholesale price

rel<-lm(cotq~prc)
summary(rel)

## R-sqaured of 0.9022 shows strong linear corelation, p<0.05
## co-efficient estimate for price as -45.126 shows inverse relationship
## cotq = 6599.84 - 45.126*prc
## Cotton Produce reduces as Wholesale Price increases


require(ggplot2)
sctr<-ggplot(cotton, aes(y=COTTONQ, x = WHOPRICE))
sctr+geom_point()+geom_smooth(method='lm')+xlab("Price ($ per lb)")+ylab("Cotton Quantity (in million lb)")

    

#Residual Plot

cotqres<-resid(rel)

prcdata<-data.frame(prc)
cotqpred<-predict.lm(rel,newdata=prcdata)

#Impact Estimation

prcimpact<- -45.126*sum(prc)/sum(cotqpred)
randimpact<- 6599.84*28/sum(cotqpred)

prcimpact
randimpact

### Price explains makes -270% impact on cotton quantity


plot(cotqpred, cotqres, xlab="Predicted Values of cotton quantity (in million lb)", ylab="Residue", pch=4, col='red')

## Variation of residue around 0 is not constant

hist(cotqres)

## Residues follow nearly normal distribution

#Determine price that will yield highest total revenue

rev<-cotqpred*prc

plot(prc, rev)

ndata<-data.frame(prc, cotq, cotqpred, rev)

max(rev)
## Maximum revenue = $ 213395.2 at Price = $ 98 /lb

#Optimal price using differential equation

## Revenue = (6599.840 - 45.126*price)*price
## Differentiating to get Optimal Price = -6599.84/(-2*45.126)

optprc<--6599.840/(2*-45.126)
optrev<-(6599.840 - 45.126*optprc)*optprc


## Optimal price is $ 73.1268 /lb and Optimal revenue = $ 241312.6

prcp<-seq(10,120, by=1)
predrev<-(6599.840 - 45.126*prcp)*prcp

plot(prcp, predrev, xlab="Price (in $ /lb)", ylab="Quarterly Revenue ($)", pch=16, col='blue')

#Estimation of parameters for imports and exports

relall<-lm(cotq~prc+impr+expr)
summary(relall)

## R-square value of 0.9347, p<0.05
## cotq = 6757.0093 - 46.9564*prc - 6.5175*impr + 0.3190*expr

## Impact Estimation

prcallimpact<--46.9564*sum(prc)/sum(cotqpred)
imprallimpact<--6.5175*sum(impr)/sum(cotqpred)
exprallimpact<-0.3190*sum(expr)/sum(cotqpred)
randallimpact<-6757.0093*28/sum(cotqpred)


prcallimpact
imprallimpact
exprallimpact
randallimpact

### Price makes an impact of -281%
### Import makes an impact of -2.7%
### Export makes an impact of +4.9%











