
#Sourcing data in R
naturalgasraw <- read.csv("natural gas demand.csv", stringsAsFactors = FALSE)

##########################################################################################################
# ENSURE FOLLOWING LIBRARIES ARE INSTALLED : using install.packages() & restart R studio after insallation
##########################################################################################################

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(lubridate)
library(scales)
library(gridExtra)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
require(reshape2)

View(naturalgasraw)
summary(naturalgasraw)

#########################
##### DATA CLEANING #####
#########################

#Number of columns with all values as NA's
length(colnames(naturalgasraw)[colSums(is.na(naturalgasraw)) == nrow(naturalgasraw)])

#Number of columns with all values as 0
length(colnames(naturalgasraw)[colSums(naturalgasraw == 0,na.rm = TRUE) == nrow(naturalgasraw)])
which(colSums(naturalgasraw == 0,na.rm = TRUE) == nrow(naturalgasraw))

##############Checking the NA values in each column
naturalgasraw %>% summarise_all(funs(sum(is.na(.))))

#No NA values found in the data


#Removeing first column in dataframe
naturalgas <- naturalgasraw[-1,]

View(naturalgas)

####################
##### ANALYSIS #####
####################

str(naturalgas)


#Boxplots for numeric variables to check outliers

ggplot(data = melt(naturalgas), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))

#No outliers in Overall Industry Sales
quantile(naturalgas$Overall.Industry.sales,seq(0,1,0.01))
plot_grid(ggplot(naturalgas, aes(Overall.Industry.sales))+ geom_histogram(binwidth = 10),
          ggplot(naturalgas, aes(x="",y=Overall.Industry.sales))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#No outliers in Price of Furnace Oil
quantile(naturalgas$Price.of.Furnace.Oil,seq(0,1,0.01))
plot_grid(ggplot(naturalgas, aes(Price.of.Furnace.Oil))+ geom_histogram(binwidth = 10),
          ggplot(naturalgas, aes(x="",y=Price.of.Furnace.Oil))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#No outliers in Price of Diesel Oil
quantile(naturalgas$Price.of.Natural.gas,seq(0,1,0.01))
plot_grid(ggplot(naturalgas, aes(Price.of.Natural.gas))+ geom_histogram(binwidth = 10),
          ggplot(naturalgas, aes(x="",y=Price.of.Natural.gas))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#No outliers in Competitors Price
quantile(naturalgas$Competitors..Price,seq(0,1,0.01))
plot_grid(ggplot(naturalgas, aes(Competitors..Price))+ geom_histogram(binwidth = 10),
          ggplot(naturalgas, aes(x="",y=Competitors..Price))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#No outliers in Exchange rate
quantile(naturalgas$Exchange.rate.previous.month,seq(0,1,0.01))
plot_grid(ggplot(naturalgas, aes(Exchange.rate.previous.month))+ geom_histogram(binwidth = 10),
          ggplot(naturalgas, aes(x="",y=Exchange.rate.previous.month))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#No outliers in Construction Indices
quantile(naturalgas$Construction.Indices,seq(0,1,0.01))
plot_grid(ggplot(naturalgas, aes(Construction.Indices))+ geom_histogram(binwidth = 10),
          ggplot(naturalgas, aes(x="",y=Exchange.rate.previous.month))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


# Running the Linear regression with all the variables

lm.Sales_all=lm(Overall.Industry.sales ~ Price.of.Furnace.Oil+ Price.of.Light.Diesel.Oil
                +Price.of.Natural.gas+ Competitors..Price+Exchange.rate.previous.month
                +Problem.Month+Construction.Indices,data=naturalgas)
summary(lm.Sales_all)
anova(lm.Sales_all)

# Ignoring Competitors price from the model due to high correlation with Diesel oil price
lm.Sales_all=lm(Overall.Industry.sales ~ Price.of.Furnace.Oil+ Price.of.Light.Diesel.Oil
                +Price.of.Natural.gas+Exchange.rate.previous.month
                +Problem.Month+Construction.Indices,data=naturalgas)
summary(lm.Sales_all)
anova(lm.Sales_all)



#Including interaction term for Price of Nat gas and Exchange rate

lm.Sales_all=lm(Overall.Industry.sales ~ Price.of.Furnace.Oil+ Price.of.Light.Diesel.Oil
                +Price.of.Natural.gas*Exchange.rate.previous.month
                +Problem.Month+Construction.Indices,data=naturalgas)
summary(lm.Sales_all)
anova(lm.Sales_all)

#Create the standarised residuals 
Sales_stdres=rstandard(lm.Sales_all)
Sales_fitted=fitted(lm.Sales_all)

# Plotting residuals with all the variables to verify that the distribution is random
par(mfrow=c(1,1))
plot(naturalgas$Price.of.Furnace.Oil, Sales_stdres, 
     ylab="Standardized Residuals", 
     xlab="Furnace Oil Price",
     main="Residuals vs Furnace oil price",pch=19, col="blue")
abline(0,0)

plot(naturalgas$Price.of.Light.Diesel.Oil, Sales_stdres, 
     ylab="Standardized Residuals", 
     xlab="Light diesel Oil Price",
     main="Residuals vs Light diesel oil price", pch=19, col="blue")
abline(0,0)

plot(naturalgas$Price.of.Natural.gas, Sales_stdres, 
     ylab="Standardized Residuals", 
     xlab="Natural gas Price",
     main="Residuals vs Natural gas oil price", pch=19, col="blue")
abline(0,0)

plot(naturalgas$Exchange.rate.previous.month, Sales_stdres, 
     ylab="Standardized Residuals", 
     xlab="Exchange rate",
     main="Residuals vs Exchange rate", pch=19, col="blue")
abline(0,0)

plot(naturalgas$Problem.Month, Sales_stdres, 
     ylab="Standardized Residuals", 
     xlab=" Problem month",
     main="Residuals vs Problm month", pch=19, col="blue")
abline(0,0)

plot(naturalgas$Construction.Indices, Sales_stdres, 
     ylab="Standardized Residuals", 
     xlab="  Construction indices",
     main="Residuals vs Construction indices", pch=19, col="blue")
abline(0,0)

# Plot of residuals with time
plot(naturalgas$Month, Sales_stdres, 
     ylab="Standardized Residuals", 
     xlab="  Month",
     main="Residuals vs time", pch=19, col="blue")
abline(0,0)
# Outcome --> To relationship detected, residual plots are random

# Plot of residuals with independent variable :Sales

plot(naturalgas$Overall.Industry.sales, Sales_stdres, 
     ylab="Standardized Residuals", 
     xlab="  Overall sales",
     main="Residuals vs Sales", pch=19, col="blue")
abline(0,0)



# Model validation

# Check model assumptions
# 1 : To check assumption that mean of residuals is approx zero

mean(lm.Sales_all$residuals)

# 2: To check for residuals heteroschedasticity and normality of residuals
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(lm.Sales_all)

# 3: Autocorrelation of residuals
library(ggplot2)

acf(lm.Sales_all$residuals)
lmtest::dwtest(lm.Sales_all)
#DW = 1.8906, p-value = 0.06538
#alternative hypothesis: true autocorrelation is greater than 0

library(car)
# Check for multicollinearity
vif(lm.Sales_all)

#require(corrplot)
library(corrplot)
corrplot(cor(naturalgas[, -1]))

# normal distribution
qqnorm(resid(lm.Sales_all))

#==========================================================
# ==================== Competing model ===================
#==========================================================

gasDemand1=read.csv("C:/Users/ganesh/Desktop/caba_iima/regression/NGD_wo_outlier.csv",header=T)

attach(gasDemand1)

head(gasDemand1)

month2<-gasDemand1$Month
sales.naturalGas2<-gasDemand1$Overall.Industry.sales
price.furnaceOil2<-gasDemand1$Price.of.Furnace.Oil
price.dieselOil2<-gasDemand1$Price.of.Light.Diesel.Oil
price.naturalgas2<-gasDemand1$Price.of.Natural.gas
price.competitor2<-gasDemand1$Competitors..Price
exchangeRate2<-gasDemand1$Exchange.rate.previous.month
problemMonth2<-gasDemand1$Problem.Month
Construction.Indices2<-gasDemand1$Construction.Indices

#===================================
# reduced model

NGD.lm.try6=lm(sales.naturalGas2~ price.dieselOil2 +price.naturalgas2
               + exchangeRate2 + problemMonth2 + Construction.Indices2 )
summary(NGD.lm.try6)
# R-squared:  0.8221,	Adjusted R-squared:  0.7797

#Create the standarised residuals 
ngd.lm.try6.stdres=rstandard(NGD.lm.try6)
ngd.lm.try6.fit=fitted(NGD.lm.try6)

lmtest::dwtest(NGD.lm.try6)
vif(NGD.lm.try6)

#print(ngd.lm.try5.fit)
plot(ngd.lm.try6.stdres)
hist(ngd.lm.try6.stdres, probability=T, xlab = "Standardised Residuals", main = "Histogram of Standardised Residuals")

#Plot of the standardised residuals against the fitted values
plot(ngd.lm.try6.fit,ngd.lm.try6.stdres,xlab="Fitted Sales : Natural Gas ",ylab="Standardized Residuals", 
     pch=19, main="Random Sample : (e* vs y fitted)", col="red")
abline(c(0,0))

#Plot of the standardised residuals against the variables
#plot(price.furnaceOil,ngd.lm.try5.stdres,xlab="Graduation Rate",ylab="Standardized Residuals", pch=19, main="Plot of standardized residuals against graduation rate", col="red")
#abline(c(0,0))
plot(price.dieselOil2,ngd.lm.try6.stdres,xlab="Price : Diesel", ylab="Standardized Residuals", 
     pch=19, main="Standardized residuals against Price of Diesel", col="red")
abline(c(0,0))

plot(price.naturalgas2,ngd.lm.try6.stdres,xlab="Price : Natural gas", ylab="Standardized Residuals",
     pch=19, main="Standardized residuals against price of natural gas", col="red")
#plot(price.naturalgas2)
abline(c(0,0))

plot(exchangeRate2,ngd.lm.try6.stdres,xlab="Exchange Rate", ylab="Standardized Residuals",
     pch=19, main="Standardized residuals against Exchange Rate", col="red")
abline(c(0,0))

plot(problemMonth2,ngd.lm.try6.stdres,xlab="Problem month", ylab="Standardized Residuals", 
     pch=19, main="Standardized residuals against problem month", col="red")
abline(c(0,0))

plot(Construction.Indices2,ngd.lm.try6.stdres,xlab="Construction Index", ylab="Standardized Residuals", 
     pch=19, main="Standardized residuals against construction Index", col="red")
abline(c(0,0))

plot(month2,ngd.lm.try6.stdres)
sum(ngd.lm.try6.stdres)
print(ngd.lm.try6.stdres)

plot(ngd.lm.try6.stdres, xlab="Index", ylab="Standardized Residuals", pch=19, main="Independent observations", col="red")
abline(c(0,0))


