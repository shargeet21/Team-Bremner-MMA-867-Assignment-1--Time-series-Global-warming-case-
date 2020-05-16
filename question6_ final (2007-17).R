library(ggplot2)
library(Amelia)
library(readr)
library(plyr)
library(readxl)
library(dplyr)
library(forecast)
remove(list = ls())



##NASA dataset......................................................................................................................................

NASA_data <- read_excel("C:/Users/sharg/Desktop/Geeta College Assignments/867- Predictive Modelling/Assignment 2/New folder/Combined.xlsx",sheet = 3)

NASA_data$Temperature <- (NASA_data$Temperature+ 57.2)

NASA06_ts <-ts(NASA_data$Temperature, start=c(1907,1),end=c(2006,12),frequency=12)
str(NASA06_ts)

##tbats model for NASA ##

NASA_tbats <- tbats(NASA06_ts)
NASA_tbats_pred <-forecast(NASA_tbats, h=132, level=c(0.75, 0.90))

#export the predicted temp for 2007 to 2017 into a cvs file##

write.csv(NASA_tbats_pred, "Predicted_temp_NASA_07.csv")

### Read_predicted data/compare ##

predicted_07_17 <- read.csv(file.choose())

Actual_NASAdata <- subset(NASA_data,(NASA_data$Year >=2007 & NASA_data$Year <=2017))

NASA_actual <- select(Actual_NASAdata, Temperature)
predicted_NASA.07 <- select(predicted_07_17, Point.Forecast)
predicted_NASA.07

str(NASA_actual)

#calculate MAPE:
percent.errors.nasa <- abs(predicted_NASA.07- NASA_actual)/predicted_NASA.07
mean(percent.errors.nasa$Point.Forecast)*100 #### mape for NASA TABTS value is 1.097825 ############

#calculate RMSE:
residual<-predicted_NASA.07 - NASA_actual
Q7NASA_TBATS_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7NASA_TBATS_RMSE
#### RMSE for NASA TABTS value is 0.6575786 ############


#NASA data .............................................................................................................................
#Constant temperature approach _Naive method

NASA_naive_pred06 <-naive(NASA06_ts,h=132)
write.csv(NASA_naive_pred06, "NASA_07_pred_naive.csv")

#calculate MAPE
NASA_naive_07 <-read.csv(file.choose(), header=TRUE, sep = ",")
NASA_naive_07 <-select(NASA_naive_07,Point.Forecast)

percent.error.naive <-abs(NASA_naive_07 - NASA_actual)/ NASA_naive_07
percent.error.naive$Point.Forecast

mean(percent.error.naive$Point.Forecast)*100
#### MAPE for NASA naive value is 0.454177 ############

#calculate RMSE:
residual<-NASA_naive_07 - NASA_actual
Q7naive_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7naive_RMSE
#### RMSE for NASA naive value is 0.3153762 ############



##Seasonal naive method ####################################################
str(NASA06_ts)

NASA_snaive_pred07 <-snaive(NASA06_ts,h=132)
write.csv(NASA_snaive_pred07, "NASA_07_pred_snaive.csv")

NASA_snaive_07 <-read.csv(file.choose(), header=TRUE, sep = ",")
NASA_snaive_2007 <-select(NASA_snaive_07,Point.Forecast)

percent.error.snaive <-abs(NASA_snaive_2007 - NASA_actual)/ NASA_snaive_2007
mean(percent.error.snaive$Point.Forecast)*100 

#MAPE is 1.000181 using snaive method

#calculate RMSE:
residual<-NASA_snaive_2007 - NASA_actual
Q7snaive_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7snaive_RMSE
#### RMSE for NASA using snaive method is 0.6164137 ############


##ETS (ANN) method (without seasonality and trend)####################################################

NASA_temp_ANN_2006 <- ets(NASA06_ts, model="ANN", damped=FALSE)
NASA_temp_ANN_2007_pred <- forecast(NASA_temp_ANN_2006, h=132, level=c(0.75, 0.90))

write.csv(NASA_temp_ANN_2007_pred,"2007-2017_ETS(ANN)_Predicted NASA.csv")

predicted_07_ETS_ANN <- read.csv(file.choose())

plot(NASA_temp_ANN_2007_pred, xlab="Year", ylab="Global Average Temperatures ANN 2007- 2017")

NASA_pred_07_ETS_ANN <- select (predicted_07_ETS_ANN,Point.Forecast)


percent.error.ets.ann.nasa <- abs(NASA_pred_07_ETS_ANN - NASA_actual)/NASA_pred_07_ETS_ANN
percent.error.ets.ann.nasa$Point.Forecast
mean(percent.error.ets.ann.nasa$Point.Forecast)*100  

## MAPE value is 0.6473988 with ANN_ets model

#calculate RMSE:
residual<-NASA_pred_07_ETS_ANN - NASA_actual
Q7ets_ANN_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7ets_ANN_RMSE
#### RMSE for NASA using ets(ANN) method is 0.4141276 ############




##UK dataset......................................................................................................................................

UK_data <- read_excel("C:/Users/sharg/Desktop/Geeta College Assignments/867- Predictive Modelling/Assignment 2/New folder/met_life data.xlsx")

UK_data$Temperature <- (UK_data$Temperature + 14)

UK_06 <- ts(UK_data$Temperature, start=c(1907,1), end=c(2006,12),frequency=12)

str(UK_06)

#####
## tbats model for uk dataset
######

UK06_tbats <- tbats(UK_06)
UK_tbats_pred <-forecast(UK06_tbats, h=132, level=c(0.75, 0.90))

##export the predicted temperature from 2007 to 2017 values in cvs

write.csv(UK_tbats_pred, "Predicted temp_UK07.csv")

### Read_predicted data/compare ##

predicted.uk.07 <- read.csv(file.choose(), header=TRUE, sep = ",")

str(predicted.uk.07)

actual.uk.data <- subset(UK_data,(UK_data$Year >=2007 & UK_data$Year <=2017))
uk.actual <- select(actual.uk.data, Temperature)
predicted.ukdata <- select(predicted.uk.07, Point.Forecast)
str(predicted.ukdata)
str(uk.actual)
uk.actual <- data.frame(uk.actual)
predicted.ukdata <- as.data.frame(predicted.ukdata)

percent.errors.uk <- abs(predicted.ukdata-uk.actual)/predicted.ukdata
mean(percent.errors.uk$Point.Forecast)*100 

#### mape for UK TBATS value is 4.540998 ############

#calculate RMSE:
residual<-predicted.ukdata - uk.actual
Q7tbats_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7tbats_ukRMSE
#### RMSE for UK using TBATS method is 0.6877331 ############



#UK data .............................................................................................................................
#Constant temperature approach _Naive method

UK_naive_pred07 <-naive(UK_06,h=132)
write.csv(UK_naive_pred07, "UK_07_pred_naive.csv")

#calculate MAPE
UK_naive_07 <-read.csv(file.choose(), header=TRUE, sep = ",")
UK_naive_07 <-select(UK_naive_07,Point.Forecast)

percent.error.uk.naive <-abs(UK_naive_07 - uk.actual)/ UK_naive_07
percent.error.uk.naive$Point.Forecast

mean(percent.error.uk.naive$Point.Forecast)*100 
#MAPE value using naive method is 4.260964

#calculate RMSE:
residual<-UK_naive_07 - uk.actual
Q7naive_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7naive_ukRMSE
#### RMSE for UK using naive method is 0.6173509 ############


##Seasonal naive method ####################################################
str(UK_06)

UK_snaive_pred07 <-snaive(UK_06,h=132)
write.csv(UK_snaive_pred07, "UK_07_pred_snaive.csv")

UK_snaive_07 <-read.csv(file.choose(), header=TRUE, sep = ",")
UK_snaive_2007 <-select(UK_snaive_07,Point.Forecast)

percent.error.uk.snaive <-abs(UK_snaive_2007 - uk.actual)/ UK_snaive_2007
mean(percent.error.uk.snaive$Point.Forecast)*100 

#MAPE value is 4.409947 using snaive method

#calculate RMSE:
residual<-UK_snaive_2007 - uk.actual
Q7snaive_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7snaive_ukRMSE
#### RMSE for UK using snaive method is 0.6808314 ############


##ETS (ANN) method (without seasonality and trend)####################################################

UK_temp_ANN_07 <- ets(UK_06, model="ANN", damped=FALSE)
UK_temp_ANN_2007_pred <- forecast(UK_temp_ANN_07, h=132, level=c(0.75, 0.90))

write.csv(UK_temp_ANN_2007_pred,"2007-17_ANN_Predicted UK.csv")

predicted_uk07_ETS_ANN <- read.csv(file.choose())

plot(UK_temp_ANN_2007_pred, xlab="Year", ylab="Global Average Temperatures ANN 2007- 2017")

UK_pred_07_ETS_ANN <- select (predicted_uk07_ETS_ANN,Point.Forecast)


percent.error.uk.ets_ANN<- abs(UK_pred_07_ETS_ANN - uk.actual)/UK_pred_07_ETS_ANN
percent.error.uk.ets_ANN$Point.Forecast
mean(percent.error.uk.ets_ANN$Point.Forecast)*100  

## MAPE value is 4.655204  with ANN_ets model

#calculate RMSE:
residual<-UK_pred_07_ETS_ANN - uk.actual
Q7UK_ets_ANN_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7UK_ets_ANN_RMSE
#### RMSE for UK using ets(ANN) method is 0.6682177 ############


################################## Plots_ Actual temperature vs. Predicted #####################################
nasa.tbats<- read.csv(file.choose())
nasa.naive <- read.csv(file.choose())
nasa.snaive <- read.csv(file.choose())
nasa.ANN <- read.csv(file.choose())

plot(NASA_actual$Temperature,ylab = "Global nasa temperatures", xlab = "Observations")
lines(nasa.tbats$Point.Forecast,col = "yellow")
lines(NASA_actual$Temperature,col="blue")
lines(nasa.ANN$Point.Forecast, col = "purple")
lines(nasa.naive$Point.Forecast, col = "Dark green")
lines(nasa.snaive$Point.Forecast, col = "orange")
legend("top", legend=c("TBATS","Actual","Naive","Snaive","ets(ANN)"), 
       col=c(" yellow", "blue","purple","purple","orange"), lty=1:6)

uk.tbats <-read.csv(file.choose())
uk.naive <- read.csv(file.choose())
uk.snaive <- read.csv(file.choose())
uk.ANN <- read.csv(file.choose())


plot(uk.actual$Temperature,ylab = "Global UK temperatures", xlab = "Observations")
lines(uk.tbats$Point.Forecast, col = "brown")
lines(uk.actual$Temperature,col="steelblue")
lines(uk.naive$Point.Forecast, col = "purple")
lines(uk.ANN$Point.Forecast, col = "Dark green")
lines(uk.snaive$Point.Forecast, col = "orange")
legend("top", legend=c("TBATS","Actual","Naive","Snaive","ets(ANN)"), 
       col=c(" Brown", "steelblue","purple","Dark green","orange"), lty=1:6)

