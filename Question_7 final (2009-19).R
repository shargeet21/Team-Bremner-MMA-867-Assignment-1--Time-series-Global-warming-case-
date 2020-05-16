library(ggplot2)
library(Amelia)
library(readr)
library(plyr)
library(readxl)
library(dplyr)
library(forecast)


##NASA dataset......................................................................................................................................
NASA_data <- read_excel(file.choose(), sheet=3)
str(NASA_data)

NASA_data$Temperature <- (NASA_data$Temperature+ 57.2)
str(NASA_data)

NASA_ts <- ts(NASA_data$Temperature, start=c(1880,1), end = c(2008,12) ,frequency=12) # Traing data

plot(NASA_ts)

##################
#TBATS

NASA_tbats <- tbats(NASA_ts)

# Using TBATS model from question 1 - predicting 2009 to 2019 temperature,  h =(11 years * 12 months = 132 months)

NASA_tbats_pred <- forecast(NASA_tbats, h= 132, level=c(0.75,0.90))

##### Plot #######
par(mfrow=c(1,1)) 
plot(NASA_tbats_pred, xlab="Year", ylab="Global Average Temperatures 2009-2019")
NASA_tbats

#### Export the predicted values temp (2009-19) in csv #####

write.csv(NASA_tbats_pred, "2009-19_Predicted_temp_NASA.csv")

##Predicted NASA temperatures from 2009 to 2019 tbats model ####

predicted.NASA.09 <-read.csv(file.choose())

NASA.predicted.09 <-select(predicted.NASA.09,Point.Forecast)
actual.NASA.data <- subset(NASA_data,(NASA_data$Year >=2009 & NASA_data$Year <=2019))

NASA.actual <-select(actual.NASA.data, Temperature)

percent.error.nasa <- abs(NASA.predicted.09- NASA.actual)/NASA.predicted.09
percent.error.nasa$Point.Forecast

mean(percent.error.nasa$Point.Forecast)*100 

###Mape for NASA is 0.3731635 using TBATS model

#calculate RMSE:
residual<-NASA.predicted.09 - NASA.actual
Q7NASA_TBATS_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7NASA_TBATS_RMSE
#### RMSE for NASA TABTS value is 0.2666195 ############



####Constant temperature approach _Naive method#######################

NASA_naive_pred09 <-naive(NASA_ts,h=132)
write.csv(NASA_naive_pred09, "NASA_09_pred_naive.csv")

#calculate MAPE

NASA_naive_09 <-read.csv(file.choose(), header=TRUE, sep = ",")
NASA_naive_09 <-select(NASA_naive_09,Point.Forecast)

percent.error.naive <- abs(NASA_naive_09 - NASA.actual)/ NASA_naive_09
percent.error.naive$Point.Forecast
mean(percent.error.naive$Point.Forecast)*100 
#MAPE value for NASA using naive method is 0.44872

#calculate RMSE:
residual<-NASA_naive_09 - NASA.actual
Q9_NASA_naive_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9_NASA_naive_RMSE
#### RMSE for NASA naive value is 0.3095133 ############







####Seasonal naive method ##################################
str(NASA_ts)

NASA_snaive_pred09 <-snaive(NASA_ts,h=132)
write.csv(NASA_snaive_pred09, "NASA_09_pred_snaive.csv")

NASA_snaive_09 <-read.csv(file.choose(), header=TRUE, sep = ",")
NASA_snaive_09 <-select(NASA_snaive_09,Point.Forecast)

percent.error.snaive <-abs(NASA_snaive_09 - NASA.actual)/ NASA_snaive_09
mean(percent.error.snaive$Point.Forecast)*100 
#MAPE value for NASA using snaive method is 0.4612283

#calculate RMSE:
residual<-NSA_snaive_09 - NASA.actual
Q9NASA_snaive_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9NASA_snaive_RMSE
#### RMSE for NASA using snaive method is 0.3244027 ############



###### Using ETS (ANN) Model###################

NASA_temp_ANN_2009 <- ets(NASA_ts, model="ANN", damped=FALSE)
NASA_temp_ANN_2009_pred <- forecast(NASA_temp_ANN_2009, h=132, level=c(0.75, 0.90))

write.csv(NASA_temp_ANN_2009_pred,"2009-19_(ANN)_Predicted NASA.csv")

predicted_09_ETS_ANN <- read.csv(file.choose())

plot(NASA_temp_ANN_2009_pred, xlab="Year", ylab="Global Average Temperatures ANN 2009- 2019")

NASA_pred_09_ETS_ANN <- select (predicted_09_ETS_ANN,Point.Forecast)

percent.error.ets_ann <- abs(NASA_pred_09_ETS_ANN - NASA.actual)/NASA_pred_09_ETS_ANN 
percent.error.ets_ann$Point.Forecast
mean(percent.error.ets_ann$Point.Forecast)*100  
## MAPE for NASA using ETS (ANN) method is 0.3709477

#calculate RMSE:
residual<-NASA_pred_09_ETS_ANN - NASA.actual
Q9NASA_ets_ann_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9NASA_ets_ann_RMSE
#### RMSE for NASA using ets(ANN) method is 0.2666888 ############


#################################################################################################################################################
#UK data set 

UK_data <- read_excel(file.choose())

UK_data$Temperature <-(UK_data$Temperature + 14)

UK_09 <- ts(UK_data$Temperature, start=c(1850,1), end=c(2008,12),frequency=12)

str(UK_09)

#####
## tbats model for uk dataset to predict year 2009 to 2019 (h = 11 years* 12 =132 months)
######

UK09_tbats <- tbats(UK_09)
UK_tbats_pred <-forecast(UK09_tbats, h=132, level=c(0.75, 0.90))

##export the predicted temperature from 2007 to 2017 values in cvs

write.csv(UK_tbats_pred, "2009-19_Predicted_temp_UK.csv")

### Read_predicted data/compare ##

predicted.uk.09 <- read.csv(file.choose(), header=TRUE, sep = ",")

str(predicted.uk.09)

actual.uk.data <- subset(UK_data,(UK_data$Year >=2009 & UK_data$Year <=2019))


uk.actual <- select(actual.uk.data, Temperature)
str(uk.actual)
predicted.ukdata <- select(predicted.uk.09, Point.Forecast)
str(predicted.ukdata)
str(uk.actual)
uk.actual <- data.frame(uk.actual)
predicted.ukdata <- as.data.frame(predicted.ukdata)

percent.errors.uk <- abs(predicted.ukdata-uk.actual)/predicted.ukdata
mean(percent.errors.uk$Point.Forecast)*100 
# MAPE is 0.7040578 for UK 2009 - 2019 predicted value using TBATS


#calculate RMSE:
residual<-predicted.ukdata - uk.actual
Q9tbats_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9tbats_ukRMSE
#### RMSE for UK using tbats method is 0.1414572 ############


####.....................................................................................................................................................
# constant approach with Naive method - using UK dataset

UK_naive_pred09 <-naive(UK_09,h=132)
write.csv(UK_naive_pred09, "UK_09_pred_naive.csv")

#calculate MAPE

UK_naive_09 <-read.csv(file.choose(), header=TRUE, sep = ",")
UK_naive_09 <-select(UK_naive_09,Point.Forecast)

percent.error.uk.naive <- abs(UK_naive_09 - uk.actual)/ UK_naive_09
percent.error.uk.naive$Point.Forecast
mean(percent.error.uk.naive$Point.Forecast)*100 
#MAPE value is 1.512889 for UK dataset using naive method

#calculate RMSE:
residual<-UK_naive_09 - uk.actual
Q9naive_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9naive_ukRMSE
#### RMSE for UK using naive method is 0.2577166 ############



##Seasonal naive method ####################################################
str(UK_09)

UK_snaive_pred09 <-snaive(UK_09,h=132)
write.csv(UK_snaive_pred09, "UK_09_pred_snaive.csv")

UK_snaive_09 <-read.csv(file.choose(), header=TRUE, sep = ",")
UK_snaive_09 <-select(UK_snaive_09,Point.Forecast)

percent.error.uk.snaive <-abs(UK_snaive_09 - uk.actual)/ UK_snaive_09
mean(percent.error.uk.snaive$Point.Forecast)*100 
##MAPE value is 1.57471 for UK using snaive method

#calculate RMSE:
residual<-UK_snaive_09 - uk.actual
Q9snaive_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9snaive_ukRMSE
#### RMSE for UK using snaive method is 0.2786768 ############



### Using ETS (ANN) Model##########################################
UK_temp_ANN_2009 <- ets(UK_09, model="ANN", damped=FALSE)
UK_temp_ANN_2009_pred <- forecast(UK_temp_ANN_2009, h=132, level=c(0.75, 0.90))

write.csv(UK_temp_ANN_2009_pred,"2009-2019_(ANN)_Predicted UK.csv")

predicted_uk09_ETS_ANN <- read.csv(file.choose())

plot(UK_temp_ANN_2009_pred, xlab="Year", ylab="Global Average Temperatures ANN 2009- 2019")

UK_pred_09_ETS_ANN <- select (predicted_uk09_ETS_ANN,Point.Forecast)

percent.error.uk.ets_ann <- abs(UK_pred_09_ETS_ANN - uk.actual)/UK_pred_09_ETS_ANN
percent.error.uk.ets_ann$Point.Forecast
mean(percent.error.uk.ets_ann$Point.Forecast)*100  
###  mape value is 1.170722 for UK using ETS (ANN) model

#calculate RMSE:
residual<-UK_pred_09_ETS_ANN - uk.actual
Q9ets_ann_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9ets_ann_ukRMSE
#### RMSE for UK using ets(ANN) method is 0.1952772  ############






######## Plots  Actual Temperature vs. Predicted temperature overview #############################
plot(NASA.actual$Temperature,ylab = "Global nasa temperatures", xlab = "Observations")
lines(NASA.predicted.09$Point.Forecast,col="Dark green") # tbats 2009 - 2019
lines(NASA.actual$Temperature,col="blue")
lines(NASA_naive_09$Point.Forecast,col="purple")
lines(NASA_snaive_09$Point.Forecast,col="orange")
lines(NASA_pred_09_ETS_ANN$Point.Forecast,col="pink")
legend("top", legend=c("TBATS","Actual","Naive","Snaive","ets(ANN)"), 
       col=c(" Dark green", "blue", "red","purple","orange","pink"), lty=1:6)


plot(actual.uk.data$Temperature, ylab = "Global UK temperature", xlab = "observations")
lines(predicted.ukdata$Point.Forecast,col="green") 
lines(actual.uk.data$Temperature,col="blue")
lines(UK_naive_09$Point.Forecast,col="orange")
lines(UK_snaive_09$Point.Forecast,col="purple")
lines(UK_pred_09_ETS_ANN$Point.Forecast,col="pink")

legend("top", legend=c("TBATS","Actual"," Naive","Snaive","ETS(ANN)"), 
       col=c("green", "blue", "red","orange","purple","pink"), lty=1:6)







