library(ggplot2)
library(Amelia)
library(readr)
library(plyr)
library(readxl)
library(dplyr)
library(forecast)
remove(list = ls())


NASA <- read_excel(file.choose(), sheet=3)
str(NASA)
NASA$Temperature <- (NASA$Temperature+ 57.2)
str(NASA)



NASA_ts<- ts(NASA$Temperature, start=c(1880,1,1), frequency=12)
NASA_fit_multiplicative <- decompose(NASA_ts, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(NASA_fit_multiplicative)

NASA_fit_additive<- decompose(NASA_ts, type="additive") #decompose using "classical" method, additive form
plot(NASA_fit_additive)

NASA_fit_periodic <- stl(NASA_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(NASA_fit_periodic)

plot(NASA_ts)

################################################################

NASA_temperature_AAA <- ets(NASA_ts, model="AAA", damped=FALSE)
NASA_temperature_AAA_d <- ets(NASA_ts, model="AAA", damped=TRUE)
NASA_temperature_MMM_d <- ets(NASA_ts, model="MMM", damped=TRUE)
NASA_temperature_MMM <- ets(NASA_ts, model="MMM", damped=FALSE)
NASA_temperature_ZMM <- ets(NASA_ts, model="MMM", damped=FALSE)


NASA_temperature_AAA_pred <- forecast(NASA_temperature_AAA, h=969, level=c(0.75, 0.90))
NASA_temperature_AAA_pred_d <- forecast(NASA_temperature_AAA_d, h=969, level=c(0.75, 0.90))
NASA_temperature_MMM_pred_d <- forecast(NASA_temperature_MMM_d, h=969, level=c(0.75, 0.90))
NASA_temperature_MMM_pred <- forecast(NASA_temperature_MMM, h=969, level=c(0.75, 0.90))

NASA_tbats <- tbats(NASA_ts)
NASA_tbats_pred <-forecast(NASA_tbats, h=969, level=c(0.75, 0.90))

# Compare the prediction "cones" visually
par(mfrow=c(1,4)) 
par(mfrow=c(1,1)) 
plot(NASA_temperature_AAA_pred, xlab="Year", ylab="Global Average Temperatures")
plot(NASA_temperature_AAA_pred_d, xlab="Year", ylab="Global Average Temperatures")
plot(NASA_temperature_MMM_pred_d, xlab="Year", ylab="Global Average Temperatures")
plot(NASA_temperature_MMM_pred, xlab="Year", ylab="Global Average Temperatures")

NASA_temperature_AAA
NASA_temperature_MMM

###TBATS
par(mfrow=c(1,1)) 
plot(NASA_tbats_pred, xlab="Year", ylab="Global Average Temperatures")
NASA_tbats


### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)
f_AAA  <- function(y, h) forecast(ets(y, model="AAA", damped=FALSE), h = h)
errors_AAA <- tsCV(NASA_ts, f_AAA, h=1, window=842)

f_AAA_d  <- function(y, h) forecast(ets(y, model="AAA",damped=TRUE), h = h)
errors_AAA_d <- tsCV(NASA_ts, f_AAA_d, h=1, window=842)

f_MMM_d  <- function(y, h) forecast(ets(y, model="MMM", damped=TRUE), h = h)
errors_MMM_d <- tsCV(NASA_ts, f_MMM_d, h=1, window=842)

f_MMM  <- function(y, h) forecast(ets(y, model="MMM", damped=FALSE), h = h)
errors_MMM <- tsCV(NASA_ts, f_MMM, h=1, window=842)


####################################### Plots errors
par(mfrow=c(1,1)) 
plot(errors_AAA, ylab='tsCV errors')
abline(0,0)
lines(errors_AAA_d, col="red")
lines(errors_MMM_d, col="green")
lines(errors_MMM, col="blue")
legend("left", legend=c("CV_error_AAA", "CV_error_AAA_d","CV_error_MMM_d","CV_error_MMM"), col=c("black", "red", "green", "blue"), lty=1:4)
################################## Plots

mean(abs(errors_AAA/NASA_ts), na.rm=TRUE)*100
mean(abs(errors_AAA_d/NASA_ts), na.rm=TRUE)*100
mean(abs(errors_MMM_d/NASA_ts), na.rm=TRUE)*100
mean(abs(errors_MMM/NASA_ts), na.rm=TRUE)*100

f_TBATS  <- function(y, h) forecast(tbats(y), h = h)
errors_TBATS <- tsCV(NASA_ts, f_TBATS, h=1, window=842)
mean(abs(errors_TBATS/NASA_ts), na.rm=TRUE)*100

plot(errors_AAA, ylab='tsCV errors', col="green")
abline(0,0)
lines(errors_MMM, col="blue")
lines(errors_TBATS, col="gray")
legend("left", legend=c("CV_error_AAA", "CV_error_MMM","CV_error_TBATS"), col=c("green", "blue", "gray"), lty=1:4)

# Print the mean and confidence intervals for the MMZ model
NASA_tbats_pred

# Export the results out

file1<-data.frame(Year = NASA$Year,Temparature = NASA_tbats_pred)
write.csv(NASA_tbats_pred, file1 = "Predicted Global Average Nasa.csv") # export the selected model's predictions into a CSV file
