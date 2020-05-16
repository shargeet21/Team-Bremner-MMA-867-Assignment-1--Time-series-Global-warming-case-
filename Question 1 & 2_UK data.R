library(forecast)
library(readxl)

UKData<-read.csv(file.choose())
str(UKData)

UK_ts <- ts(UKData$Temperature,start=1850, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

#plot various decompositions into error/noise, trend and seasonality

fit <- decompose(UK_ts, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(fit)

fit <- decompose(UK_ts, type="additive") #decompose using "classical" method, additive form
plot(fit)

fit <- stl(UK_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fit)

plot(UK_ts)

# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
UK_AAA <- ets(UK_ts, model="AAA")
UK_AAA_d <- ets(UK_ts, model="AAA", damped=TRUE)
UK_AAN <- ets(UK_ts, model="AAN")
UK_AAN_d <- ets(UK_ts, model="AAN",damped=TRUE)
UK_AAZ <- ets(UK_ts, model="AAZ", damped=FALSE)
UK_AAZ_d <- ets(UK_ts, model="AAZ", damped=TRUE)
UK_MMN <- ets(UK_ts, model="MMN", damped=FALSE)
UK_MMN_d <- ets(UK_ts, model="MMN", damped=TRUE)
UK_MMZ <- ets(UK_ts, model="MMZ", damped=FALSE)
UK_MMZ_d <- ets(UK_ts, model="MMZ", damped=FALSE)
UK_MMM <- ets(UK_ts, model="MMM", damped=FALSE)
UK_MMM_d <- ets(UK_ts, model="MMM", damped=FALSE)

# Create their prediction "cones" for 360 months (30 years) into the future with quintile confidence intervals
UK_AAA_pred <- forecast(UK_AAA, h=970, level=c(0.75, 0.90))
UK_AAA_pred_d <- forecast(UK_AAA_d, h=970, level=c(0.75, 0.90))
UK_AAN_pred <- forecast(UK_AAN, h=970, level=c(0.75, 0.90))
UK_AAN_pred_d <- forecast(UK_AAN_d, h=970, level=c(0.75, 0.90))
UK_AAZ_pred <- forecast(UK_AAZ, h=970, level=c(0.75, 0.90))
UK_AAZ_pred_d <- forecast(UK_AAZ_d, h=970, level=c(0.75, 0.90))
UK_MMN_pred <- forecast(UK_MMN, h=970, level=c(0.75, 0.90))
UK_MMN_pred_d <- forecast(UK_MMN_d, h=970, level=c(0.75, 0.90))
UK_MMZ_pred <- forecast(UK_MMZ, h=970, level=c(0.75, 0.90))
UK_MMZ_pred_d <- forecast(UK_MMZ_d, h=970, level=c(0.75, 0.90))
UK_MMM_pred <- forecast(UK_MMZ, h=970, level=c(0.75, 0.90))
UK_MMM_pred_d <- forecast(UK_MMZ_d, h=970, level=c(0.75, 0.90))

# Compare the prediction "cones" visually
###par(mfrow=c(1,4)) # This command sets the plot window to show 1 row of 4 plots
###plot(UK_AAN_pred, xlab="Year", ylab="Predicted Temperature")
####plot(UK_MMN_pred, xlab="Year", ylab="Predicted Temperature")
###plot(UK_AAZ_pred, xlab="Year", ylab="Predicted Temperature")
##plot(UK_MMZ_pred, xlab="Year", ylab="Predicted Temperature")

# Lets look at what our models actually are -- ETS
##UK_AAZ
##UK_MMZ

#Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model
UK_tbats <- tbats(UK_ts)
UK_tbats_pred <-forecast(UK_tbats, h=970, level=c(0.75, 0.90))

# Lets look at what our models actually are -- TBATS
##UK_tbats

###
### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)
###
f_AAA  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA <- tsCV(UK_ts, f_AAA, h=1, window=1021)

f_AAA_d  <- function(y, h) forecast(ets(y, model="AAA", damped=TRUE), h = h)
errors_AAA_d <- tsCV(UK_ts, f_AAA_d, h=1, window=1021)

f_AAZ  <- function(y, h) forecast(ets(y, model="AAZ"), h = h)
errors_AAZ <- tsCV(UK_ts, f_AAN, h=1, window=1021)

f_AAZ_d  <- function(y, h) forecast(ets(y, model="AAZ", damped=TRUE), h = h)
errors_AAZ_d <- tsCV(UK_ts, f_AAN_d, h=1, window=1021)

f_AAN  <- function(y, h) forecast(ets(y, model="AAN"), h = h)
errors_AAN <- tsCV(UK_ts, f_AAN, h=1, window=1021)

f_AAN_d  <- function(y, h) forecast(ets(y, model="AAN", damped=TRUE), h = h)
errors_AAN_d <- tsCV(UK_ts, f_AAN_d, h=1, window=1021)

f_MMN  <- function(y, h) forecast(ets(y, model="MMN"), h = h)
errors_MMN <- tsCV(UK_ts, f_MMN, h=1, window=1021)

par(mfrow=c(1,1)) 
plot(errors_AAN, ylab='tsCV errors')
abline(0,0)
lines(errors_AAZ, col="red")
lines(errors_AAA, col="green")
lines(errors_MMN, col="blue")
legend("left", legend=c("CV_error_AAN", "CV_error_MMN","CV_error_AAA","CV_error_AAZ"), col=c("black", "red", "green", "blue"), lty=1:4)
mean(abs(errors_AAA/UK_ts), na.rm=TRUE)*100
mean(abs(errors_AAA_d/UK_ts), na.rm=TRUE)*100
mean(abs(errors_AAN/UK_ts), na.rm=TRUE)*100
mean(abs(errors_AAN_d/UK_ts), na.rm=TRUE)*100
mean(abs(errors_AAZ/UK_ts), na.rm=TRUE)*100
mean(abs(errors_AAZ_d/UK_ts), na.rm=TRUE)*100
mean(abs(errors_MMN/UK_ts), na.rm=TRUE)*100


accurate(errors)

f_TBATS  <- function(y, h) forecast(tbats(y), h = h)
errors_TBATS <- tsCV(UK_ts, f_TBATS, h=1, window=1021)

##plot(errors_AAA, ylab='tsCV errors', col="green")
##abline(0,0)
##lines(errors_MMM, col="blue")
##lines(errors_TBATS, col="gray")
##legend("left", legend=c("CV_error_AAA", "CV_error_MMM","CV_error_TBATS"), col=c("green", "blue", "gray"), lty=1:4)

mean(abs(errors_TBATS/UK_ts), na.rm=TRUE)*100

# Print the mean and confidence intervals for the MMZ model
UK_tbats_pred

# Export the results out
write.csv(UK_tbats_pred, file = "Predicted TemperaturesUK1.csv") # export the selected model's predictions into a CSV file

