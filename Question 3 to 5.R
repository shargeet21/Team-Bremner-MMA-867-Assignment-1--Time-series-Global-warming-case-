
library(readxl)
library(tidyverse)
library(ggplot2)
library(GGally)
library(tidyr)
library(stringr)
library(lubridate)
install.packages('mice')
library(mice)
library(caret)
library(corrplot)
install.packages('fastDummies')
library(fastDummies)
library(dplyr)
library(forcats)
library(Matrix)
library(estimatr)
library(forecast)
library(zoo)
install.packages('MLmetrics')
library(Metrics)

## Load Data ##

UK.kingston <- read_excel(choose.files())

UK.kingston1 <- gather(UK.kingston, "Month", "Temp", 2:13)
UK.kingston1$Month <- match(UK.kingston1$Month,month.abb) # converting char month to number
UK.kingston1$time.ID <- as.yearmon(paste(UK.kingston1$Year, UK.kingston1$Month), "%Y %m")
UK.kingston1 <- UK.kingston1[c(4,3)]
UK.kingston1 <- arrange(UK.kingston1, UK.kingston1$time.ID)
UK.kingston1$Temp <- UK.kingston1$Temp + 14
UK.KINGSTON <- UK.kingston1[1:2042,]
remove(UK.kingston1)


## Question 3
Kingston.Temp.ts <- ts(UK.KINGSTON$Temp, start = 1850, frequency = 12)

TBATS.Kingston <- tbats(Kingston.Temp.ts)

predict.Kingston <- forecast(TBATS.Kingston, h=969, level=0.9)
plot(predict.Kingston)

write.csv(predict.Kingston, "C:\\Users\\qianh\\Desktop\\Kingston.Prediction.csv", row.names = TRUE)




## Question 4

NASA.global <- read_excel(choose.files(),sheet=3)
UK.global <- read_excel(choose.files(),sheet=2)

NASA.global$Temperature <- NASA.global$Temperature + 57.2
NASA.global.1951.1980.ts <- ts(NASA.global$Temperature, start = c(1951,1), end=c(1980,12), frequency = 12)

TBATS.NASA.1951.1980<- tbats(NASA.global.1951.1980.ts)
predict.NASA.1981.2020 <- forecast(TBATS.NASA.1951.1980, h=471, level=c(0.7,0.8,0.9,0.95))

plot(predict.NASA.1981.2020)

write.csv(predict.NASA.1981.2020, "C:\\Users\\qianh\\Desktop\\predict.NASA.1981.2020.csv", row.names = TRUE)


UK.global$Temperature <- UK.global$Temperature + 14
UK.global.1961.1990.ts <- ts(UK.global$Temperature, start = c(1961,1), end=c(1990,12), frequency = 12)

TBATS.UK.1961.1990<- tbats(UK.global.1961.1990.ts)
predict.UK.1991.2020 <- forecast(TBATS.UK.1961.1990, h=360, level=c(0.7,0.8,0.9,0.95))

plot(predict.UK.1991.2020)

write.csv(predict.UK.1991.2020, "C:\\Users\\qianh\\Desktop\\predict.UK.1991.2020.csv", row.names = TRUE)



## Question 5

NASA.Predict.2013.Temp <- c(56.66, 56.70, 56.72, 56.72, 56.73, 56.72, 56.77, 56.74, 56.76, 56.76, 56.74, 56.74)
NASA.Actual.2013.Temp <- c(57.91, 57.83, 57.87, 57.76, 57.82, 57.91, 57.81, 57.90, 57.97, 57.89, 58.05, 57.90)
NASA.Predict.2020.Temp <- c(56.72, 56.73, 56.72, 56.77, 56.74, 56.76, 56.76, 56.74, 56.74, 56.66, 56.70, 56.72)
NASA.Actual.2020.Temp <- c(58.22, 58.06, 58.12, 58.14, 58.14, 58.12, 58.22, 58.20, 58.30, 58.37, 58.45, 58.39)

SD.DEV.NASA.Predict.2013 <- sd(NASA.Predict.2013.Temp)
#0.0298481
SD.DEV.NASA.Actual.2013 <- sd(NASA.Actual.2013.Temp)
#0.07681146

SD.DEV.NASA.Predict.2020 <- sd(NASA.Predict.2020.Temp)
#0.0298481
SD.DEV.NASA.Actual.2020 <- sd(NASA.Actual.2020.Temp)
#0.1238859




UK.2013.predict.Temp <- c(13.83, 14.47, 14.15, 13.83, 13.79, 13.69, 14.14, 14.28, 14.11, 13.93, 13.82, 13.66)
UK.2013.Actual.Temp <- c(14.47, 14.50, 14.42, 14.45, 14.53, 14.50, 14.52, 14.54, 14.55, 14.52, 14.66,14.53)
UK.2020.predict.Temp <- c(14.15, 13.83, 13.79, 13.69, 14.14, 14.28, 14.11, 13.93, 13.82, 13.66, 13.83, 14.47)
UK.2020.Actual.Temp <- c(14.874, 14.78, 14.61, 14.708, 14.706, 14.719, 14.713, 14.752, 14.693, 14.88, 14.982, 14.999)


SD.DEV.UK.Predict.2013 <- sd(UK.2013.predict.Temp)
#0.2517033
SD.DEV.UK.Actual.2013 <- sd(UK.2013.Actual.Temp)
#0.05961366

SD.DEV.UK.Predict.2020 <- sd(UK.2020.predict.Temp)
#0.2517033
SD.DEV.UK.Actual.2020 <- sd(UK.2020.Actual.Temp)
#0.1219026





