
library(ggplot2)
library(forecast)
library(tseries)
library(tidyverse)
library(rio)
library(readxl)

data1 <- read.csv("ppr_data_encoded.csv")

#Must be in a ymd data format

data$date <- data[, ymd(date)]

#set up date class
data1$date =as.Date(data1$date)

options(scipen = 999)
ggplot(data1, aes(date, price)) + geom_line() + scale_x_date("Year")

Dublin <- subset(data1, ppr_county=="Dublin",
                 select=c(price, year, date))

#Set up Dublin subset for date class 

Dublin$date =as.Date(Dublin$date)

ggplot(Dublin, aes(date, price)) + geom_line() + scale_x_date("Month")

ggplot(Dublin, aes(date, price)) +geom_point(color = "navyblue") +
  facet_wrap(~ year)+ scale_x_date("year") + ylab("HousePrice") +
  xlab("")

count_TSObject = ts(Dublin[, c("price")])

Dublin$clean_count = tsclean(count_TSObject)

ggplot()+
  geom_line(data = Dublin, aes(x = date, y = clean_count)) + ylab("Cleaned count")

Dublin$cnt_ma = ma(Dublin$clean_count, order = 7)
Dublin$cnt_ma30 = ma(Dublin$clean_count, order = 30)
ggplot()+
  geom_line(data = Dublin, aes(x = date, y = clean_count, colour = "Counts")) +
  geom_line(data = Dublin, aes(x = date, y = cnt_ma, colour = "Weekly Moving Averages")) +
  geom_line(data = Dublin, aes(x = date, y = cnt_ma30, colour = "Monthly Moving Average")) +
  scale_y_continuous(expand = c(0,0))+
  ylab("Price")

count_ma = ts(na.omit(Dublin$cnt_ma), frequency = 30)
decomp = stl(count_ma, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

adf.test(count_ma, alternative = "stationary")

#Dickey fuller = -24.553, Lag Order = 41, p-value = 0.01
#showcasing that this data is non stationary. 

Acf(count_ma, main = "")
#Plots display correlation between a series and its lags

pacf(count_ma, main = "")
#Plots display correlation between a series and its lags that explained by its previous lags
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)

adf.test(count_d1, alternative = "stationary")
#Dickey - fuller = -68.12, lag order = 41

Acf(count_d1, main = "ACF for Differentiated Series")

Pacf(count_d1, main = "PACF for Differentiated Series")

auto.arima(deseasonal_cnt, seasonal = FALSE)
#ARIMA (4,1,0)

fit <- auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 60, main = "(1,1,1) Model Residuals")
#Default ARIMA values, single lag at 7
#Modify model for p or q at 7

fit2 <- arima(deseasonal_cnt, order = c(1,1,7))
tsdisplay(residuals(fit2), lag.max = 45, main = "Seasonal Model Residuals")

fit3 <- arima(deseasonal_cnt, order = c(1,0,7))
tsdisplay(residuals(fit3), lag.max = 60, main = "ARIMA Model Residuals")

fcast <- forecast(fit2, h = 5000)
plot(fcast)

#Test model performance
hold <- window(ts(deseasonal_cnt), start = 60310)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(60310:70310)]), order = c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout, h = 10000)
plot(fcast_no_holdout, main = "")
#lines(ts(deseasonal_cnt))
#col = rgb(0, 0, 0, 0.15)

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal = TRUE)
seas_fcast <- forecast(fit_w_seasonality, h = 1000)
plot(seas_fcast)

lines(ts(count_ma))
lines(ts(deseasonal_cnt))

tsdisplay(residuals(fit_w_seasonality), lag.max = 50, main = "Seasonal model residuals")

fit4 = auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit4), lag.max = 50, main = "Seasonal model residuals")

fit5 = arima(deseasonal_cnt, order = c(1,1,1))      
tsdisplay(residuals(fit5), lag.max = 50, main = "Seasonal model residuals")

par(mfrow = c(2,2))

fcast <- forecast(fit_w_seasonality, h = 1000)
plot(fcast)

fcast2<- forecast(fit3, h = 1000)
plot(fcast2)

fcast3 <- forecast(fit4, h = 1000)
plot(fcast3)

fcast4 <- forecast(fit5, h = 1000)
plot(fcast4)
