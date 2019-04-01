
library(forecast)
library(lubridate)
install.packages("tseries")
library(tseries)
setwd("/Users/vaishali/Documents/Documents/Semester II/Capstone/Cluster")
ts <- read.csv("password.csv")
attach(ts)

# Defining variables
Y <- Password
d.Y <- diff(Y)
t <- Day

# Descriptive statistics and plotting the data
summary(Y)
summary(d.Y)
plot(t,Y)
plot(d.Y)

# Dickey-Fuller test for variable
adf.test(Y, alternative="stationary", k=0)
adf.test(Y, alternative="explosive", k=0)
#augumented dickey-fuller
adf.test(Y, alternative="stationary")

# DF and ADF tests for differenced variable
adf.test(d.Y, k=0)
adf.test(d.Y)
# ACF and PACF
acf(Y)
pacf(Y)
acf(d.Y)
pacf(d.Y)
# ARIMA(1,0,0) or AR(1)
arima(Y, order = c(1,0,0))

# ARIMA on differenced variable
# ARIMA(1,1,0)
arima(d.Y, order = c(1,0,0))
# ARIMA(0,1,1)
arima(d.Y, order = c(0,0,1))
# ARIMA(1,0,1)
arima(d.Y, order = c(1,0,1))

# ARIMA(1,0,1) forecasting
mydata.arima101 <- arima(Y, order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima101, n.ahead=100)
plot (Y)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")

# ARIMA(1,1,1) forecasting
mydata.arima111 <- arima(d.Y, order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima111, n.ahead=100)
plot (d.Y)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")

