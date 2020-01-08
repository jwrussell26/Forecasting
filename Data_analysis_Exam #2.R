library(lubridate)
library(zoo)
library(forecast)
data <- read.csv("DataFileSp19.csv", header = T)
names(data)
dataset_1 <- data[1:479,]
yrwk <- dataset_1[, 6]
date1 <- as.Date(paste(yrwk, 1), "%Y%U %u")

time_series <- ts(dataset_1$Pecent.of.deaths.due.to.pneumonia.or.influenza, frequency = 365.25/7, start = decimal_date(ymd(date1[1])))

#No Seasonality
best_fit1 <- list(aicc = Inf)
for(i in 1:25)
{
  fit1 <- auto.arima(time_series, xreg = fourier(time_series, K=i), seasonal = F)
  if(fit1$aicc < best_fit1$aicc)
    best_fit1 <- fit1
  else break;
  k_value1 <- max(i)
}

#Verify model
dif <- diff(time_series)
plot(dif, main = "ARIMA(2,1,4)")
acf_plot <- acf(dif, plot = F)
acf_plot$lag <- acf_plot$lag * 52
plot(acf_plot, col = "blue", xlab = "Lag (Weeks)")
pacf_plot <- acf(dif, lag.max = 104, type = "partial", plot = F)
pacf_plot$lag <- pacf_plot$lag * 52
plot(pacf_plot, col = "red", xlab = "Lag (Weeks)")

checkresiduals(best_fit1)


#Seasonality 
best_fit2 <- list(aicc = Inf)
for(i in 1:25)
{
  fit2 <- auto.arima(time_series, xreg = fourier(time_series, K=i), seasonal = T)
  if(fit2$aicc < best_fit2$aicc)
    best_fit2 <- fit2
  else break;
  k_value2 <- max(i)
}

#Verify Model
dif52 <- diff(dif, lag = 52)
plot(dif52, main = "ARIMA(0,1,2)(1,0,0)[52]")
acf_plot_season <- acf(dif52, lag.max = 104, plot = F)
acf_plot_season$lag <- acf_plot_season$lag * 52
plot(acf_plot_season, col = "blue", xlab = "Lag (Weeks)")
pacf_plot_season <- acf(dif52, lag.max = 104, type = "partial", plot = F)
pacf_plot_season$lag <- pacf_plot_season$lag * 52
plot(pacf_plot_season, col = "red", xlab = "Lag (Weeks)")

checkresiduals(best_fit2)

#Forecasting
pred <- forecast(best_fit2, xreg = fourier(time_series, K = 2, h = 104))
plot(pred)
