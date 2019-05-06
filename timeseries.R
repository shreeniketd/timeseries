#install.packages("TTR")
#install.packages("forecast")
#install.packages("quadprog")
library(TTR)
library("forecast")
library("quadprog")
rm(list=ls())
dev.off()

#Birth Data Analysis

birth_data <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
timeseries <- ts(birth_data, frequency=12, start=c(1946,1))
timeseries
Table1 <- as.array(summary(timeseries))
Table1
write.csv(Table1,"table1.csv")
View(timeseries)
plot(timeseries)
component <-decompose(timeseries)
component
plot(component)

#Subtracting seasonal effect from original values
seasonallyAdjusted <- timeseries - component$seasonal
plot(seasonallyAdjusted)


#HoltWinters
forecast_holt <- HoltWinters(timeseries,gamma = FALSE)
forecast_holt
forecast_holt$SSE
plot(forecast_holt)
HoltWinters(timeseries, gamma = FALSE, l.start = 608, b.start = 9)
forecast_holt2 <- forecast(forecast_holt, h=10)
plot(forecast_holt2)



#Volcano Analysis
rm(list=ls())
dev.off()
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
volcanodustseries
plot(volcanodustseries)
Table2 <- as.array(summary(volcanodustseries))
Table2
write.csv(Table2,"table2.csv")

View(volcanodustseries)


#ACF PACF
acf_plot <- acf(volcanodustseries, lag.max = 20)
acf_plot
pacf_plot <- pacf(volcanodustseries, lag.max = 20)
pacf_plot

#Forecasting using acf, pacf 
volacnoseiresarima1 <- arima(volcanodustseries,order = c(2,0,0))
volacnoseiresarima1
volcanoseriesforecast1 <- forecast(volacnoseiresarima1, h=31)
volcanoseriesforecast1
plot(volcanoseriesforecast1)


#Auto Arima
auto.arima(volcanodustseries)

#Forecasting using Arima using Auto Arima function
volacnoseiresarima <- arima(volcanodustseries,order = c(1,0,2))
volacnoseiresarima

volcanoseriesforecast <- forecast(volacnoseiresarima, h=31)
volcanoseriesforecast
plot(volcanoseriesforecast)


library(tseries)
adf.test(volcanodustseries)
