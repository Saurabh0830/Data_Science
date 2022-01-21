library('forecast')
library('ggplot2')
library('tseries')

ecomm=read.csv('C:/Users/sbtha/Desktop/Time Series and Logistic regression/eComm_US.csv')
str(ecomm)
ecomm$DATE=as.Date(ecomm$DATE,format = "%Y-%m-%d")
str(ecomm)
ecomm_ts=ts(ecomm$ECOMNSA,start =c(1999,4), end =c(2021,2), freq = 4)
ecomm_ts

class(ecomm_ts)
start(ecomm_ts)
end(ecomm_ts)
frequency(ecomm_ts)

plot.ts(ecomm_ts)

abline(reg=lm(ecomm_ts~time(ecomm_ts)))

cycle(ecomm_ts)

plot(aggregate(ecomm_ts,FUN=mean))

boxplot(ecomm_ts~cycle(ecomm_ts))

ecomm_ts_components <- decompose(ecomm_ts, "multiplicative")

plot(ecomm_ts_components)

ecomm_ts1<-ecomm_ts-ecomm_ts_components$seasonal
plot(ecomm_ts1)

#==============================================================================
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors,na.rm = T)/4
  mysd   <- sd(forecasterrors,na.rm = T)
  mymin  <- min(forecasterrors,na.rm = T) - mysd*5
  mymax  <- max(forecasterrors,na.rm = T) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


#==============================================================================

#Model1 using Average
ecomm.mean<-meanf(ecomm_ts,h=10)
ecomm.mean$model
plot(ecomm.mean)
accuracy(ecomm.mean)
autoplot(ecomm.mean)+
  autolayer(fitted(ecomm.mean),series = "Fitted")

#Model2 using naive
ecomm.naive<-naive(ecomm_ts,h=10)
ecomm.naive$model
plot(ecomm.naive)
accuracy(ecomm.naive)
autoplot(ecomm.naive)+
  autolayer(fitted(ecomm.naive),series = "Fitted")

#model3 using seasonal naive
ecomm.seasonalnaive<-snaive(ecomm_ts,h=3)
summary(ecomm.seasonalnaive)
plot(ecomm.seasonalnaive)
accuracy(ecomm.seasonalnaive)
autoplot(ecomm.seasonalnaive)+
  autolayer(fitted(ecomm.seasonalnaive),series = "Fitted")

#Model4 using simple expontential smoothing without trend and sesonality
ecomm.ses<-ses(ecomm_ts, h=3)
summary(ecomm.ses)
round(accuracy(ecomm.ses),2)
plot(ecomm.ses)
autoplot(ecomm.ses)+
  autolayer(fitted(ecomm.ses),series = "Fitted")

#Model5 using error trend and seasonality algoithm with ANN method
ecomm.ANN<-ets(ecomm_ts, model = "AAN")
summary(ecomm.ANN)
forecast(ecomm.ANN,3)
round(accuracy(ecomm.ANN),2)
autoplot(ecomm.ANN)+
  autolayer(fitted(ecomm.ANN),series = "Fitted")

#Model6 using ets algorithm with zzz method
ecomm.ZZZ<-ets(ecomm_ts, model = "ZZZ")
summary(ecomm.ZZZ)
forecast(ecomm.ZZZ,3)
round(accuracy(ecomm.ZZZ),2)
autoplot(ecomm.ZZZ)+
  autolayer(fitted(ecomm.ZZZ),series = "Fitted")
plot.ts(ecomm.ZZZ$residuals)     
plotForecastErrors(ecomm.ZZZ$residuals)
#Ljung-Box test
acf(ecomm.ZZZ$residuals,na.action = na.pass,lag=20)
Box.test(ecomm.ZZZ$residuals, lag=20, type="Ljung-Box")

#Model7 using Holt-Winter method with additive season
ecomm.hw<-hw(ecomm_ts,10)
ecomm.hw$model
plot(ecomm.hw)
accuracy(ecomm.hw)
autoplot(ecomm.hw)+
  autolayer(fitted(ecomm.hw),series = "Fitted")

#Model8 using Holt-Winter method with multiplicative season
ecomm.hwm<-hw(ecomm_ts,seasonal = c('multiplicative'),10)
ecomm.hwm$model
accuracy(ecomm.hwm)
plot(ecomm.hwm)

autoplot(ecomm.hwm)+
  autolayer(fitted(ecomm.hwm),series = "Fitted")
plot.ts(ecomm.hwm$residuals)     
plotForecastErrors(ecomm.hwm$residuals)
#Ljung-Box test
acf(ecomm.hwm$residuals,na.action = na.pass,lag=20)
Box.test(ecomm.hwm$residuals, lag=20, type="Ljung-Box")

#================================================================================

#Model9 ARIMA
#To perform arima analysis the time series should be stationary.
plot(ecomm_ts_components)
#To make time series stationary, mean, variance and co-varience should be constant or should not vary with time.
#take log to make mean constant
ecomm_ts_log=log(ecomm_ts)
plot(ecomm_ts_log)
adf.test(ecomm_ts_log)
kpss.test(ecomm_ts_log, null = 'Trend')
#take diff to make variance constant
ecomm_ts_log_diff1=diff(ecomm_ts_log,differences = 1)
plot(ecomm_ts_log_diff1)
adf.test(ecomm_ts_log_diff1)
kpss.test(ecomm_ts_log_diff1, null = 'Trend')
#diff2
ecomm_ts_log_diff2=diff(ecomm_ts_log, differences = 2)
plot(ecomm_ts_log_diff2)
adf.test(ecomm_ts_log_diff2)
#diff3
ecomm_ts_log_diff3=diff(ecomm_ts_log, differences = 3)
plot(ecomm_ts_log_diff3)
adf.test(ecomm_ts_log_diff3)

#For ARIMA model we need three parameters which is p,q and d.from above differing method we got d=1.
#for q , we need check autocorrelation and partial autocorrelation
acf(ecomm_ts_log_diff1,lag.max=20)             # plot a correlogram
acf(ecomm_ts_log_diff1, lag.max=20, plot=FALSE)


pacf(ecomm_ts_log_diff1,lag.max=20)             # plot a correlogram
pacf(ecomm_ts_log_diff1, lag.max=20, plot=FALSE)

#Fitting an ARIMA model
fit_arima <- arima(ecomm_ts_log, order=c(0,1,0))
summary(fit_arima)

#Evaluating Model Fit
qqnorm(fit_arima$residuals)
qqline(fit_arima$residuals)
Box.test(fit_arima$residuals, type="Ljung-Box")
checkresiduals(fit_arima)
accuracy(fit_arima)


#Auto ARIMA
fit.ecomm<-auto.arima(ecomm_ts_log)
fit.ecomm

#SARIMA
fit_sarima<-arima(ecomm_ts_log, order=c(0,1,0),seasonal = c(1,1,0))
summary(fit_sarima)
forecast(2.18*fit_sarima, h=3)
#Evaluating Model Fit
qqnorm(fit_sarima$residuals)
qqline(fit_sarima$residuals)
Box.test(fit_sarima$residuals, type="Ljung-Box")
checkresiduals(fit_sarima)
accuracy(fit_sarima)

pred=predict(fit_sarima,n.ahead = 3*4)
pred1= 2.718^pred$pred
pred1
ts.plot(ecomm_ts, 2.718^pred$pred, log='y',lty=c(1,10))
#===============================================================================
#Comparing model
ecomm_ts_train <- window(ecomm_ts,start =c(1999,4))
autoplot(window(ecomm_ts_train, start=c(1999,4))) +
  autolayer(ecomm.mean, series="Mean", PI=FALSE) +
  autolayer(ecomm.naive, series="Naïve", PI=FALSE) +
  autolayer(ecomm.seasonalnaive, series="Seasonal naïve", PI=FALSE) +
  autolayer(ecomm.ses, series="Simle Exponential Smoothing", PI=FALSE) +
  autolayer(ecomm.hw, series="Holt-Winter with additive", PI=FALSE) +
  autolayer(ecomm.hwm, series="Holt-Winter with multiplicative", PI=FALSE) +
  xlab("Year") + ylab("ECOMNUSA") +
  ggtitle("Forecasts for quarterly E-Commerce retaile sales in US") +
  guides(colour=guide_legend(title="Forecast"))


