
library(forecast)

#importing data
min_temp <- read.csv("C:/Users/spine/PYTHON/daily_mim_temp/daily-minimum-temperatures-in-me-treated_v2.csv")
tail(min_temp)
temperatures <- min_temp[c("daily_min_temp")]

# Create a daily Date object - helps my work on dates
inds <- seq(as.Date("1981-01-02"), as.Date("1990-12-31"), by = "day")

# Create a time series object
myts <- ts(temperatures,    start = c(1981, as.numeric(format(inds[1], "%j"))),frequency = 365)

bestfit <- list(aicc=Inf)
for (i in seq(12)) {
  fit <- auto.arima(myts, xreg = fourier(myts, K = i))
  aiic <- fit[["aicc"]]
  print(aiic)
  
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- i}
}

#checking our model 
print(bestK)
print(bestfit)
final_fit[["aicc"]]
checkresiduals(bestfit, lag=15)

#The auto.arima found an optimized model with order=c(3,1,1) and K = 2

#validation loop, lets train the model only with the data collected before the forecast date. 
#That is, we will train a model for each forecast, dropping the previous data.
train_ts <- myts
fcast_list <- list()
date_list <-list()
for (i in seq(3250)){
  train_ts <- head(train_ts,-1)  
  final_fit <- Arima(train_ts,order=c(3,1,1), xreg = fourier(train_ts, K = 2))
  fcast <- forecast(final_fit, h=1, xreg = fourier(train_ts, K = 2))
  fcast_df <- data.frame(fcast)
  point_fcast <- fcast_df[1,1]
  fcast_list<- append(fcast_list,point_fcast)
  date_list<-append(date_list,min_temp[3653-i,1])
}

#create and save DataFrame
fcast_df <- do.call(rbind, Map(data.frame, date=date_list, forecast=fcast_list))
write.csv(fcast_df, "C:\\Users\\spine\\PYTHON\\forecast_models\\Forecast Files\\dynamic_regression_forecast.csv", row.names=FALSE)


