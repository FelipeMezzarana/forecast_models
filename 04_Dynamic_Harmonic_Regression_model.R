
library(forecast)

#importing data
min_temp <- read.csv("C:/Users/spine/PYTHON/daily_mim_temp/daily-minimum-temperatures-in-me-treated_v2.csv")
tail(min_temp)
temperatures = min_temp[c("daily_min_temp")]


## Create a daily Date object - helps my work on dates
inds <- seq(as.Date("1981-01-02"), as.Date("1990-12-31"), by = "day")

## Create a time series object
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
checkresiduals(bestfit, lag=15)

#testing (delete later)
final_fit <- Arima(myts,order=c(3,1,1), xreg = fourier(myts, K = 2))
final_fit[["aicc"]]
fcast <- forecast(final_fit, xreg = fourier(myts, K = 2))

#validation loop  - under development
train_ts <- myts
for (i in seq(3)){
  train_ts <- head(train_ts,-1)  
  tail(train_ts)
}





