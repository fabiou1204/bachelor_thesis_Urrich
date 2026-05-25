#test for stationarity for one single time series, so for each individual fx and interest rate time series
#give fx rates as log fx rates for comparibility with log fx returns

stationrity_1 <- function(data, series){
  #ADF test with standard number of lags
  #for fx data and interest rate data: use constant but not trend
  series_data <- data[[series]]
  series_data <- series_data %>% na.omit()

  adf_result <- ur.df(series_data, type = "drift", selectlags = "AIC")#use AIC to select the lags
  adf_stat      <- as.numeric(adf_result@teststat[, "tau2"]) #extract the ADF test statistic
  adf_crit_5pct <- as.numeric(adf_result@cval["tau2", "5pct"]) #extract 5% critical value
  
  
  #function returns all relevant values to decide whether we have stationarity or not
  return(list(
    adf_stat = adf_stat,
    adf_crit_5pct = adf_crit_5pct
  ))
}