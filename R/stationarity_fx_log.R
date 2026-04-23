#check stationarity for fx log returns

stationarity_fx <- function(data, fx){
  
  data <- data %>% mutate(
    log_returns = (log(lead({{fx}},1)) - log({{fx}}))*100) %>% 
      drop_na(log_returns)#since lead,1 creates an NA at the end of the column
  
  data_fx <- data$log_returns
  
  #adf test
  adf_result <- ur.df(data_fx, type = "drift", selectlags = "AIC")#use AIC to select the lags
  #drift to be on the safe side, in case the log returns exhibit a small drift; also legitimate to use no drift when assuming that log returns fluctuate around mean of 0
  #!!!!important to explain in thesis
  adf_stat      <- as.numeric(adf_result@teststat[, "tau2"]) #extract the ADF test statistic
  adf_crit_5pct <- as.numeric(adf_result@cval["tau2", "5pct"]) #extract 5% critical value
  

  return(list(
    adf_stat = adf_stat,
    adf_crit_5pct = adf_crit_5pct
  ))
  
  
}