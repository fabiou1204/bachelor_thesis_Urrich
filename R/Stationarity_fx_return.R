#check stationarity for fx log returns

stationarity_fx <- function(data, fx){
  
  data <- data %>% mutate(
    log_returns = (log(dplyr::lead({{fx}},1)) - log({{fx}}))*100) %>% 
      drop_na(log_returns)#since lead,1 creates an NA at the end of the column
  
  data_fx <- data$log_returns
  
  #adf test
  adf_result <- ur.df(data_fx, type = "drift", selectlags = "AIC")#use AIC to select the lags
  #drift to be on the safe side, in case the log returns exhibit a small drift; also legitimate to use no drift when assuming that log returns fluctuate around mean of 0
  #!!!!important to explain in thesis
  adf_stat      <- as.numeric(adf_result@teststat[, "tau2"])#extract the ADF test statistic
  adf_crit_5pct <- as.numeric(adf_result@cval["tau2", "5pct"]) #extract 5% critical value
  adf_crit_1pct <- as.numeric(adf_result@cval["tau2", "1pct"])#extract 1% critical value
  
  
  
  #Phillips Perron test as robustness check
  #use constant to match drift of adf; Z-tau gives statistic that matches adf
  pp_result <- ur.pp(data$log_returns, type = "Z-tau", model = "constant", lags = "short")#short for lag selection in Newey West  
  pp_stat <- as.numeric(pp_result@teststat)#get test statistic
  pp_crit_5pct <- as.numeric(pp_result@cval[, "5pct"])#extract 5% crit value
  pp_crit_1pct <- as.numeric(pp_result@cval[, "1pct"])#extract 1% crit value
  

  return(list(
    adf_stat = adf_stat,
    adf_crit_5pct = adf_crit_5pct,
    adf_crit_1pct = adf_crit_1pct,
    pp_stat = pp_stat,
    pp_crit_5pct = pp_crit_5pct,
    pp_crit_1pct = pp_crit_1pct
  ))
  
  
}