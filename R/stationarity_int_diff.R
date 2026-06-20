#check stationarity for interest rate differentials

stationarity_int <- function(data, home_int, US_int){
  
  #calculate daily interest rate differential scaled by days gap again, since this time series is used as regressor later
  data <- data %>% mutate(
    days_gap = as.numeric(difftime(dplyr::lead(Date, 1), Date, units = "days")),
    interest_rate_differential = {{home_int}} - {{US_int}},
    interest_rate_differential_daily = interest_rate_differential*days_gap/365) %>% 
    drop_na(interest_rate_differential_daily) #because lead,1 creates an NA
  
  
  #adf test
  #again use of drift but no trend !!!!explain in thesis
  adf_result <- ur.df(data$interest_rate_differential_daily, type = "drift", selectlags = "AIC")
  adf_stat      <- as.numeric(adf_result@teststat[, "tau2"])#extract the adf statistic
  adf_crit_5pct <- as.numeric(adf_result@cval["tau2", "5pct"])#extract 5% crit value
  adf_crit_1pct <- as.numeric(adf_result@cval["tau2", "1pct"])#extract 1% crit value
  
  #Phillips Perron test as robustness check
  #use constant to match drift of adf; Z-tau gives statistic that matches adf
  pp_result <- ur.pp(data$interest_rate_differential_daily, type = "Z-tau", model = "constant", lags = "short") #short for lag selection in Newey West   
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