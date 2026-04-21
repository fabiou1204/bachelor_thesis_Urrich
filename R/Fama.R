#fama regression from 01-01-2006 t0 31-12-2025 as benchmark for 

Fama <- function(data, fx, home_int, US_int){
  
  #only use data from 01-01-2006 onwards, because volatility prediction will only be done for dates starting from 01-01-2006
  data <- data %>% filter(Date >= as.Date("2006-01-01") & Date <= as.Date("2025-12-31"))
  
  #calculate log returns of exchange rate
  data <- data %>%
    mutate(log_returns = (log(lead({{fx}}, 1)) - log({{fx}}))*100) 
  #multiply by 100 to have log returns in percentage points, otherwise values are very small and hard to interpret ->better interpretation of beta
  #both fx log returns and interest rate differentials are in percentages; important to have same scale
  
  #calculate interest rate differential
  #assuming US as foreign country
  data <- data %>% mutate(
    days_gap = as.numeric(difftime(lead(Date, 1), Date, units = "days")),
    interest_rate_differential = {{home_int}} - {{US_int}},
    #????consider leap years: divide by 366 on each day that is at least one year ahead of the 29 february of the leap year
    #although rates are annualised, so dividing by 365 should be correct
    #for simplicity I just divide by 365 as often done in finance
    interest_rate_differential_daily =interest_rate_differential*days_gap/365) #%>% 
    #na.omit()#because lead,1 in fx log returns creates ony NA at the end of the dataframe
  
  
  
  #basic OLS Fama regression
  fama_model <- lm(log_returns ~ interest_rate_differential_daily, data = data)
  
  
  arch_lag1 <- ArchTest(residuals(fama_model),lags = 1)
  arch_lag3 <- ArchTest(residuals(fama_model),lags = 3)
  arch_lag6 <- ArchTest(residuals(fama_model),lags = 6)
  arch_lag12 <- ArchTest(residuals(fama_model),lags = 12)
  #when always p<0,05 -->evidence for GARCH
  
  #return results in list
  return(list(
    model = fama_model,
    summary = summary(fama_model),
    data = data
  ))
}
