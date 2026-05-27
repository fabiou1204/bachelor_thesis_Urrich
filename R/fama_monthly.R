#fama regression monthly for double-check

fama_monthly <- function(data, fx, home_int, USD_int){
  
  #data <- data %>% filter(Date >= as.Date("2006-01-01"))
  #not needed anymore because use of entire sample from 2000 to 2025 without burn-in
  
  data <- data %>% mutate(
  #calculate log returns of exchange rate
  log_returns = (log(lead({{fx}}, 3)) - log({{fx}}))*100,
  
  #calculate interest differential
  interest_rate_differential = {{home_int}} - {{USD_int}},
  
  #calculate monthly interest rate differential
  interest_rate_differential_monthly = interest_rate_differential/4,
  
  #calculate month gap
  # calculates how many months lie between two observations 
  #months(1) transforms interval in month format
  month_gap = interval(Date, lead(Date, 3)) %/% months(1)
  ) %>% 
  
  
  #create data frame for regression
  #drop rows with NA from lead() and the month where USD has no interest rate data
  filter(!is.na(log_returns),
         !is.na(interest_rate_differential_monthly),
         month_gap == 3)
  #I checked: exactly one month is deleted via this procedure

  #basic OLS Fama regression
  fama_model <- lm(log_returns ~ interest_rate_differential_monthly, data = data)
  
  beta_test <- linearHypothesis(fama_model, c("interest_rate_differential_monthly = 1"))
  #linearHypothesis calculates a f statistic. However, p value is the same, so it is fine for this quick double check
  
  return(list(
    data = data,
    coef = summary(fama_model)$coefficients,
    beta_test = beta_test
    ))
}