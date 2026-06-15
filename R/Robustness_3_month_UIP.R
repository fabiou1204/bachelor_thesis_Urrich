#fama regression monthly for double-check

fama_monthly <- function(data, fx, home_int, USD_int){
  
  #data <- data %>% filter(Date >= as.Date("2006-01-01"))
  #not needed anymore because use of entire sample from 2000 to 2025 without burn-in
  
  data <- data %>% mutate(
  #calculate log returns of exchange rate
  log_returns = (log(dplyr::lead({{fx}}, 3)) - log({{fx}}))*100,
  
  #calculate interest differential
  interest_rate_differential = {{home_int}} - {{USD_int}},
  
  #calculate monthly interest rate differential
  interest_rate_differential_monthly = interest_rate_differential/4,
  
  #calculate month gap
  # calculates how many months lie between two observations 
  #months(1) transforms interval in month format
  month_gap = interval(Date, dplyr::lead(Date, 3)) %/% months(1)
  ) %>% 
  
  
  #create data frame for regression
  #drop rows with NA from lead() and the month where USD has no interest rate data
  filter(!is.na(log_returns),
         !is.na(interest_rate_differential_monthly),
         month_gap == 3)
  #I checked: exactly one month is deleted via this procedure

  #basic OLS Fama regression
  fama_model <- lm(log_returns ~ interest_rate_differential_monthly, data = data)
  
  #use of Newey West standard errors due to autocorrelation in error terms
  nw_vcov <- NeweyWest(fama_model, lag = 2, prewhite = FALSE, adjust =TRUE)#lag 2 because error term follows MA(2) structure
  nw_coef <- coeftest(fama_model, vcov = nw_vcov)
  
  #ACF confirmation of the MA(2) structure
  #does not change lag=2 above, just confirmation
  res              <- residuals(fama_model)
  n_obs            <- length(res)
  acf_res          <- as.numeric(acf(res, lag.max = 10, plot = FALSE)$acf)[-1]#drop lag 0 via -1
  conf             <- 1.96 / sqrt(n_obs)
  acf_diag         <- data.frame(
    lag         = seq_along(acf_res),
    autocorr    = acf_res,
    significant = abs(acf_res) > conf
  )
  insig            <- which(abs(acf_res) < conf)
  acf_selected_lag <- if (length(insig) > 0) insig[1] else NA_integer_
  
  
  
  #linearHypothesis calculates a f statistic. However, p value is the same, so it is fine for this quick double check
  beta_test <- linearHypothesis(fama_model, 
                                c("interest_rate_differential_monthly = 1"),
                                vcov. = nw_vcov)
  
  

                                
  return(list(
    #data = data,
    #coef = summary(fama_model)$coefficients, #original OLS coefs
    nw_coef = nw_coef, #newey west adjusted summary
    beta_test = beta_test,
    adf_selected_lag = acf_selected_lag,
    acf_diag = acf_diag
    ))
}