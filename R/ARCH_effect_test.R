#test for ARCH effects

#function for FAMA regression and ARCH test
Archtest <- function(data, fx, home_int, US_int){


  #calculate log returns of exchange rate
  data <- data %>%
    mutate(log_returns = (log(lead({{fx}}, 1)) - log({{fx}}))*100) 
  #multiply by 100 to have log returns in percentage points, otherwise values are very small and hard to interpret ->better interpretation of beta

  #calculate interest rate differential
  #assuming US as foreign country
  data <- data %>% mutate(
    days_gap = as.numeric(difftime(lead(Date, 1), Date, units = "days")),
    interest_rate_differential = {{home_int}} - {{US_int}},
    #????consider leap years: divide by 366 on each day that is at least one year ahead of the 29 february of the leap year
    #although rates are annualised, so dividing by 365 should be correct
    #for simplicity I just divide by 365 as often done in finance
    interest_rate_differential_daily =interest_rate_differential*days_gap/365) %>% 
    na.omit()#because lead,1 in fx log returns creates ony NA at the end of the dataframe
  
    

  #basic OLS Fama regression
  fama_model <- lm(log_returns ~ interest_rate_differential_daily, data = data)
  
  t_value <- linearHypothesis(fama_model, c("interest_rate_differential_daily = 1"))
  
  #again, calculate t statistic and p value
  beta_hat <- coef(fama_model)["interest_rate_differential_daily"]
  se_beta  <- summary(fama_model)$coefficients["interest_rate_differential_daily", "Std. Error"]
  t_stat_beta1 <- (beta_hat - 1) / se_beta
  p_val_beta1  <- 2 * pt(-abs(t_stat_beta1), df = fama_model$df.residual)
  
  
  arch_lag5 <- ArchTest(residuals(fama_model),lags = 5)
  arch_lag10 <- ArchTest(residuals(fama_model),lags = 10)
  arch_lag20 <- ArchTest(residuals(fama_model),lags = 20)
  #when always p<0,05 -->evidence for GARCH
  
  #return results in list
  return(list(
    #model = fama_model,
    #summary = summary(fama_model),
    coef = summary(fama_model)$coefficients,
    arch5 = arch_lag5,
    arch10 = arch_lag10,
    arch20 = arch_lag20,
    #data = data,
    p_value = p_val_beta1
  ))
}

