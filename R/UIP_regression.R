#fama regression from 01-01-2006 t0 31-12-2025 as benchmark for 
Fama <- function(data, fx, home_int, US_int){
  
  #calculate log returns of exchange rate
  data <- data %>% 
    mutate(log_returns = log(dplyr::lead({{fx}}, 1)) - log({{fx}}))
  data <- data %>% 
    mutate(log_returns = log_returns*100)
  #multiply by 100 to have log returns in percentage points, otherwise values are very small and hard to interpret ->better interpretation of beta
  #both fx log returns and interest rate differentials are in percentages; important to have same scale
  
  #calculate interest rate differential
  #assuming US as foreign country
  data <- data %>% mutate(
    days_gap = as.numeric(difftime(dplyr::lead(Date, 1), Date, units = "days")),
    interest_rate_differential = {{home_int}} - {{US_int}},
    #for simplicity I just divide by 365 as often done in finance
    interest_rate_differential_daily =interest_rate_differential*days_gap/365) %>% 
    #interest_rate_differential_daily =interest_rate_differential/365) %>% 
    na.omit()#because lead,1 in fx log returns creates ony NA at the end of the dataframe
  
  
  
  #basic OLS Fama regression
  fama_model <- lm(log_returns ~ interest_rate_differential_daily, data = data)
  
  #use of Newey-West (HAC) standard errors
  #lag selected by computing ACF and choosing first lag that is no longer statistically significant 
  res     <- residuals(fama_model)
  n_obs   <- length(res)
  acf_res <- as.numeric(acf(res, lag.max = 15, plot = TRUE)$acf)[-1]#drop lag 0 via -1
  conf    <- 1.96 / sqrt(n_obs)
  insig   <- which(abs(acf_res) < conf)#lags lying inside the band
  nw_lag  <- if (length(insig) > 0) insig[1] else 15#first insignificant lag (otherwise: 15)
  
  #Newey-West covariance at the selected lag
  vcov_nw <- sandwich::NeweyWest(fama_model, lag = nw_lag, prewhite = FALSE, adjust = TRUE)#prewhite FALSE so that caluclation is performed with chose lag
  coef_nw <- lmtest::coeftest(fama_model, vcov. = vcov_nw)
  
  t_value_nw <- linearHypothesis(fama_model, c("interest_rate_differential_daily = 1"), vcov. = vcov_nw)
  #linearHypothesis gives values for F statistic
  #double check with manual calculation
  #calculate t statistic and pvalue
  beta_hat <- coef(fama_model)["interest_rate_differential_daily"]
  se_beta_nw  <- sqrt(vcov_nw["interest_rate_differential_daily", "interest_rate_differential_daily"])#extract standard errors
  t_stat_beta1_nw <- (beta_hat - 1) / se_beta_nw
  p_val_beta1_nw  <- 2 * pt(-abs(t_stat_beta1_nw), df = fama_model$df.residual)
  
  #return results in list
  return(list(
    model = fama_model,
    summary = summary(fama_model),
    data = data,
    coef = summary(fama_model)$coefficients,
    p_value_nw = p_val_beta1_nw,
    vcov_nw = vcov_nw,
    coef_nw = coef_nw,
    nw_lag = nw_lag
  ))
}