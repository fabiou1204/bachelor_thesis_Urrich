#comparison of beta coefficients between high- and middle-income country currencies via pooled OLS
#make wald test on interaction term

compare_betas <- function(high_income_results, mid_income_results) {
  
#extract data correctly for high and middle income countries
  high_income_data <- bind_rows(lapply(names(high_income_results), function(nm) {
    high_income_results[[nm]]$data %>%
      select(Date, log_returns, interest_rate_differential_daily) %>%
      mutate(Dummy_middle = 0L, currency = nm)
  }))
  
  mid_income_data <- bind_rows(lapply(names(mid_income_results), function(nm) {
    mid_income_results[[nm]]$data %>%
      select(Date, log_returns, interest_rate_differential_daily) %>%
      mutate(Dummy_middle = 1L, currency = nm)
  }))
  
  pooled_data <- bind_rows(high_income_data, mid_income_data)
  
  #regression with country fixed effects and interaction term that distinguishes between high and middle income countries
  fe_model <- lm(
    log_returns ~ interest_rate_differential_daily +
      Dummy_middle:interest_rate_differential_daily + as.factor(currency),
    data = pooled_data
  )
  
  #use of robust cov matrix
  vcov_clustered <- vcovCL(fe_model, cluster = ~ currency)
  
  #wald test using robust standard errors
  wald <- linearHypothesis(
    fe_model, 
    "interest_rate_differential_daily:Dummy_middle = 0",
    vcov = vcov_clustered
  )
  
  beta_high   <- coef(fe_model)["interest_rate_differential_daily"]
  beta_middle <- beta_high + coef(fe_model)["interest_rate_differential_daily:Dummy_middle"]
  
  return(list(
    model       = fe_model,
    robust_coeftest = coeftest(fe_model, vcov = vcov_clustered),
    wald        = wald,
    beta_high   = unname(beta_high),
    beta_middle = unname(beta_middle)
  ))
}
