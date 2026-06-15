#comparison of beta coefficients between high- and middle-income country currencies via pooled OLS
#make wald test on interaction term
#use of Driscoll-Kraay standard errors to account for cross-sectional dependence

compare_betas <- function(high_income_results, mid_income_results) {
  
#extract data correctly for high and middle income countries
  high_income_data <- bind_rows(lapply(names(high_income_results), function(currency) {
    #function applied to each currency and extracts Date, log_returns, and interest_rate_differential_daily via $data
    high_income_results[[currency]]$data %>%
      select(Date, log_returns, interest_rate_differential_daily) %>%
      mutate(Dummy_middle = 0L, currency = currency)#every high-income currency gets Dummy_middle =0
  }))
  
  mid_income_data <- bind_rows(lapply(names(mid_income_results), function(currency) {
    mid_income_results[[currency]]$data %>%
      select(Date, log_returns, interest_rate_differential_daily) %>%
      mutate(Dummy_middle = 1L, currency = currency)#every middle-income currency gets Dummy_middle=1
  }))
  
  pooled_data <- bind_rows(high_income_data, mid_income_data)
  #build interaction column
  pooled_data$interaction <- pooled_data$Dummy_middle * pooled_data$interest_rate_differential_daily
  
  #regression with currency fixed effects and interaction term that distinguishes between high and middle income countries
  pdata <- plm::pdata.frame(pooled_data, index = c("currency", "Date"))#data frame for panel data; declares currency has individual dimension and Date as time dimension
  
  fe_model <- plm::plm(
    log_returns ~ interest_rate_differential_daily +
      interaction,
    #no standalone Dummy_middle included because perfectly collinear with currency fixed effects
    data = pdata,
    model = "within",#fixed effects: subtracts each currency's mean
    effect = "individual"#fixed effects along the individual dimension, i.e. currency
  )
  
  #use of driscoll-kraay covariance matrix
  vcov_driscoll_kraay <- plm::vcovSCC(fe_model, type = "HC0")#automatic max lag selection
  
  #robust coefficients and wald test using driscoll-kraay standard errors
  robust_coeftest <- lmtest::coeftest(fe_model, vcov = vcov_driscoll_kraay)#coefficients with robust SEs

  wald <- car::linearHypothesis(#wald test with robust SEs
    fe_model,
    c("interaction = 0"),
    vcov = vcov_driscoll_kraay
  )
  
  beta_high   <- coef(fe_model)["interest_rate_differential_daily"]
  beta_middle <- beta_high + coef(fe_model)["interaction"]
  
  return(list(
    model       = fe_model,
    pooled_data = pooled_data,
    robust_coeftest = robust_coeftest,
    wald        = wald,
    beta_high   = unname(beta_high),
    beta_middle = unname(beta_middle)
  ))
}
