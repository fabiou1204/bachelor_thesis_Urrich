#summary statistics

summary_stats <- function(merged_data, fx, home_int, US_int){
  #calculate log returns of exchange rate
  merged_data <- merged_data %>% 
    mutate(log_returns = log(dplyr::lead({{fx}}, 1)) - log({{fx}})) %>% 
    mutate(log_returns = log_returns*100)

  #calculate interest rate differential
  merged_data <- merged_data %>% mutate(
    days_gap = as.numeric(difftime(dplyr::lead(Date, 1), Date, units = "days")),
    interest_rate_differential = {{home_int}} - {{US_int}},
    interest_rate_differential_daily =interest_rate_differential*days_gap/365) %>% 
    na.omit()
  
  #log returns and interest rate differentials
  log_return <- merged_data$log_returns
  interest_rate_diff <- merged_data$interest_rate_differential_daily
  
  return(list(
    n = length(log_return),
    #log returns
    returns_mean = mean(log_return),
    returns_sd = sd(log_return),
    returns_min = min(log_return),
    returns_25th = quantile(log_return, 0.25),
    returns_median = median(log_return),
    returns_75th = quantile(log_return, 0.75),
    returns_max = max(log_return),
    #interest rate differentials
    interest_mean = mean(interest_rate_diff),
    interest_sd = sd(interest_rate_diff),
    interest_min = min(interest_rate_diff),
    interest_25th = quantile(interest_rate_diff, 0.25),
    interest_median = median(interest_rate_diff),
    interest_75th = quantile(interest_rate_diff, 0.75),
    interest_max = max(interest_rate_diff)
  ))
  

  
}