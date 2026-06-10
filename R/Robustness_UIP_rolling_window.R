#robustness check: rolling windows UIP regression
#create five year rolling windows with monthly shifts
#extract average, min, max beta
#create figure with rolling betas and confidence intervals


rob_roll_window <- function(data, fx, home_int, US_int){
  
  #calculate log returns of exchange rate
  #same as done before in UIP_regression.R
  data <- data %>% 
    mutate(
      log_returns = (log(lead({{fx}}, 1)) - log({{fx}})) * 100,
      days_gap = as.numeric(difftime(lead(Date, 1), Date, units = "days")),
      interest_rate_differential = {{home_int}} - {{US_int}},
      interest_rate_differential_daily = interest_rate_differential * days_gap / 365
    ) %>% 
    na.omit()
  
  #assign every observation to a year-month combination; needed for monthly shifts
  data <- data %>% 
    mutate(YearMonth = lubridate::floor_date(Date, "month"))
  
  #get unique year month combinations
  unique_year_month <- sort(unique(data$YearMonth))
  total_months <- length(unique_year_month)
  
  #create empty list to store results from for loop
  result_list <- list()
  
  #loop through all unique month combinations
  #start at 60 until total months is reached
  for (i in 60:total_months){
    #define start and end month
    start_month <- unique_year_month[i - 59]
    end_month <- unique_year_month[i]
    #filter data for current window
    window_data <- data %>% 
      filter(YearMonth >= start_month & YearMonth <= end_month)
    #run UIP baseline reg
    model <- lm(log_returns ~ interest_rate_differential_daily, data = window_data)
    #calculate 95% confidence interval
    conf_int_beta <- confint(model, "interest_rate_differential_daily", level = 0.95)
    #save results in list
    result_list[[i - 59]] <- list(
      start_month = start_month,
      end_month = end_month,
      beta = coef(model)["interest_rate_differential_daily"],
      conf_int_lower = conf_int_beta[1],
      conf_int_upper = conf_int_beta[2]
    )
  }
  
  #store results in single dataframe
  rolling_results <- bind_rows(result_list)
  
  #extract average min and max beta
  avg_beta <- mean(rolling_results$beta, na.rm = TRUE)
  min_beta <- min(rolling_results$beta, na.rm = TRUE)
  max_beta <- max(rolling_results$beta, na.rm = TRUE)

  
  #create figure with window betas and corresponding confidence  intervals
  #extract currency name
  fx_name <- rlang::as_label(rlang::enquo(fx))
  
  roll_window_plot <- ggplot(rolling_results, aes(x = end_month, y = beta)) +
    geom_ribbon(aes(ymin = conf_int_lower, ymax = conf_int_upper), fill = "blue", alpha = 0.15) +
    geom_line(color = "blue", linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      #title = paste0("5-Year Rolling UIP Betas (Monthly Shifts): ",  fx_name),
      #subtitle = "Shaded area represents 95% Confidence Intervals",
      #all six currencies in one figure in latex document, so no figure needed in the end, because description there
      x = "Window End Date",
      y = "Beta Coefficient"
    ) +
    theme_minimal()
  
  return(list(
    rolling_results = rolling_results,
    avg_beta = avg_beta,
    min_beta = min_beta,
    max_beta = max_beta,
    roll_window_plot = roll_window_plot
  ))
  
  
}
  