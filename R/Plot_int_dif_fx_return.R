#plot fx log return and interest rate differential over time

plot_int_dif_fx_return <- function(data, fx, home_int, US_int){
  #same as in UIP_regression.R
  #calculate log returns of exchange rate
  data <- data %>% 
    mutate(log_returns = log(dplyr::lead({{fx}}, 1)) - log({{fx}})) %>% 
    mutate(log_returns = log_returns*100)

  #calculate interest rate differential
  data <- data %>% mutate(
    days_gap = as.numeric(difftime(dplyr::lead(Date, 1), Date, units = "days")),
    interest_rate_differential = {{home_int}} - {{US_int}}) %>% 
    #use annual interest rate differential for interpretation
    na.omit()
  
  #use of two scales for better visualization
  returns_range <- range(data$log_returns, na.rm = TRUE)#range of log returns
  int_diff_range <- range(data$interest_rate_differential, na.rm = TRUE)#range of interest rate differential
  #scale interest rate differential to log returns so that both can be plotted on the same graph
  scale <- diff(returns_range) / diff(int_diff_range)
  #move minimums to the same point 
  move <- returns_range[1] - int_diff_range[1] * scale 
  
  #plot log returns and interest rate differential over time
  p <- ggplot(data, aes(x = Date)) +
    geom_line(aes(y = log_returns),
              colour = "grey35", linewidth = 0.05, na.rm = TRUE) +
    geom_line(aes(y = move + scale*interest_rate_differential),
              colour = "#c0392b", linewidth = 0.05, na.rm = TRUE) +
    scale_y_continuous(
      name     = "FX log return (%)",
      sec.axis = sec_axis(~ (. - move) / scale, name = "Interest rate differential (% p.a.)")
    ) +
    labs(x = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      axis.title.y.left  = element_text(colour = "grey35"),
      axis.text.y.left   = element_text(colour = "grey35"),
      axis.title.y.right = element_text(colour = "#c0392b"),
      axis.text.y.right  = element_text(colour = "#c0392b")
    )
  
  return(list(
    plot = p
  ))
}


