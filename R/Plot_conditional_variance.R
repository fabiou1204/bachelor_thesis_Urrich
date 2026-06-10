#plot of the conditional variance estiamte

plot_cond_vol <- function(data, fit, title = NULL){
  #use merged data for data argument for Date variable and fitted model for sigma estimate
  
  plot_data <- data[1:nrow(data)-1, ]#remove last row since sigma is NA for last observation
  
  plot_data$sigma <- as.numeric(sigma(fit)) 
  
  ggplot(plot_data, aes(x = Date, y = sigma)) +
    geom_line(colour = "#1f6f8b", linewidth = 0.3) +
    scale_x_date(date_labels = "%Y") +
    labs(title = title, x = NULL, y = "Daily conditional SD (%)") +
    theme_minimal(base_size = 11)
}