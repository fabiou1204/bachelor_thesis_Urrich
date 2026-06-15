#plots for stationarity analysis

plot_stationarity_1 <- function(data, date, series, type){
 #plot the time series against the date
  series_name <- as_label(enquo(series)) #extract the series' name
  p <- ggplot(data, aes(x={{date}}, y={{series}} ))+
                          geom_line (color = "blue", linewidth = 0.2) +
                          theme_minimal() +
                          labs(title = paste("Time Series", series_name),
                               x = "Date",
                               y = type)
                          return(p)
}