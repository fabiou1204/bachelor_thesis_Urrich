#Jarque Bera test to test log returns for normality


jb_test <- function(data, fx){
  data <- data %>% mutate(log_returns = (log(dplyr::lead({{fx}}, 1)) - log({{fx}}))*100) %>% 
    na.omit()#calculate log returns as done before for Fama()
  
  returns <- data$log_returns
  jb <- jarque.bera.test(returns)#jarque bera test
  #null hypothesis that kurtosis is 3 and skewness is 0
  
  #JB puts both of these info into one statistic; but knowing values important for correct distribution choice
  skew <- moments::skewness(returns)
  kurt <- moments::kurtosis(returns)
  
  return(list(
    jb_stat = jb$statistic,
    p_value = jb$p.value,
    skewness = skew,
    kurtosis = kurt#note for interpretation: 3 is normaldist; >3 is leptokurtic
  ))
}