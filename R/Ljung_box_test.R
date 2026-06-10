#Ljung Box test
#test for autocorrelation in 

ljung_box <- function(resid, lags = c(5, 10, 20), df = 0){
  #df 0 for OLS resid, needs to be overridden for GARCH
 
  results <- map_dfr(lags, function(h){#map_dfr applies the function to each elemtnt of lags
  normal_resid <- Box.test(resid, lag = h, type = "Ljung-Box", fitdf = 0)
  #fitdf = 0 because no mean equation has AR or MA terms (so m in Q(h) formula 0)

  squared_resid <- Box.test(resid^2, lag=h, type="Ljung-Box", fitdf = df)
  #this is the real test for volatility clustering; used as robustness to ARCH-LM test performed separately
  
  tibble( #use tibble instead of normal dataframe because of better formatting
    series = c("residuals (level)", "squared residuals"),
    fitdf = c(0, df),
    statistic = c(as.numeric(normal_resid$statistic), as.numeric(squared_resid$statistic)), #returns Q statistic
    df = c(h, h - df), #effective dfs
    p_value = c(as.numeric(normal_resid$p.value), as.numeric(squared_resid$p.value))
  )
  })
  
  return(list(
    lag = lags,
    results = results))
  }





#how to call in main
#lb_fama_aud  <- ljung_box(residuals(AUD_fama$model))                          
#lb_garch_aud <- ljung_box(as.numeric(residuals(AUD_garch$garch_model, standardize = TRUE)),
#                         df = 2)