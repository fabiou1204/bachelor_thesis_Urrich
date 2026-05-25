#main file
#first rows for connection to all functions of RProject
library(devtools)
library(here)
setwd("/Users/fabiourrich/Library/CloudStorage/OneDrive-Personal/UIP_fx_volatility/Data/BA_Fabio")
load_all()

library(tidyverse)
library(FinTS)
library(lubridate)
library(readxl)
library(ggplot2)
library(dplyr)
library(tseries)#for stationarity test
library(urca)#for stationarity test
library(car)
library(purrr)
library(tseries)
library(moments)


#read exchange rates
setwd("/Users/fabiourrich/Library/CloudStorage/OneDrive-Personal/UIP_fx_volatility/Data/BA_Fabio/01_data/fx_rates")
AUD_fx <- read_excel("fx_AUD.xlsx", sheet=2) %>% select(`TIME_PERIOD:Period`, `OBS_VALUE:Value`) %>% mutate(Date = ymd(`TIME_PERIOD:Period`))
EUR_fx <- read_excel("fx_EUR.xlsx", sheet=2) %>% select(`TIME_PERIOD:Period`, `OBS_VALUE:Value`) %>% mutate(Date = ymd(`TIME_PERIOD:Period`))
GBP_fx <- read_excel("fx_GBP.xlsx", sheet=2) %>% select(`TIME_PERIOD:Period`, `OBS_VALUE:Value`) %>% mutate(Date = ymd(`TIME_PERIOD:Period`))
ZAR_fx <- read_excel("fx_ZAR.xlsx", sheet=2) %>% select(`TIME_PERIOD:Period`, `OBS_VALUE:Value`) %>% mutate(Date = ymd(`TIME_PERIOD:Period`))
INR_fx <- read_excel("fx_INR.xlsx", sheet=2) %>% select(`TIME_PERIOD:Period`, `OBS_VALUE:Value`) %>% mutate(Date = ymd(`TIME_PERIOD:Period`))
IDR_fx <- read_excel("fx_IDR.xlsx", sheet=2) %>% select(`TIME_PERIOD:Period`, `OBS_VALUE:Value`) %>% mutate(Date = ymd(`TIME_PERIOD:Period`))


#reading interest rates
setwd("/Users/fabiourrich/Library/CloudStorage/OneDrive-Personal/UIP_fx_volatility/Data/BA_Fabio/01_data/interest_rate")
US_interest <- read_excel("USDOND.xlsx") %>% mutate(Date = as_date(Date)) %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))
AUD_interest <- read_excel("AUDOND.xlsx") %>% mutate(Date = as_date(Date)) %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))
EUR_interest <- read_excel("EUROND.xlsx") %>% mutate(Date = as_date(Date)) %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))
GBP_interest <- read_excel("GBPOND.xlsx") %>% mutate(Date = as_date(Date)) %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))
ZAR_interest <- read_excel("ZAROND.xlsx") %>% mutate(Date = as_date(Date)) %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))
INR_interest <- read_excel("INROND.xlsx") %>% mutate(Date = as_date(Date)) %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))
IDR_interest <- read_excel("IDROND.xlsx") %>% mutate(Date = as_date(Date)) %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))


#merge interest rates and exchange rates for each country with the USD interest rate; only take dates available in all three dataframes
#inner_join() only keeps rows with matching dates, so each merged dataframe has the three required observations in each line
AUD_merged <- AUD_fx %>% inner_join(AUD_interest, by = "Date") %>% inner_join(US_interest, by = "Date") %>% 
  rename("AUD_fx" = "OBS_VALUE:Value") %>% rename("AUD_int" = "AUDOND= (BID)") %>% rename("US_int" = "USDOND= (BID)") %>% 
  mutate(AUD_int = as.numeric(AUD_int), US_int = as.numeric(US_int)) %>% 
  na.omit()
EUR_merged <- EUR_fx %>% inner_join(EUR_interest, by = "Date") %>% inner_join(US_interest, by = "Date") %>% 
  rename("EUR_fx" = "OBS_VALUE:Value") %>% rename("EUR_int" = "EUROND= (BID)") %>% rename("US_int" = "USDOND= (BID)") %>% 
  mutate(EUR_int = as.numeric(EUR_int), US_int = as.numeric(US_int)) %>%
  na.omit()
GBP_merged <- GBP_fx %>% inner_join(GBP_interest, by = "Date") %>% inner_join(US_interest, by = "Date") %>% 
  rename("GBP_fx" = "OBS_VALUE:Value") %>% rename("GBP_int" = "GBPOND= (BID)") %>% rename("US_int" = "USDOND= (BID)") %>% 
  mutate(GBP_int = as.numeric(GBP_int), US_int = as.numeric(US_int)) %>%
  na.omit()
ZAR_merged <- ZAR_fx %>% inner_join(ZAR_interest, by = "Date") %>% inner_join(US_interest, by = "Date") %>% 
  rename("ZAR_fx" = "OBS_VALUE:Value") %>% rename("ZAR_int" = "ZAROND= (BID)") %>% rename("US_int" = "USDOND= (BID)") %>% 
  mutate(ZAR_int = as.numeric(ZAR_int), US_int = as.numeric(US_int)) %>%
  na.omit()
INR_merged <- INR_fx %>% inner_join(INR_interest, by = "Date") %>% inner_join(US_interest, by = "Date") %>% 
  rename("INR_fx" = "OBS_VALUE:Value") %>% rename("INR_int" = "INROND= (BID)") %>% rename("US_int" = "USDOND= (BID)") %>% 
  mutate(INR_int = as.numeric(INR_int), US_int = as.numeric(US_int)) %>%
  na.omit()
IDR_merged <- IDR_fx %>% inner_join(IDR_interest, by = "Date") %>% inner_join(US_interest, by = "Date") %>% 
  rename("IDR_fx" = "OBS_VALUE:Value") %>% rename("IDR_int" = "IDROND= (BID)") %>% rename("US_int" = "USDOND= (BID)") %>% 
  mutate(IDR_int = as.numeric(IDR_int), US_int = as.numeric(US_int)) %>%
  na.omit()



#Arch test for Fama regression residuals
AUD_arch <- Archtest(AUD_merged, AUD_fx, AUD_int, US_int)
print(AUD_arch)
EUR_arch <- Archtest(EUR_merged, EUR_fx, EUR_int, US_int)
print(EUR_arch)
GBP_arch <- Archtest(GBP_merged, GBP_fx, GBP_int, US_int)
print(GBP_arch)
ZAR_arch <- Archtest(ZAR_merged, ZAR_fx, ZAR_int, US_int)
print(ZAR_arch)
INR_arch <- Archtest(INR_merged, INR_fx, INR_int, US_int)
print(INR_arch)
IDR_arch <- Archtest(IDR_merged, IDR_fx, IDR_int, US_int)
print(IDR_arch)


#Ljung Box test for Fama residuals and squared residuals
ljung_box_fama_aud  <- ljung_box(residuals(AUD_fama$model))
print(ljung_box_fama_aud)
ljung_box_fama_eur  <- ljung_box(residuals(EUR_fama$model))
print(ljung_box_fama_eur)
ljung_box_fama_gbp  <- ljung_box(residuals(GBP_fama$model))
print(ljung_box_fama_gbp)
ljung_box_fama_zar  <- ljung_box(residuals(ZAR_fama$model))
print(ljung_box_fama_zar)
ljung_box_fama_inr  <- ljung_box(residuals(INR_fama$model))
print(ljung_box_fama_inr)
ljung_box_fama_idr  <- ljung_box(residuals(IDR_fama$model))
print(ljung_box_fama_idr)


#calculate Fama regerssion from 2006-2025 as benchmark
AUD_fama <- Fama(AUD_merged, AUD_fx, AUD_int, US_int)
print(AUD_fama)
#check whether interest rates and fx log returns have the same scale
mean(AUD_fama$data$log_returns)
mean(AUD_fama$data$interest_rate_differential_daily)

EUR_fama <- Fama(EUR_merged, EUR_fx, EUR_int, US_int)
print(EUR_fama)
GBP_fama <- Fama(GBP_merged, GBP_fx, GBP_int, US_int)
print(GBP_fama)
ZAR_fama <- Fama(ZAR_merged, ZAR_fx, ZAR_int, US_int)
print(ZAR_fama)
INR_fama <- Fama(INR_merged, INR_fx, INR_int, US_int)
print(INR_fama)
IDR_fama <- Fama(IDR_merged, IDR_fx, IDR_int, US_int)
print(IDR_fama)


#plot fx time series data to check for drifts and trends
AUD_plot_stationarity <- plot_stationarity_1(AUD_merged, Date, AUD_fx)
AUD_plot_stationarity
EUR_plot_stationarity <- plot_stationarity_1(EUR_merged, Date, EUR_fx)
EUR_plot_stationarity
GBP_plot_stationarity <- plot_stationarity_1(GBP_merged, Date, GBP_fx)
GBP_plot_stationarity
ZAR_plot_stationarity <- plot_stationarity_1(ZAR_merged, Date, ZAR_fx)
ZAR_plot_stationarity
INR_plot_stationarity <- plot_stationarity_1(INR_merged, Date, INR_fx)
INR_plot_stationarity
IDR_plot_stationarity <- plot_stationarity_1(IDR_merged, Date, IDR_fx)
IDR_plot_stationarity

#plot of interest rate data to check for drifts and trends
AUD_plot_stationarity_int <- plot_stationarity_1(AUD_merged, Date, AUD_int)
AUD_plot_stationarity_int
EUR_plot_stationarity_int <- plot_stationarity_1(EUR_merged, Date, EUR_int)
EUR_plot_stationarity_int
GBP_plot_stationarity_int <- plot_stationarity_1(GBP_merged, Date, GBP_int)
GBP_plot_stationarity_int
ZAR_plot_stationarity_int <- plot_stationarity_1(ZAR_merged, Date, ZAR_int)
ZAR_plot_stationarity_int
INR_plot_stationarity_int <- plot_stationarity_1(INR_merged, Date, INR_int)
INR_plot_stationarity_int
IDR_plot_stationarity_int <- plot_stationarity_1(IDR_merged, Date, IDR_int)
IDR_plot_stationarity_int

#check for stationarity of fx rates
AUD_stationarity_fx <- stationrity_1(AUD_merged, "AUD_fx")
print(AUD_stationarity_fx)
EUR_stationarity_fx <- stationrity_1(EUR_merged, "EUR_fx")
print(EUR_stationarity_fx)
GBP_stationarity_fx <- stationrity_1(GBP_merged, "GBP_fx")
print(GBP_stationarity_fx)
ZAR_stationarity_fx <- stationrity_1(ZAR_merged, "ZAR_fx")
print(ZAR_stationarity_fx)
INR_stationarity_fx <- stationrity_1(INR_merged, "INR_fx")
print(INR_stationarity_fx)
IDR_stationarity_fx <- stationrity_1(IDR_merged, "IDR_fx")
print(IDR_stationarity_fx)

#check for stationarity of interest rates
AUD_stationarity_int <- stationrity_1(AUD_merged, "AUD_int")
print(AUD_stationarity_int)
EUR_stationarity_int <- stationrity_1(EUR_merged, "EUR_int")
print(EUR_stationarity_int)
GBP_stationarity_int <- stationrity_1(GBP_merged, "GBP_int")
print(GBP_stationarity_int)
ZAR_stationarity_int <- stationrity_1(ZAR_merged, "ZAR_int")
print(ZAR_stationarity_int)
INR_stationarity_int <- stationrity_1(INR_merged, "INR_int")
print(INR_stationarity_int)
IDR_stationarity_int <- stationrity_1(IDR_merged, "IDR_int")
print(IDR_stationarity_int)

#check stationarity on log fx returns
AUD_stationarity_fx_log <- stationarity_fx(AUD_merged, AUD_fx)
print(AUD_stationarity_fx_log)
EUR_stationarity_fx_log <- stationarity_fx(EUR_merged, EUR_fx)
print(EUR_stationarity_fx_log)
GBP_stationarity_fx_log <- stationarity_fx(GBP_merged, GBP_fx)
print(GBP_stationarity_fx_log)
ZAR_stationarity_fx_log <- stationarity_fx(ZAR_merged, ZAR_fx)
print(ZAR_stationarity_fx_log)
INR_stationarity_fx_log <- stationarity_fx(INR_merged, INR_fx)
print(INR_stationarity_fx_log)
IDR_stationarity_fx_log <- stationarity_fx(IDR_merged, IDR_fx)
print(IDR_stationarity_fx_log)


#check stationarity on daily interest rate differentials
AUD_stationarity_int_diff <- stationarity_int(AUD_merged, AUD_int, US_int)
print(AUD_stationarity_int_diff)
EUR_stationarity_int_diff <- stationarity_int(EUR_merged, EUR_int, US_int)
print(EUR_stationarity_int_diff)
GBP_stationarity_int_diff <- stationarity_int(GBP_merged, GBP_int, US_int)
print(GBP_stationarity_int_diff)
ZAR_stationarity_int_diff <- stationarity_int(ZAR_merged, ZAR_int, US_int)
print(ZAR_stationarity_int_diff)
INR_stationarity_int_diff <- stationarity_int(INR_merged, INR_int, US_int)
print(INR_stationarity_int_diff)
IDR_stationarity_int_diff <- stationarity_int(IDR_merged, IDR_int, US_int)
print(IDR_stationarity_int_diff)


#Jarque Bera test to check for normality in log returns
AUD_jb <- jb_test(AUD_merged, AUD_fx)
print(AUD_jb)
EUR_jb <- jb_test(EUR_merged, EUR_fx)
print(EUR_jb)
GBP_jb <- jb_test(GBP_merged, GBP_fx)
print(GBP_jb)
ZAR_jb <- jb_test(ZAR_merged, ZAR_fx)
print(ZAR_jb)
INR_jb <- jb_test(INR_merged, INR_fx)
print(INR_jb)
IDR_jb <- jb_test(IDR_merged, IDR_fx)
print(IDR_jb)











########################### presentation to Ivan 
EUR_arch <- Archtest(EUR_merged, EUR_fx, EUR_int, US_int)
EUR_arch$summary$coefficients
EUR_fama <- Fama(EUR_merged, EUR_fx, EUR_int, US_int)
EUR_fama$summary$coefficients
EUR_fama$t_value

UK_fama <- Fama(GBP_merged, GBP_fx, GBP_int, US_int)
UK_fama$summary$coefficients
UK_fama$t_value




