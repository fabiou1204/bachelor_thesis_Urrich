#main file
#first rows for connection to all functions of RProject
library(devtools)
library(here)
setwd("/Users/fabiourrich/Library/CloudStorage/OneDrive-Personal/UIP_fx_volatility/Data/BA_Fabio")
load_all()

library(tidyverse)
library(FinTS)
library(readxl)
library(dplyr)

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
AUD_check <- AUD_arch$data %>% select("days_gap", "log_returns")
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


#calculate Fama regerssion from 2006-2025 as benchmark
AUD_fama <- Fama(AUD_merged, AUD_fx, AUD_int, US_int)
print(AUD_fama)
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
