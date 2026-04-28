#main for monthly data that are used for double check
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
library(car)


#read fx rates
setwd("/Users/fabiourrich/Library/CloudStorage/OneDrive-Personal/UIP_fx_volatility/Data/BA_Fabio/01_data/monthly/fx_rates")
AUD_monthly_fx <- read.csv("AUD_monthly.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
EUR_monthly_fx <- read.csv("EUR_monthly.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
GBP_monthly_fx <- read.csv("GBP_monthly.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
ZAR_monthly_fx <- read.csv("ZAR_monthly.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
INR_monthly_fx <- read.csv("INR_monthly.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
IDR_monthly_fx <- read.csv("IDR_monthly.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)

#read interest rates
setwd("/Users/fabiourrich/Library/CloudStorage/OneDrive-Personal/UIP_fx_volatility/Data/BA_Fabio/01_data/monthly/interest_rates")
AUD_monthly_int <- read.csv("AUD_int_mon.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
EUR_monthly_int <- read.csv("EUR_int_mon.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
GBP_monthly_int <- read.csv("GBP_int_mon.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
ZAR_monthly_int <- read.csv("ZAR_int_mon.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
INR_monthly_int <- read.csv("INR_int_mon.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
IDR_monthly_int <- read.csv("IDR_int_mon.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)
US_monthly_int <- read.csv("USD_int_mon.csv") %>% select(TIME_PERIOD, OBS_VALUE) %>% mutate(Date = ym(TIME_PERIOD)) %>% arrange(Date)

#merge data sets
AUD_mon_merge <- AUD_monthly_fx %>% inner_join(AUD_monthly_int, by = "Date") %>% inner_join(US_monthly_int, by = "Date") %>% 
  rename("AUD_fx" = "OBS_VALUE.x") %>% rename("AUD_int" = "OBS_VALUE.y") %>% rename("US_int" = "OBS_VALUE") %>% 
  mutate(AUD_int = as.numeric(AUD_int), US_int = as.numeric(US_int)) %>% 
  na.omit()
EUR_mon_merge <- EUR_monthly_fx %>% inner_join(EUR_monthly_int, by = "Date") %>% inner_join(US_monthly_int, by = "Date") %>%
  rename("EUR_fx" = "OBS_VALUE.x") %>% rename("EUR_int" = "OBS_VALUE.y") %>% rename("US_int" = "OBS_VALUE") %>% 
  mutate(EUR_int = as.numeric(EUR_int), US_int = as.numeric(US_int)) %>%
  na.omit()
GBP_mon_merge <- GBP_monthly_fx %>% inner_join(GBP_monthly_int, by = "Date") %>% inner_join(US_monthly_int, by = "Date") %>% 
  rename("GBP_fx" = "OBS_VALUE.x") %>% rename("GBP_int" = "OBS_VALUE.y") %>% rename("US_int" = "OBS_VALUE") %>% 
  mutate(GBP_int = as.numeric(GBP_int), US_int = as.numeric(US_int)) %>%
  na.omit()
ZAR_mon_merge <- ZAR_monthly_fx %>% inner_join(ZAR_monthly_int, by = "Date") %>% inner_join(US_monthly_int, by = "Date") %>% 
  rename("ZAR_fx" = "OBS_VALUE.x") %>% rename("ZAR_int" = "OBS_VALUE.y") %>% rename("US_int" = "OBS_VALUE") %>% 
  mutate(ZAR_int = as.numeric(ZAR_int), US_int = as.numeric(US_int)) %>%
  na.omit()
INR_mon_merge <- INR_monthly_fx %>% inner_join(INR_monthly_int, by = "Date") %>% inner_join(US_monthly_int, by = "Date") %>% 
  rename("INR_fx" = "OBS_VALUE.x") %>% rename("INR_int" = "OBS_VALUE.y") %>% rename("US_int" = "OBS_VALUE") %>%
  mutate(INR_int = as.numeric(INR_int), US_int = as.numeric(US_int)) %>%
  na.omit()
IDR_mon_merge <- IDR_monthly_fx %>% inner_join(IDR_monthly_int, by = "Date") %>% inner_join(US_monthly_int, by = "Date") %>% 
  rename("IDR_fx" = "OBS_VALUE.x") %>% rename("IDR_int" = "OBS_VALUE.y") %>% rename("US_int" = "OBS_VALUE") %>% 
  mutate(IDR_int = as.numeric(IDR_int), US_int = as.numeric(US_int)) %>%
  na.omit()



#call monthly fama function
AUD_monthly_fama <- fama_monthly(AUD_mon_merge, AUD_fx, AUD_int, US_int)
AUD_monthly_fama
EUR_monthly_fama <- fama_monthly(EUR_mon_merge, EUR_fx, EUR_int, US_int)
EUR_monthly_fama
GBP_monthly_fama <- fama_monthly(GBP_mon_merge, GBP_fx, GBP_int, US_int)
GBP_monthly_fama
ZAR_monthly_fama <- fama_monthly(ZAR_mon_merge, ZAR_fx, ZAR_int, US_int)
ZAR_monthly_fama
INR_monthly_fama <- fama_monthly(INR_mon_merge, INR_fx, INR_int, US_int)
INR_monthly_fama
IDR_monthly_fama <- fama_monthly(IDR_mon_merge, IDR_fx, IDR_int, US_int)
IDR_monthly_fama
