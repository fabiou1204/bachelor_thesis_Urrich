#check of interest rate data
#BA check for missing values in data
library(tidyverse)
library(dplyr)
library(lubridate) 

setwd("/Users/fabiourrich/Library/CloudStorage/OneDrive-Personal/UIP_fx_volatility/Data/BA_Fabio/01_data/interest_rate")
AUD <- readxl::read_xlsx("AUDOND.xlsx")
EUR <- readxl::read_xlsx("EUROND.xlsx")
GBP <- readxl::read_xlsx("GBPOND.xlsx")
USD <- readxl::read_xlsx("USDOND.xlsx")
ZAR <- readxl::read_xlsx("ZAROND.xlsx")
INR <- readxl::read_xlsx("INROND.xlsx")
IDR <- readxl::read_xlsx("IDROND.xlsx")

EUR <- EUR %>% mutate (Date = as_date(Date))
AUD <- AUD %>% mutate (Date = as_date(Date))
GBP <- GBP %>% mutate (Date = as_date(Date))
USD <- USD %>% mutate (Date = as_date(Date))
ZAR <- ZAR %>% mutate (Date = as_date(Date))
INR <- INR %>% mutate (Date = as_date(Date))
IDR <- IDR %>% mutate (Date = as_date(Date))

EUR$Year <- year(EUR$Date)
AUD$Year <- year(AUD$Date)
GBP$Year <- year(GBP$Date)
USD$Year <- year(USD$Date)
ZAR$Year <- year(ZAR$Date)
INR$Year <- year(INR$Date)
IDR$Year <- year(IDR$Date)

EUR_no <- EUR %>% group_by(Year) %>% count()
AUD_no <- AUD %>% group_by(Year) %>% count()
GBP_no <- GBP %>% group_by(Year) %>% count()
USD_no <- USD %>% group_by(Year) %>% count()
ZAR_no <- ZAR %>% group_by(Year) %>% count()
INR_no <- INR %>% group_by(Year) %>% count()
IDR_no <- IDR %>% group_by(Year) %>% count()

#number of observations for each country for the period 01-01-2000 to 31-12-2005 
EUR_no_2000_2005 <- EUR_no %>% filter(Year >= 2000 & Year <= 2005)
sum(EUR_no_2000_2005$n)
AUD_no_2000_2005 <- AUD_no %>% filter(Year >= 2000 & Year <= 2005)
sum(AUD_no_2000_2005$n)
GBP_no_2000_2005 <- GBP_no %>% filter(Year >= 2000 & Year <= 2005)
sum(GBP_no_2000_2005$n)
USD_no_2000_2005 <- USD_no %>% filter(Year >= 2000 & Year <= 2005)
sum(USD_no_2000_2005$n)
ZAR_no_2000_2005 <- ZAR_no %>% filter(Year >= 2000 & Year <= 2005)
sum(ZAR_no_2000_2005$n)
INR_no_2000_2005 <- INR_no %>% filter(Year >= 2000 & Year <= 2005)
sum(INR_no_2000_2005$n)
IDR_no_2000_2005 <- IDR_no %>% filter(Year >= 2000 & Year <= 2005)
sum(IDR_no_2000_2005$n)

# use period from 2000 to 2025; only two years where one country has less than 100 observations: India (60) in 2024 and South Africa (88) in 2000
#still needed: fx data for Indonesia, because not available on FRED