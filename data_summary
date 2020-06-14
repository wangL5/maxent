#R v.3.6.1
setwd("/Users/Winni/Desktop/maxent_2020/nerrs_data/")
library(tidyr); library(dplyr);library(lubridate);library(PerformanceAnalytics)

###water quality data###
#Charleston Bridge - soschwq

###nutrient data###
#Charleston Bridge - soschnut

###weather station###
#Charleston - soscmmet
charleston_wq <- read.csv(file="2010_data/soschwq2010.csv", sep=",", header=TRUE)


#max temp
max(charleston_wq$Temp, na.rm=TRUE) #18.3

#max sal
max(charleston_wq$Sal, na.rm=TRUE) #33.7

#max turb
max(charleston_wq$Turb, na.rm=TRUE) #198

#max pH
max(charleston_wq$pH, na.rm=TRUE) #8.3

#max Do
max(charleston_wq$DO_mgl, na.rm=TRUE) #12.7

#max ChlFluor
max(charleston_wq$ChlFluor, na.rm=TRUE) #398.6

##charleston
charleston_wq_wet <- charleston_wq %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb, ChlFluor) %>%
  filter(date >= as.POSIXct("2010-02-01") & date <= as.POSIXct("2010-04-30")) %>%
  summarise_all("mean", na.rm=TRUE)
charleston_wq_wet

charleston_wq_dry <- charleston_wq %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb, ChlFluor) %>%
  filter(date >= as.POSIXct("2010-05-01") & date <= as.POSIXct("2010-07-31")) %>%
  summarise_all("mean", na.rm=TRUE)
charleston_wq_dry

charleston_wq_annual <- charleston_wq %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb, ChlFluor) %>%
  summarise_all("mean", na.rm=TRUE)
charleston_wq_annual

charleston_wq_fall <- charleston_wq %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb, ChlFluor) %>%
  filter(date >= as.POSIXct("2010-08-01") & date <= as.POSIXct("2010-10-31")) %>%
  summarise_all("mean", na.rm=TRUE)
charleston_wq_fall


####nutrient data####
charleston_nut <- read.csv(file="2010_data/soschnut2010.csv", sep=",", header=TRUE)

max(charleston_nut$PO4F, na.rm=TRUE) #0.073

max(charleston_nut$NO3F, na.rm=TRUE) #0.36

max(charleston_nut$CHLA_N, na.rm=TRUE) #18.21

charleston_nut_annual <- charleston_nut %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(PO4F,NO3F,CHLA_N) %>%
  summarise_all("mean", na.rm=TRUE)
charleston_nut_annual

charleston_nut_upwell <- charleston_nut %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date,PO4F,NO3F,CHLA_N) %>%
  filter(date >= as.POSIXct("2010-04-01") & date <= as.POSIXct("2010-09-01"))%>%
  summarise_all("mean", na.rm=TRUE)
charleston_nut_upwell

#testing all 2010 data for correlations#
summary_nut_2010 <- read.table(file="2010_data/2010_nutrient_summary.txt", header=TRUE, sep="\t", row.names=1)
head(summary_nut_2010)
cor(summary_nut_2010, method="pearson")
chart.Correlation(summary_nut_2010, histogram=TRUE, pch=19)

summary_wq_2010 <- read.table(file="2010_data/2010_wq_summary.txt", header=TRUE, sep="\t", row.names=1)
head(summary_wq_2010)
cor(summary_wq_2010, method="pearson")
chart.Correlation(summary_wq_2010, histogram=TRUE, pch=19)
