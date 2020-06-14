#R v.3.6.1
###5/25/2020###
#using swmpr to download and summarize data from nerrsdata
install.packages("SWMPr")
library(SWMPr)

###water quality data###
#Charleston Bridge - soschwq
#Valino Island - sosvawq
#Winchester - soswiwq

###nutrient data###
#Charleston Bridge - soschnut
#Valino Island - sosvanut
#Winchester - soswinut

#looking at years 2010, 2016, and 2018
## import water quality data from a given date range
#all_params_dtrng(station_code, dtrng, param=NULL, trace=TRUE, Max=NULL)
#example: all_params_dtrng('stationcode', c('01/01/2010', '12/31/2010'))
#can also retrieve single parameters given a date range but it won't come with QC flags

#local file import, after you've downloaded data outside of R
#import_local(path, station_code, trace=FALSE, collMethd = c("1,"2"))


#filter data based on qc flag, default is 0
#option to include 1-flagged data: qaqc(swmpr_in, qaqc_keep=c('0', '1'))
#qaqc(data)

#rem_reps removes replicates in nutrient data
#subset.swmpr subsets a swmpr object by a date range, parameters, or non-empty values
#subset string form of 'YYYY-mm-dd HH:MM' to subset a date range
#subset(data, subset=c('YYYY-mm-dd HH:MM', 'YYYY-mm-dd HH:MM'))

#2016 water quality
path2016 <- '/Users/Winni/Desktop/maxent_2020/nerrs_data/later/2016'
charles2016 <- import_local(path2016, 'soschwq2016') #charleston bridge
valino2016 <- import_local(path2016, 'sosvawq2016') #valino island
winchester2016 <- import_local(path2016, 'soswiwq2016') #winchester

charles2016 <- qaqc(charles2016)
valino2016 <- qaqc(valino2016)
winchester2016 <- qaqc(winchester2016)

#2016 nutrient
charlesnut2016 <- import_local(path2016, 'soschnut2016') #charleston bridge
valinonut2016 <- import_local(path2016, 'sosvanut2016') #valino
winchesternut2016 <- import_local(path2016, 'soswinut2016') #winchester

#include 1 flags for CHB (held beyond specified holding time)
charlesnut2016 <- qaqc(charlesnut2016, qaqc_keep=c('0', 'CHB'))
valinonut2016 <- qaqc(valinonut2016, qaqc_keep =c('0', 'CHB'))
winchesternut2016 <- qaqc(winchesternut2016, qaqc_keep=c('0', 'CHB'))
#0, 'CHB'

charlesnut2016 <- rem_reps(charlesnut2016)
valinonut2016 <- rem_reps(valinonut2016)
winchesternut2016 <- rem_reps(winchesternut2016)

##summaries
charles2016 %>% summarise_all("mean", na.rm=TRUE)
valino2016 %>% summarise_all("mean", na.rm=TRUE)
winchester2016 %>% summarise_all("mean", na.rm=TRUE)

charles2016 %>% summarise_all("max", na.rm=TRUE)
valino2016 %>% summarise_all("max", na.rm=TRUE)
winchester2016 %>% summarise_all("max", na.rm=TRUE)

#feb-april
subset(charles2016, subset=c('2016-02-01 00:00', '2016-04-30 23:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(valino2016, subset=c('2016-02-01 00:00', '2016-04-30 23:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(winchester2016, subset=c('2016-02-01 00:00', '2016-04-30 23:45')) %>% summarise_all("mean", na.rm=TRUE)

#aug-october
subset(charles2016, subset=c('2016-08-01 00:00', '2016-10-31 23:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(valino2016, subset=c('2016-08-01 00:00', '2016-10-31 23:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(winchester2016, subset=c('2016-08-01 00:00', '2016-10-31 23:45')) %>% summarise_all("mean", na.rm=TRUE)

#nutrient
charlesnut2016 %>% summarise_all("mean", na.rm=TRUE)
valinonut2016 %>% summarise_all("mean", na.rm=TRUE)
winchesternut2016 %>% summarise_all("mean", na.rm=TRUE)

charlesnut2016 %>% summarise_all("max", na.rm=TRUE)
valinonut2016 %>% summarise_all("max", na.rm=TRUE)
winchesternut2016 %>% summarise_all("max", na.rm=TRUE)

#aprl-sept
subset(charlesnut2016, subset=c('2016-04-25 15:03', '2016-09-28 07:15')) %>% summarise_all("mean", na.rm=TRUE)
subset(valinonut2016, subset=c('2016-04-25 15:03', '2016-09-28 07:15')) %>% summarise_all("mean", na.rm=TRUE)
subset(winchesternut2016, subset=c('2016-04-25 15:03', '2016-09-28 07:15')) %>% summarise_all("mean", na.rm=TRUE)

###coos
empire2016 <- read.csv(file="later/2016/Empire_2016.txt", sep="\t", header=TRUE)
northspit2016 <- read.csv(file="later/2016/BLM_2016.txt", sep="\t", header=TRUE)


###empire
empirewet2016 <- empire2016 %>%
  mutate(date = mdy_hms(DateTimeStamp)) %>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb) %>%
  filter(date >= as.POSIXct("2016-02-01") & date <= as.POSIXct("2016-04-30")) %>%
  summarise_all("mean", na.rm=TRUE)
empirewet2016

empire_wq_2016 <- empire2016 %>%
  mutate(date = mdy_hms(DateTimeStamp)) %>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb) %>%
  summarise_all("mean", na.rm=TRUE)
empire_wq_2016

empire_fall_2016 <- empire2016 %>%
  mutate(date = mdy_hms(DateTimeStamp)) %>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb) %>%
  filter(date >= as.POSIXct("2016-08-01") & date <= as.POSIXct("2016-10-31")) %>%
  summarise_all("mean", na.rm=TRUE)
empire_fall_2016

##north spit##
northspit_wet_2016 <- northspit2016 %>%
  mutate(date = mdy_hms(DateTimeStamp)) %>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb) %>%
  filter(date >= as.POSIXct("2016-02-01") & date <= as.POSIXct("2016-04-30")) %>%
  summarise_all("mean", na.rm=TRUE)
northspit_wet_2016

northspit_wq_annual <- northspit2016 %>%
  mutate(date = mdy_hms(DateTimeStamp))%>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb) %>%
  summarise_all("mean", na.rm=TRUE)
northspit_wq_annual

northspit_fall_2016 <- northspit2016 %>%
  mutate(date = mdy_hms(DateTimeStamp)) %>%
  select(date, Temp, Sal, DO_mgl, Depth, pH, Turb) %>%
  filter(date >= as.POSIXct("2016-08-01") & date <= as.POSIXct("2016-10-31")) %>%
  summarise_all("mean", na.rm=TRUE)
northspit_fall_2016


###2018 water quality###
path2018 <- '/Users/Winni/Desktop/maxent_2020/nerrs_data/later/2018'
charles2018 <- import_local(path2018, 'soschwq2018') #charleston bridge
valino2018 <- import_local(path2018, 'sosvawq2018') #valino island
winchester2018 <- import_local(path2018, 'soswiwq2018') #winchester

charles2018 <- qaqc(charles2018)
valino2018 <- qaqc(valino2018)
winchester2018 <- qaqc(winchester2018)

#2018 nutrient
charlesnut2018 <- import_local(path2018, 'soschnut2018') #charleston bridge
valinonut2018 <- import_local(path2018, 'sosvanut2018') #valino
winchesternut2018 <- import_local(path2018, 'soswinut2018') #winchester

#include 1 flags for CHB (held beyond specified holding time)
charlesnut2018 <- qaqc(charlesnut2018, qaqc_keep=c('0', 'CHB'))
valinonut2018 <- qaqc(valinonut2018, qaqc_keep =c('0', 'CHB'))
winchesternut2018 <- qaqc(winchesternut2018, qaqc_keep=c('0', 'CHB'))
#0, 'CHB'

charlesnut2018 <- rem_reps(charlesnut2018)
valinonut2018 <- rem_reps(valinonut2018)
winchesternut2018 <- rem_reps(winchesternut2018)

##summaries
charles2018 %>% summarise_all("mean", na.rm=TRUE)
valino2018 %>% summarise_all("mean", na.rm=TRUE)
winchester2018 %>% summarise_all("mean", na.rm=TRUE)

charles2018 %>% summarise_all("max", na.rm=TRUE)
valino2018 %>% summarise_all("max", na.rm=TRUE)
winchester2018 %>% summarise_all("max", na.rm=TRUE)

#feb-april
subset(charles2018, subset=c('2018-02-01 00:00', '2018-04-30 23:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(valino2018, subset=c('2018-02-01 00:00', '2018-04-30 23:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(winchester2018, subset=c('2018-02-01 00:00', '2018-04-30 23:45')) %>% summarise_all("mean", na.rm=TRUE)

#aug-october
subset(charles2018, subset=c('2018-08-01 00:00', '2018-10-31 23:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(valino2018, subset=c('2018-08-01 00:00', '2018-10-31 23:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(winchester2018, subset=c('2018-08-01 00:00', '2018-10-31 23:45')) %>% summarise_all("mean", na.rm=TRUE)

#nutrient
charlesnut2018 %>% summarise_all("mean", na.rm=TRUE)
valinonut2018 %>% summarise_all("mean", na.rm=TRUE)
winchesternut2018 %>% summarise_all("mean", na.rm=TRUE)

charlesnut2018 %>% summarise_all("max", na.rm=TRUE)
valinonut2018 %>% summarise_all("max", na.rm=TRUE)
winchesternut2018 %>% summarise_all("max", na.rm=TRUE)

#aprl-sept
subset(charlesnut2018, subset=c('2018-04-24 11:39', '2018-09-28 06:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(valinonut2018, subset=c('2018-04-24 11:39', '2018-09-28 06:45')) %>% summarise_all("mean", na.rm=TRUE)
subset(winchesternut2018, subset=c('2018-04-24 11:39', '2018-09-28 06:45')) %>% summarise_all("mean", na.rm=TRUE)



###coos 2018
empire2018 <- read.table(file="Empire_2018.txt", sep="\t", header=TRUE)
northspit2018 <- read.csv(file="BLM_2018.txt", sep="\t", header=TRUE)


###empire
empirewet2018 <- empire2018 %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date, Temp_C, Sal.psu, ODO.mg.L, Depth.m, pH, Turbidity.FNU) %>%
  filter(date >= as.POSIXct("2018-02-01") & date <= as.POSIXct("2018-04-30")) %>%
  summarise_all("mean", na.rm=TRUE)
empirewet2018

empire_wq_2018 <- empire2018 %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date, Temp_C, Sal.psu, ODO.mg.L, Depth.m, pH, Turbidity.FNU) %>%
  summarise_all("mean", na.rm=TRUE)
empire_wq_2018

empire_fall_2018 <- empire2018 %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date, Temp_C, Sal.psu, ODO.mg.L, Depth.m, pH, Turbidity.FNU) %>%
  filter(date >= as.POSIXct("2018-08-01") & date <= as.POSIXct("2018-10-31")) %>%
  summarise_all("mean", na.rm=TRUE)
empire_fall_2018

##north spit##
northspit_wet_2018 <- northspit2018 %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date, Temp_C, Sal.psu, ODO.mg.L, Depth.m, pH, Turbidity.FNU) %>%
  filter(date >= as.POSIXct("2018-02-01") & date <= as.POSIXct("2018-04-30")) %>%
  summarise_all("mean", na.rm=TRUE)
northspit_wet_2018

northspit_wq_annual <- northspit2018 %>%
  mutate(date = mdy_hm(DateTimeStamp))%>%
  select(date, Temp_C, Sal.psu, ODO.mg.L, Depth.m, pH, Turbidity.FNU) %>%
  summarise_all("mean", na.rm=TRUE)
northspit_wq_annual

northspit_fall_2018 <- northspit2018 %>%
  mutate(date = mdy_hm(DateTimeStamp)) %>%
  select(date, Temp_C, Sal.psu, ODO.mg.L, Depth.m, pH, Turbidity.FNU) %>%
  filter(date >= as.POSIXct("2018-08-01") & date <= as.POSIXct("2018-10-31")) %>%
  summarise_all("mean", na.rm=TRUE)
northspit_fall_2018

setwd("/Users/Winni/Desktop/maxent_2020/nerrs_data/later/")
#testing all 2016 data for correlations#
summary_nut_2016 <- read.table(file="2016/2016_nutr_all.txt", header=TRUE, sep="\t", row.names=1)
head(summary_nut_2016)
cor(summary_nut_2016, method="pearson", na.rm = TRUE)
library(PerformanceAnalytics)
chart.Correlation(summary_nut_2016, histogram=TRUE, pch=19)

summary_wq_2016 <- read.table(file="2016/2016_wq_all.txt", header=TRUE, sep="\t", row.names=1)
head(summary_wq_2016)
cor(summary_wq_2016, method="pearson")
chart.Correlation(summary_wq_2016, histogram=TRUE, pch=19)

#testing all 2018 data for correlations#
summary_nut_2018 <- read.table(file="2018/2018_nutr_all.txt", header=TRUE, sep="\t", row.names=1)
head(summary_nut_2018)
cor(summary_nut_2018, method="pearson")
library(PerformanceAnalytics)
chart.Correlation(summary_nut_2018, histogram=TRUE, pch=19)

summary_wq_2018 <- read.table(file="2018/2018_wq_all.txt", header=TRUE, sep="\t", row.names=1)
head(summary_wq_2018)
cor(summary_wq_2018, method="pearson")
chart.Correlation(summary_wq_2018, histogram=TRUE, pch=19)
