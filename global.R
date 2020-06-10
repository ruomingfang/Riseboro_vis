# global.R for the riseboro visulization project. 
# This file contains pre-processing steps and variables visible to both server and ui.

library(shiny)
library(tidyverse)
library(lubridate)
library(Hmisc)


# setwd("~/Desktop/Ballbreaker/RA/Riseboro/Riseboro_vis")  #for setting any working directory
varnames <- c("PM1(µg/m3)","PM2.5(µg/m3)","PM10(µg/m3)","CO2(ppm)","CO(ppb)","NO2(ppb)","NO(ppb)","O3(ppb)","Temperature(degC)","Relative Humidity(%)")
sensorlist <- c("1D","2D","3D","4D","Boiler Room","Rooftop")

# Define functions

extractData <- function(filepath){
  sourceData = read.csv(filepath)
  sensorName = tools::file_path_sans_ext(basename(filepath)) #works best if filename=sensorname
  sensorData = sourceData %>% select(id,timestamp_local,sn,temp_manifold,rh_manifold,co2,voc,co,no,no2,o3,pm1,pm25,pm10) #modify here to change the attributes to extract
  sensorData$sensorname = sensorName
  return(sensorData)
}

# Define lookup
lookupattr <- list("PM1(µg/m3)"="pm1","PM2.5(µg/m3)"="pm25","PM10(µg/m3)"="pm10","CO2(ppm)"="co2","CO(ppb)"="co","NO2(ppb)"="no2","NO(ppb)"="no","O3(ppb)"="o3","Temperature(degC)" = "temp_manifold","Relative Humidity(%)" = "rh_manifold","No Comparison" = "No Comparison")
lookupattrname <- list(pm1="PM1(µg/m3)",pm25="PM2.5(µg/m3)",pm10="PM10(µg/m3)",co2="CO2(ppm)",co="CO(ppb)",no2="NO2(ppb)",no="NO(ppb)",o3="O3(ppb)",temp_manifold = "Temperature(degC)","rh_manifold" = "Relative Humidity(%)","No Comparison" = "No Comparison")
lookupsensor <- list("1D"="1D","2D"="2D","3D"="3D","4D"="4D","Boiler Room" = "Boiler","Rooftop"="Rooftop")


# getdata
filenames <- list.files(path = "./Data02130601",pattern = "csv",full.names = TRUE)
data = data.frame()
for (f in filenames){
  name = tools::file_path_sans_ext(basename(f))
  data = rbind(data,extractData(f))
}

#Process time
data$timestamp_local = as.POSIXct(data$timestamp_local,format = '%Y-%m-%dT%H:%M:%SZ', tz = "EST")
data = data %>% 
  mutate(st_week = floor_date(timestamp_local,"week")) %>%
  mutate(st_hour = floor_date(timestamp_local,"hour")) %>%
  mutate(st_time_inweek = st_hour - st_week) %>%
  mutate(st_time_all_in_oneweek = st_week[1] + st_time_inweek) # put every obs in one week

data = data %>%
  mutate(st_day = floor_date(timestamp_local,"day")) %>%
  mutate(st_time_inday = st_hour - st_day) %>%
  mutate(st_time_all_in_oneday = st_day[1] + st_time_inday) #put every obs in one day

#get data time range
datadaterange = range(data$timestamp_local,na.rm = TRUE)
datadatemin = as.Date(datadaterange[1],'EST')
datadatemax = as.Date(datadaterange[2],'EST')
