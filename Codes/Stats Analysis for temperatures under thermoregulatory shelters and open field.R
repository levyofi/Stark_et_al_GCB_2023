library(lubridate)
library(dplyr)
library(tidyr)
options(scipen = 999)
setwd('C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation')
Data <- read.csv('Winter data for ground temperature.csv'/'Summer data for ground temperature.csv'/'Summer_microclimate (June-September 2020).csv'/'Winter_microclimate (December-Feburary 2020).csv ')
#Create table of mean&min&max values for the raw table:
Data <- tidyr::separate(Data, Time_Date, c("Date", "Time"), sep = " ",remove = FALSE) #separates the time and date column
Max_ibutton_temp <- aggregate(iButton_Temp ~ Date+Object+Size, data = Data, max)#create maximum values according to object and size
Min_ibutton_temp <- aggregate(iButton_Temp ~ Date+Object+Size, data = Data, min)#create minimum values according to object and size
Mean_ibutton_temp <- aggregate(iButton_Temp ~ Date+Object+Size, data = Data, mean)#create average values according to object and size
###stats for maximum temp under ground###
library(dplyr)
Max_ibutton_temp_bush <- Max_ibutton_temp %>%
  group_by(Object) %>%
  filter(!any(Object == "Rock"))
Max_ibutton_temp_mean <- aggregate(Max_ibutton_temp_bush[, 4], list(Max_ibutton_temp_bush$Size), mean)
Max_ibutton_temp_sd <- aggregate(Max_ibutton_temp_bush[, 4], list(Max_ibutton_temp_bush$Size), sd)

###stats for minimum temp under ground###
library(dplyr)
Min_ibutton_temp_rock <- Min_ibutton_temp %>%
  group_by(Object) %>%
  filter(!any(Object == "Rock"))
Min_ibutton_temp_mean <- aggregate(Min_ibutton_temp_rock[, 4], list(Min_ibutton_temp_rock$Size), mean)
Min_ibutton_temp_sd <- aggregate(Min_ibutton_temp_rock[, 4], list(Min_ibutton_temp_rock$Size), sd)

###stats for average temp under ground###
library(dplyr)
mean_ibutton_temp_rock <- Mean_ibutton_temp %>%
  group_by(Object) %>%
  filter(!any(Object == "Rock"))
mean_ibutton_temp_mean <- aggregate(mean_ibutton_temp_rock[, 4], list(mean_ibutton_temp_rock$Size), mean)
mean_ibutton_temp_sd <- aggregate(mean_ibutton_temp_rock[, 4], list(mean_ibutton_temp_rock$Size), sd)

##Stats for open field temps
Max_open_temp <- aggregate(iButton_Temp ~ Date+Object+Size, data = Data, max)#create maximum values according to object and size
Min_open_temp <- aggregate(iButton_Temp ~ Date+Object+Size, data = Data, min)#create minimum values according to object and size
Mean_open_temp <- aggregate(iButton_Temp ~ Date+Object+Size, data = Data, mean)





