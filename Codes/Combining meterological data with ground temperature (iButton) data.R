library(ape)
library(phytools)
library(plyr)
library(car)
library(fmsb)
library(FSA)
library(ggplot2)
library(caper)
library(lmtest)
library(MASS)
library(mglmn)
library(lubridate)
library(MASS)
library(rayshader)
library("lme4")
library(MuMIn)
library(Rmisc)
library(Hmisc)
library(dplyr)
library(Epi)
library(statsr)
library(dplyr)
library(stringr)
library(lubridate)
options(scipen = 999)
############Code can be applied for both summer and winter data (example below refer to winter data)############
#Winter:
#First: fixing the am-pm problem in the ibutton dataset:
#Winter:
winter_ibutton = read.csv("Full iButton Dataset.csv")
with_AM_winter = winter_ibutton[str_detect(winter_ibutton$Time_Date, "M"),]
with_AM_winter$dt = mdy_hms(with_AM_winter$Time_Date)
without_AM_winter = winter_ibutton[!str_detect(winter_ibutton$Time_Date, "M"),]
without_AM_winter$dt = mdy_hm(without_AM_winter$Time_Date)

fixed_winter_ibutton = rbind(with_AM_winter, without_AM_winter)
fixed_winter_ibutton = fixed_winter_ibutton[order(fixed_winter_ibutton$dt),]
write.table(fixed_winter_ibutton, file="fixed winter ibutton dataset.csv", row.names = F, col.names = T, sep=",")

#Set columns for winterdata and time separately:
meterological_data_winter <- read.csv("CR300Series_MeteoData-winter.csv", sep=",", header = T)
meterological_data_winter <- tidyr::separate(meterological_data_winter,TIMESTAMP, c("Date", "Time"), sep = " ",remove = FALSE)
meterological_data_winter$dt <- dmy_hm(paste(meterological_data_winter$Date, meterological_data_winter$Time))
meterological_data_winter$round_dt <- round_date(meterological_data_winter$dt, "30 minutes")

#Load winter iButton data:
ibutton_data_winter <- read.table("fixed winter ibutton dataset.csv", sep=",", header = T)
ibutton_data_winter <- tidyr::separate(ibutton_data_winter,Time_Date, c("Date", "Time"), sep = " ",remove = FALSE)
ibutton_data_winter$dt <- mdy_hms(paste(ibutton_data_winter$Date, ibutton_data_winter$Time))
ibutton_data_winter$round_dt = round_date(ibutton_data_winter$dt, "30 minutes")

#Merge tables for winter data:
ibutton_meterological_winter <- merge(ibutton_data_winter, meterological_data_winter, by="round_dt")


write.table(ibutton_meterological_winter, file="new_winter_microclimate_iButton.csv", row.names = F, col.names = T, sep=",")




write.table(ibutton_meterological_winter, file="microclimate_fieldata.csv", row.names = F, col.names = T, sep=",")
