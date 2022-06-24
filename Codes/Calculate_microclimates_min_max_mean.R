library(lubridate)
library(dplyr)
library(plyr)
library(tidyr)
options(scipen = 999)


#rocks and bushes
Data_summer <- read.csv('Stark_et_al_ELE/Data/Summer microclimate fieldata.csv')
Data_winter <- read.csv('Stark_et_al_ELE/Data/Winter microclimate fieldata.csv')
Data = rbind(Data_summer, Data_winter)
#Create table of mean&min&max values for the raw table:
Data$round_dt <- ymd_hms(Data$round_dt) 
Data = Data[Data$Radiation_Avg>5,]
#organize data
#first - calculate the average across all ibuttons for every hour
mean_data = aggregate(iButton_Temp ~ round_dt+Object+Size+Season, data = Data, mean)
#next - calculate the max, min, and mean for every day
mean_data$Date = date(mean_data$round_dt)
agg_data = ddply(mean_data, .(Date,Object,Size,Season), summarise,
                 min_temp = min(iButton_Temp), max_temp = max(iButton_Temp), mean_temp = mean(iButton_Temp))
#finaly - calculate the average max, min, and mean across days
mean_sd_shade =  ddply(agg_data, .(Object,Size,Season), summarise,
                              min = mean(min_temp), max = mean(max_temp), mean = mean(mean_temp), sd_min = sd(min_temp), sd_max = sd(max_temp), sd_mean = sd(mean_temp))

##Stats for open field temps
Data_summer <- read.csv('Stark_et_al_ELE/Data/Summer station data.csv')
Data_summer$Season="Summer"
Data_winter <- read.csv('Stark_et_al_ELE/Data/Winter station data.csv')
Data_winter$Season="Winter"
Data = rbind(Data_summer, Data_winter)
Data = Data[Data$Radiation_Avg>5 & (!is.nan(Data$IR_Temp_Avg)),]
Data$TIMESTAMP <- ymd_hms(Data$TIMESTAMP)

#organize data
#first - calculate the max, min, and mean for every day
Data$Date = date(Data$TIMESTAMP)
agg_data = ddply(Data, .(Date, Season), summarise,
                 min_temp = min(IR_Temp_Avg), max_temp = max(IR_Temp_Avg), mean_temp = mean(IR_Temp_Avg), n=length(IR_Temp_Avg))
#take only days with full data
agg_data = agg_data[agg_data$n>50,]

#finaly - calculate the average max, min, and mean across days
mean_sd_open =  ddply(agg_data, .(Season), summarise,
                              min = mean(min_temp), max = mean(max_temp), mean = mean(mean_temp), sd_min = sd(min_temp), sd_max = sd(max_temp), sd_mean = sd(mean_temp))
mean_sd_open$Object = "Open"
mean_sd_open$Size = ""

results = rbind(mean_sd_shade, mean_sd_open)

#write results
results$mean = sprintf(results$mean, results$sd_mean, fmt = '%#.1f ± %#.1f')# paste(round(results$mean,1, ), "±", round(results$sd_mean,1))
results$min = sprintf(results$min, results$sd_min, fmt = '%#.1f ± %#.1f')# paste(round(results$mean,1, ), "±", round(results$sd_mean,1))
results$max = sprintf(results$max, results$sd_max, fmt = '%#.1f ± %#.1f')# paste(round(results$mean,1, ), "±", round(results$sd_mean,1))
results$Object = factor(results$Object, levels = c("Open", "Bush", "Rock" ))
results$Size = factor(results$Size, levels = c("Small", "Medium", "Large", "" ))

results_by_season = merge(results[results$Season=="Summer",], results[results$Season=="Winter",], by=c("Object", "Size") )
results_by_season = results_by_season[order(results_by_season$Object, results_by_season$Size),]
results_by_season
write.table(results_by_season, file = "microhabitats_stats.csv", row.names = F)
