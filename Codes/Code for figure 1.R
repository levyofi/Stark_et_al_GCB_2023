library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)
library(ggpubr)

setwd('C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation')
Data_summer <- read.csv("summer ground temp with daytime only data.csv",header=T)
#Preparing micro-habitat data (summer/winter):
Data_summer_new <- tidyr::separate(Data_summer, Time_Date, c("Date", "Time"), sep = " ",remove = FALSE) #separates the time and date column
mean_summer_data <- aggregate(iButton_Temp ~ Date+Time+Object+Size, data = Data_summer_new, mean)#create mean values according to object and size
str(mean_summer_data)
t <- hm((mean_summer_data$Time))
mean_summer_data$tnum = hour(t)+minute(t)/60
mean_summer_data
mean_summer_data <- na.omit(mean_summer_data)

#Subseting according to Rock/Bush
sub_model_rock_summer <-subset(mean_summer_data, Object == "Rock",select= Object:tnum) 
str(sub_model_rock_summer)
sub_model_bush_summer <-subset(mean_summer_data, Object == "Bush",select= Object:tnum) 
str(sub_model_bush_summer)

setwd('C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation/Temperature preference experiment/Thermal Pictures and Data of M. bahaeldini')
#uploading Tpref data:
Tpref_data_summer <- read.csv("Summer Tpref data for Figure 1.csv",header=T)
str(Tpref_data_summer)
t3 <- hm(Tpref_data_summer$Hour_of_the_day)
Tpref_data_summer$hour=hour(t3)
Tpref_data_summer$tnum=hour(t3)+minute(t3)/60
Tpref_data_summer

###FIGURE 1:Comparison between temps of different sizes of shelters and Tpref across the DAY:
summer <- ggplot(sub_model_rock_summer/sub_model_bush_summer, aes(tnum, iButton_Temp))+geom_smooth(aes(linetype=Size,color=Size),se=TRUE)+
  geom_smooth(Tpref_data_summer, mapping = aes(x = tnum , y = Mean_preferred_body_temperature), size=1, color="purple",se=TRUE)
summer
summer1 <- summer+theme_bw()+theme(axis.text=element_text(face='bold', size=20), 
                         axis.text.x=element_text(face='bold',size=20)
                         ,axis.title.x=element_text(face='bold', size=20),
                         axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  labs(x="Time of the day", y="Ground temperature (°C)")+
  scale_y_continuous(limits = c(30, 45))+scale_x_discrete(breaks =  seq(8, 17,1)
              ,labels = c("8:00","9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00"))

summer2 <- summer1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
summer2

###############################Winter###################################################
setwd('C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation')
Data_winter <- read.csv("Winter data for ground temperature.csv",header=T)
Data_winter_new <- tidyr::separate(Data_winter, Time_Date, c("Date", "Time"), sep = " ",remove = FALSE) #separates the time and date column
mean_winter_data <- aggregate(iButton_Temp ~ Date+Time+Object+Size, data = Data_winter_new, mean)#create mean values according to object and size
str(mean_winter_data)
t <- hm((mean_winter_data$Time))
mean_winter_data$tnum = hour(t)+minute(t)/60
mean_winter_data
mean_winter_data <- na.omit(mean_winter_data)

#Subseting according to Rock/Bush
sub_model_rock_winter <-subset(mean_winter_data, Object == "Rock",select= Object:tnum) 
str(sub_model_rock_winter)
sub_model_bush_winter <-subset(mean_winter_data, Object == "Bush",select= Object:tnum) 
str(sub_model_bush_winter)

setwd('C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation/Temperature preference experiment/Thermal Pictures and Data of M. bahaeldini')
#uploading Tpref data:
Tpref_data_winter <- read.csv("Winter Tpref data for Figure 1.csv",header=T)
str(Tpref_data_winter)
t3 <- hm(Tpref_data_winter$Hour_of_the_day)
Tpref_data_winter$hour=hour(t3)
Tpref_data_winter$tnum=hour(t3)+minute(t3)/60
Tpref_data_winter

###FIGURE 1:Comparison between temps of different sizes of shelters and Tpref across the DAY:
winter <- ggplot(sub_model_rock_winter/sub_model_bush_winter, aes(tnum, iButton_Temp))+geom_smooth(aes(linetype=Size,color=Size),se=TRUE)+
  geom_smooth(Tpref_data_winter, mapping = aes(x = tnum , y = Mean_preferred_body_temperature), size=1, color="purple",se=TRUE)
winter
winter1 <- winter+theme_bw()+theme(axis.text=element_text(face='bold', size=20), 
                         axis.text.x=element_text(face='bold',size=20)
                         ,axis.title.x=element_text(face='bold', size=20),
                         axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  scale_y_continuous(name="Ground temperature (°C)",limits = c(15, 35))+
  scale_x_continuous(limits = c(8, 16), labels = c("8:00","10:00","12:00","14:00","16:00"))
winter2 <- d1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
winter2


