library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)
library(ggpubr)

setwd('C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation')
Data <- read.csv(file.choose(),header=T)

#Create table of min&max values for the raw table:
Data <- tidyr::separate(Data, Time_Date, c("Date", "Time"), sep = " ",remove = FALSE) #separates the time and date column
Max_temp <- aggregate(iButton_Temp ~ Date+Object+Size, data = Data, max)#create maximum values according to object and size
Min_min <- aggregate(iButton_Temp ~ Date+Object+Size, data = Data, min)#create minimum values according to object and size
mean_data <- aggregate(iButton_Temp ~ Date+Time+Object+Size, data = Data, mean)#create mean values according to object and size
New_data <- data.frame(Max_temp,Min_min) #creates new data frame
colnames(New_data) <- c('Time_max_temp','Max_temp_for_Object','Max_temp_for_Size','Max_temp','Time_min_temp','Min_temp_for_Object','Min_temp_for_Size','Min_temp')#renaming the new data-frame
write.csv(Data, file = paste("winter_microclimate_with_mesalina_temps 6.4.2022",today(), "csv", sep="." ), row.names = FALSE)

newer_mean_data_winter <- aggregate(iButton_Temp ~ Time+Object+Size+tnum,data = mean_data, mean)
newer_mean_data_winter
newer_mean_data_summer <- aggregate(iButton_Temp ~ Time+Object+Size+tnum,data = mean_data, mean)
newer_mean_data_summer

#winter - subset according to Rock/Bush
sub_model_rock_winter <-subset(newer_mean_data_winter, Object == "Rock",select= Time:iButton_Temp) 
str(sub_model_rock_winter)
sub_model_bush_winter <-subset(newer_mean_data_winter, Object == "Bush",select= Time:iButton_Temp) 
str(sub_model_bush_winter)
#Summer - subset according to Rock/Bush
sub_model_rock_summer <-subset(newer_mean_data_summer, Object == "Rock",select= Time:iButton_Temp) 
str(sub_model_rock_summer)
sub_model_bush_summer <-subset(newer_mean_data_summer, Object == "Bush",select= Time:iButton_Temp) 
str(sub_model_bush_summer)
#winter
winter_max_rock <- lm(Max_temp~Max_temp_for_Size, data=sub_model_rock_winter)
summary(winter_max_rock)
winter_max_bush <- lm(Max_temp~Max_temp_for_Size, data=sub_model_bush_winter)
summary(winter_max_bush)
winter_min_rock <- lm(Min_temp~Min_temp_for_Size, data=sub_model_rock_winter)
summary(winter_min_rock)
winter_min_bush <- lm(Min_temp~Min_temp_for_Size, data=sub_model_bush_winter)
summary(winter_min_bush)
#Summer
summer_max_rock <- lm(Max_temp~Relevel(Max_temp_for_Size,'Small'), data=sub_model_rock_summer)
summary(summer_max_rock)
summer_max_bush <- lm(Max_temp~Relevel(Max_temp_for_Size,'Small'), data=sub_model_bush_summer)
summary(summer_max_bush)
summer_min_rock <- lm(Min_temp~Relevel(Min_temp_for_Size,'Medium'), data=sub_model_rock_summer)
summary(summer_min_rock)
summer_min_bush <- lm(Min_temp~Relevel(Min_temp_for_Size,'Small'), data=sub_model_bush_summer)
summary(summer_min_bush)

Compare_object <- lm(Max_temp~Max_temp_for_Object, data=Data)
summary(Compare_object)


###FIGURE 1:Comparison between temps of different sizes of shelters and Tpref across the DAY:
sub_model_rock_winter <- read.csv(file.choose(),header=T)
sub_model_rock_winter <- na.omit(sub_model_rock_winter)

d <- ggplot(sub_model_bush_winter, aes(tnum, iButton_Temp))+geom_smooth(aes(linetype=Size,color=Size))+
  geom_smooth(Data_pref_temp, mapping = aes(x = tnum , y = Mean_preferred_body_temperature), size=1, color="purple",se=TRUE)
d
d1 <- d+theme_bw()+theme(axis.text=element_text(face='bold', size=20), 
                         axis.text.x=element_text(face='bold',size=20)
                         ,axis.title.x=element_text(face='bold', size=20),
                         axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  labs(x="Time of the day", y="Ground temperature (°C)")+
  scale_y_continuous(limits = c(15, 35))+scale_x_continuous(limits = c(8, 16), labels = c("8:00AM","10:00AM","12:00PM","14:00PM","16:00PM"))
d2 <- d1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ theme(legend.position = "none")
d2

#####Code for figure 1 above###################################################################
v <- ggplot(sub_model_bush_winter, aes(tnum, iButton_Temp))+geom_smooth(aes(linetype=Size,color=Size))+
  geom_line(Data_pref_temp, mapping = aes(x = tnum , y = Mean_preferred_body_temperature), size=1, color="purple")
v
v1 <- v+theme_bw()+theme(axis.text=element_text(face='bold', size=20), 
                         axis.text.x=element_text(face='bold',size=20)
                         ,axis.title.x=element_text(face='bold', size=20),
                         axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  labs(x="Time of the day", y="Ground temperature (°C)")+
  scale_y_continuous(limits = c(30, 40))+scale_x_continuous(limits = c(8, 18), labels = c("7:30","10:00","12:30","15:00","17:30"))
v2 <- v1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ theme(legend.position = "none")
figure_winter <- ggarrange(d2, v2,
                    ncol = 2)
figure_winter


#Summer:
Data2 <- read.csv(file.choose(),header=T)
str(newer_mean_data_summer)
t2 <- mdy_hms((newer_mean_data_summer$Time_Date))
#round_date(t, "30 minutes")
newer_mean_data_summer$Month <- month(t2)
newer_mean_data_summer$tnum = hour(t2)+minute(t2)/60
newer_mean_data_summer
write.csv(Data,"C:/Users/gavin/Desktop/PhD/Chapter 2 - Thermoregulation Experiment/Data.csv", row.names = FALSE)
#mdy_hm(Time_Date)
Tpref_data_summer <- read.csv(file.choose(),header=T)
str(Tpref_data_summer)
t3 <- hm(Tpref_data_summer$Hour_of_the_day)
Tpref_data_summer$hour=hour(t3)
Tpref_data_summer$tnum=hour(t3)+minute(t3)/60
Tpref_data_summer


n <- ggplot(sub_model_rock_summer, aes(tnum, iButton_Temp))+geom_smooth(aes(linetype=Size,color=Size))+
  geom_line(Tpref_data_summer, mapping = aes(x = tnum , y = Mean_preferred_body_temperature), size=1, color="purple")
n
n1 <- n+theme_bw()+theme(axis.text=element_text(face='bold', size=20), 
                         axis.text.x=element_text(face='bold',size=20)
                         ,axis.title.x=element_text(face='bold', size=20),
                         axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  labs(x="Time of the day", y="Ground temperature (°C)")+
  scale_y_continuous(limits = c(30, 45))+scale_x_continuous(limits = c(8, 18), labels = c("7:30","10:00","12:30","15:00","17:30"))
n1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

b <- ggplot(sub_model_bush_summer, aes(tnum, iButton_Temp))+geom_smooth(aes(linetype=Size,color=Size))+
  geom_line(Tpref_data_summer, mapping = aes(x = tnum , y = Mean_preferred_body_temperature), size=1, color="purple")
b
b1 <- b+theme_bw()+theme(axis.text=element_text(face='bold', size=20), 
                         axis.text.x=element_text(face='bold',size=20)
                         ,axis.title.x=element_text(face='bold', size=20),
                         axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  labs(x="Time of the day", y="Ground temperature (°C)")+
  scale_y_continuous(limits = c(30, 45))+scale_x_continuous(limits = c(8, 18), labels = c("7:30","10:00","12:30","15:00","17:30"))
b1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

figure_final <- ggarrange(n1, b1,d1,v1,
                           ncol = 2,nrow=2)
figure_final




