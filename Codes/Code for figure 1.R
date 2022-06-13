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
library(lubridate)
library(rayshader)
library("lme4")
library(MuMIn)
library(Rmisc)
library(Hmisc)
library(dplyr)
library(Epi)
library(statsr)
library(dplyr)
options(scipen = 999)
setwd('C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation')
Data <- read.csv(file.choose(),header=T)
Data2 <- read.csv(file.choose(),header=T)

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



#FIGURES:
###############################################################################################
#ibutton Temp vs Time:
#Winter:
mean_data <- read.csv(file.choose(),header=T)
str(mean_data)
t <- hm((mean_data$Time))
#round_date(t, "30 minutes")
mean_data$Date <- Date(t)
mean_data$tnum = hour(t)+minute(t)/60
mean_data
write.csv(Data,"C:/Users/gavin/Desktop/PhD/Chapter 2 - Thermoregulation Experiment/Data.csv", row.names = FALSE)
#mdy_hm(Time_Date)
Data_pref_temp <- read.csv(file.choose(),header=T)
str(Data_pref_temp)
t1 <- hm(Data_pref_temp$Hour_of_the_day)
Data_pref_temp$hour=hour(t1)
Data_pref_temp$tnum=hour(t1)+minute(t1)/60
Data_pref_temp
library(magrittr)
library(dplyr)
library(ggplot2)
#Comparison figure among different sizes of objects across the DAY:
sub_model_rock_winter <- read.csv(file.choose(),header=T)
sub_model_rock_winter <- na.omit(sub_model_rock_winter)
#####Code for figure 1 below###################################################################

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

scale_x_discrete(name ="Time of the day", limits=c("7:30","10:00","12:30","15:00","17:30"))


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
library(ggpubr)
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
library(ggpubr)
figure_final <- ggarrange(n1, b1,d1,v1,
                           ncol = 2,nrow=2)
figure_final





#Figure for the comparisons between rocks and bushes in different sizes:
install.packages("gridExtra")              
library("gridExtra")
Data <- read.csv(file.choose(),header=T)
Data2 <- read.csv(file.choose(),header=T)
#Winter
winter <- ggplot(Data, aes(Min_temp_for_Size, Min_temp)) + geom_boxplot(aes(fill = Min_temp_for_Object),position = position_dodge(0.9))+
  scale_x_discrete("Thermoregulatory shelter size")+
  scale_y_continuous("Minimum ground temperature in winter (°C)")+
  scale_fill_manual(breaks = c("Bush", "Rock"), 
                    values=c("green", "dimgray"))
winter
winter1 <- winter+theme_bw()+theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(size=20)
                   ,axis.title.x=element_text(face='bold', size=20),
                   axis.title.y=element_text(face='bold', size=10),
                   legend.text=element_text(size=20))
w <- winter1+ theme(legend.position = "none")
w
winter2 <- ggplot(Data2, aes(Max_temp_for_Size, Max_temp)) + geom_boxplot(aes(fill = Max_temp_for_Object),position = position_dodge(0.9))+
  scale_x_discrete("Thermoregulatory shelter size")+
  scale_y_continuous("Maximum ground temperature in winter (°C)", limits = c(15,35))+
  scale_fill_manual(breaks = c("Bush", "Rock"), 
                    values=c("chartreuse3", "dimgray"))
winter2
w <- winter2+theme_bw()+theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(size=20)
                                   ,axis.title.x=element_text(face='bold', size=20),
                                   axis.title.y=element_text(face='bold', size=10),
                                   legend.text=element_text(size=20))
w1 <- w+ theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
w1
Final_winter_plot <- grid.arrange(w, e, ncol = 2)
plot(Final_winter_plot)
#Summer
summer <- ggplot(Data2, aes(Min_temp_for_Size, Min_temp)) + geom_boxplot(aes(fill = Min_temp_for_Object),position = position_dodge(0.9))+
  scale_x_discrete("Thermoregulatory shelter size")+
  scale_y_continuous("Minimum ground temperature in summer (°C)")+
  scale_fill_manual(breaks = c("Bush", "Rock"), 
                    values=c("green", "dimgray"))
summer
summer1 <- summer+theme_bw()+theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(size=20)
                                   ,axis.title.x=element_text(face='bold', size=20),
                                   axis.title.y=element_text(face='bold', size=10),
                                   legend.text=element_text(size=20))
r <- summer1+ theme(legend.position = "none")
r
library(ggplot2)
Data2 <- read.csv(file.choose())
summer2 <- ggplot(Data2, aes(insect_type,Thermal_maxima_temperature)) + geom_boxplot(aes(fill = insect_type),position = position_dodge(0.9))+
  scale_x_discrete("Insect type")+
  scale_y_continuous("CTMAX")
summer2
summer3 <- summer2+theme_bw()+theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(size=30)
                                   ,axis.title.x=element_text(face='bold', size=20),
                                   axis.title.y=element_text(face='bold', size=20),
                                   legend.text=element_text(size=20))+
  scale_fill_manual(values = c("Black", "darkgoldenrod3"))
t <- summer3
t
+ theme(legend.position = "none")
Final_summer_plot <- grid.arrange(r, t, ncol = 2)
plot(Final_summer_plot)
Final_mega_plot <- grid.arrange(e,w1,ncol = 2,nrow=1)


#############################################################################################
#Preferred body temp vs Time:
Data1 <- read.csv(file.choose(),header=T)
str(Data1)
t <-(Data1$Time)
Data2$Time <- hour(t)
Data2$tnum = hour(t)+minute(t)/60
Data2

Data1 <- read.csv(file.choose(),header=T)
library(reshape2)
library(dplyr)
library(data.table)
library(datapasta)
library(tidyverse)

#melt(Data = Data, id.vars = "ID", measure.vars = c("blue", "red"))#

SummerNineAM <- summarise(Data1, Average = mean(c(Bt_IR_9AM), na.rm = T))
SummerTenAM <- summarise(Data1, Average = mean(c(Bt_IR_10AM), na.rm = T))
SummerElevenAM <- summarise(Data1, Average = mean(c(Bt_IR_11AM), na.rm = T))
SummerTwelvePM <- summarise(Data1, Average = mean(c(Bt_IR_12AM), na.rm = T))
SummerOnePM <- summarise(Data1, Average = mean(c(Bt_IR_13PM), na.rm = T))
SummerTwoPM <- summarise(Data1, Average = mean(c(Bt_IR_14PM), na.rm = T))
SummerThreePM <- summarise(Data1, Average = mean(c(Bt_IR_15PM), na.rm = T))
SummerFourPm <- summarise(Data1, Average = mean(c(Bt_IR_16PM), na.rm = T))
SummerFivePM <- summarise(Data1, Average = mean(c(Bt_IR_17PM), na.rm = T))
SummerHours <- rbind(SummerNineAM,SummerTenAM,SummerElevenAM,SummerTwelvePM,SummerOnePM,SummerTwoPM,SummerThreePM,SummerFourPm,SummerFivePM)
SummerHours
WinterNineAM <- summarise(Data1, Average = mean(c(Bt_IR_9AM), na.rm = T))
WinterTenAM <- summarise(Data1, Average = mean(c(Bt_IR_10AM), na.rm = T))
WinterElevenAM <- summarise(Data1, Average = mean(c(Bt_IR_11AM), na.rm = T))
WinterTwelvePM <- summarise(Data1, Average = mean(c(Bt_IR_12AM), na.rm = T))
WinterOnePM <- summarise(Data1, Average = mean(c(Bt_IR_13PM), na.rm = T))
WinterTwoPM <- summarise(Data1, Average = mean(c(Bt_IR_14PM), na.rm = T))
WinterThreePM <- summarise(Data1, Average = mean(c(Bt_IR_15PM), na.rm = T))
WinterFourPm <- summarise(Data1, Average = mean(c(Bt_IR_16PM), na.rm = T))
WinterFivePM <- summarise(Data1, Average = mean(c(Bt_IR_17PM), na.rm = T))
WinterHours <- rbind(WinterNineAM,WinterTenAM,WinterElevenAM,WinterTwelvePM,WinterOnePM,WinterTwoPM,WinterThreePM,WinterFourPm,WinterFivePM)
WinterHours
Time <- c("9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00")
names

Data <- data.frame(Time,WinterHours)
Data
colnames(Data)[1] <- "Hour_of_the_day"
colnames(Data)[2] <- "Mean_preferred_body_temperature"
Data
write.csv(Data, "C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation/Temperature preference experiment/Thermal Pictures and Data of M. bahaeldini/test24.5.csv", row.names = FALSE)
mean_data <- read.csv(file.choose())
mean_data$Hour_of_the_day_new <- factor(mean_data$Hour_of_the_day, levels=c("9:00", "10:00", "11:00","12:00","13:00","14:00","15:00","16:00"))
G <- ggplot(mean_data,aes(Hour_of_the_day_new,Mean_preferred_body_temperature,group=1))+geom_point(shape=21, color="black", fill="#69b3a2", size=6)+
  geom_line(color="gray")
G
G1 <- G+theme_bw()+theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(face='bold',size=20)
                         ,axis.title.x=element_text(face='bold', size=20),axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  scale_y_continuous(name="Preferred body temperature (°C)")+
  scale_x_discrete(name ="Time of the day")
G1
G1 
Values=c("9AM","10AM","11AM","12PM","13PM","14PM","15PM","16PM","17PM") 





###############################################################################################
#Histogram:
library(ggpubr)
Data <- read.csv(file.choose(),header=T)
Data1 <- read.csv(file.choose(),header=T)

p <- ggplot(Data,aes(Mean_Temperature))+geom_histogram(aes(y=..density..,fill=Season,color=Season, alpha=0.5),position="dodge",binwidth = 1)+
  geom_density(aes(group=Season,color=Season),alpha=0.6)
p
p1 <- p+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(size=20)
        ,axis.title.x=element_text(face='bold', size=20),
        axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  scale_x_continuous(name="Preferred body temperature (°C)",breaks=seq(25,40,1))+
  scale_y_continuous(name="Frequency")+scale_fill_manual(values = c("Orange","Blue"))+
  scale_color_manual(values = c("Orange","Blue"))
p1


+
  geom_vline(aes(xintercept=median(Mean_Temperature)),color="Purple", linetype="dashed", size=1.5)


r <- ggplot(Data1,aes(iButton_Temp))+geom_histogram(aes(fill=Object,color=Object, alpha=0.5),position="dodge",binwidth = 0.9)+
  geom_vline(aes(xintercept=median(iButton_Temp)),color="Purple", linetype="dashed", size=1.5)
r
r1 <- r+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(face='bold', size=15), axis.text.x=element_text(size=15)
        ,axis.title.x=element_text(face='bold', size=15),
        axis.title.y=element_text(face='bold', size=15,vjust = 1.5))+
  scale_x_continuous(name="ibutton Temperature (°C)",  limits = c(20,40))+
  scale_y_continuous(name="Frequency")+scale_color_manual(values = c("Green","gray"))+
  scale_fill_manual(values = c("Green","gray"))
r1
figure <- ggarrange(p1, r1,
                    labels = c("A", "B"),
                    ncol = 1)
figure
