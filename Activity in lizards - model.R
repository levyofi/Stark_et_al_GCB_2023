library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
#Upload summer data:
meterological_files <- "C:/Users/gavin/Desktop/PhD/Chapters 2&3 - Modelling Lizard's Activity Time"
setwd(meterological_files)
Data_summer <- read.csv("summer_microclimate_with_mesalina_temps 09.12.21.csv")

#Vtmean = preferred body temperature of Messalina bahaeldini:
Vtmin <- 28.66
Vtmax <- 37.30
Vtmean <- 33.39
##Calculations##:
#Check activity of lizard if it is possible in the sun 
#by calculating the Temperature distance in the sun from Vtmean:
Data_summer$active_sun <- "No"
Data_summer[Data_summer$ToSun>Vtmin & Data_summer$ToSun<Vtmax, ]$active_sun = "Yes"
Data_summer$distance_from_vtmean_sun = Data_summer$ToSun-Vtmean

#Check activity of lizard if it is possible in the shade 
#by calculating the Temperature distance in the shade from Vtmean:
Data_summer$active_shade <- "No"
Data_summer[Data_summer$ToShade>Vtmin & Data_summer$ToShade<Vtmax, ]$active_shade = "Yes"
Data_summer$distance_from_vtmean_shade = Data_summer$ToShade-Vtmean

#convert to absolute values:
Data_summer$absolute_distance_sun <- abs(Data_summer$distance_from_vtmean_sun)
Data_summer$absolute_distance_shade <- abs(Data_summer$distance_from_vtmean_shade)

#choose activity location
Data_summer$Active_location = ifelse(Data_summer$absolute_distance_sun>Data_summer$absolute_distance_shade, "shade", "open" )

######################################################################use group by=
#Check activity of lizard is possible in the rock or bush (by size) by calculating the Temperature distance of rock/bush from Vtmean:
Data_summer$active_rock_bush <- "No"
Data_summer[Data_summer$iButton_Temp>Vtmin & Data_summer$iButton_Temp<Vtmax, ]$active_rock_bush = "Yes"
Data_summer$distance_from_vtmean_rock_bush = Data_summer$iButton_Temp-Vtmean
Data_summer$absolute_distance_from_vtmean_rock_bush <- abs(Data_summer$distance_from_vtmean_rock_bush)
Data_summer <- ddply(Data_summer, .(round_dt), mutate,
               minimum_absolute_distance_from_vtmean_rock_bush = min(absolute_distance_from_vtmean_rock_bush))
#Calculate the location of the lizard, outside in the sun or in the shade or not active under rock or bush:
Data_summer<- filter(Data_summer, Data_summer$active_sun=="Yes" |  Data_summer$active_shade=="Yes" | absolute_distance_from_vtmean_rock_bush == minimum_absolute_distance_from_vtmean_rock_bush)
Data_summer = unique(Data_summer)
Data_summer$Active_location = ifelse(Data_summer$active_sun=="Yes" |  Data_summer$active_shade=="Yes", Data_summer$Active_location, paste(Data_summer$Object, Data_summer$Size, sep="-"))

write.table(Data_summer, file="summer_microclimate_with_mesalina_temps 06.04.22.csv", row.names = F, col.names = T, sep=",")

# 1) First filter night time by using solar radiation column (>2.5 day), ANALYSIS ONLY ON DAYTIME HOURS!!!.
# 2)Create table for round_dt and activity location 2) use unique function, and then use proportion code below
Data_summer <- read.csv("summer_microclimate_with_mesalina_temps 09.12.21.csv")
Data_summer_new <- Data_summer[,c("round_dt","Active_location")] 
Data_summer_new = unique(Data_summer_new)

Data_summer_prop <- prop.table(table(Data_summer_new$Active_location))
Data_summer_prop <- data.frame(Data_summer_prop)
colnames(Data_summer_prop)[1] <- "Active_location"
colnames(Data_summer_prop)[2] <- "Percentage"
Data_summer_prop$Round_percentage <- Data_summer_prop$Percentage*100

###############################################################################################################################
#Upload winter data:
library(dplyr)
library(plyr)
library(reshape2)
#Upload winter data:
meterological_files <- "C:/Users/gavin/Desktop/PhD/Chapters 2&3 - Modelling Lizard's Activity Time/new winter data for model 2022"
setwd(meterological_files)
Data_winter <- read.csv(file.choose())

#Vtmean = preferred body temperature of Messalina bahaeldini:
Vtmin <- 25.3
Vtmax <- 36.3
Vtmean <- 31.7
##Calculations##:
#Check activity of lizard if it is possible in the sun 
#by calculating the Temperature distance in the sun from Vtmean:
Data_winter$active_sun <- "No"
Data_winter[Data_winter$ToSun>Vtmin & Data_winter$ToSun<Vtmax, ]$active_sun = "Yes"
Data_winter$distance_from_vtmean_sun = Data_winter$ToSun-Vtmean

#Check activity of lizard if it is possible in the shade 
#by calculating the Temperature distance in the shade from Vtmean:
Data_winter$active_shade <- "No"
Data_winter[Data_winter$ToShade>Vtmin & Data_winter$ToShade<Vtmax, ]$active_shade = "Yes"
Data_winter$distance_from_vtmean_shade = Data_winter$ToShade-Vtmean

#convert to absolute values:
Data_winter$absolute_distance_sun <- abs(Data_winter$distance_from_vtmean_sun)
Data_winter$absolute_distance_shade <- abs(Data_winter$distance_from_vtmean_shade)

#choose activity location
Data_winter$Active_location = ifelse(Data_winter$absolute_distance_sun>Data_winter$absolute_distance_shade, "shade", "open" )

######################################################################
#Check activity of lizard is possible in the rock or bush (by size) by calculating the Temperature distance of rock/bush from Vtmean:
Data_winter$active_rock_bush <- "No"
Data_winter[Data_winter$iButton_Temp>Vtmin & Data_winter$iButton_Temp<Vtmax, ]$active_rock_bush = "Yes"
Data_winter$distance_from_vtmean_rock_bush = Data_winter$iButton_Temp-Vtmean
Data_winter$absolute_distance_from_vtmean_rock_bush <- abs(Data_winter$distance_from_vtmean_rock_bush)
Data_winter <- ddply(Data_winter, .(round_dt), mutate,
                     minimum_absolute_distance_from_vtmean_rock_bush = min(absolute_distance_from_vtmean_rock_bush))
#Calculate the location of the lizard, outside in the sun or in the shade or not active under rock or bush:
Data_winter<- filter(Data_winter, Data_winter$active_sun=="Yes" |  Data_winter$active_shade=="Yes" | absolute_distance_from_vtmean_rock_bush == minimum_absolute_distance_from_vtmean_rock_bush)
Data_winter = unique(Data_winter)
Data_winter$Active_location = ifelse(Data_winter$active_sun=="Yes" |  Data_winter$active_shade=="Yes", Data_winter$Active_location, paste(Data_winter$Object, Data_winter$Size, sep="-"))

write.table(Data_winter, file="new_winter_microclimate_with_mesalina_temps 06.04.22.csv", row.names = F, col.names = T, sep=",")
######################################################################################################################################################################################################
Data_winter <- read.csv("For figure 5 - no rocks during winter 28.3.22.csv")
Data_summer <- read.csv("For figure 5 - no rocks during summer 28.3.22.csv")

Data_winter_new <- Data_winter[,c("Time_Date","Active_location")] 
Data_summer_new = unique(Data_summer)
write.table(Data_summer_new, file="newest_summer_microclimate_with_mesalina_temps 27.03.22.csv", row.names = F, col.names = T, sep=",")

Data_winter_prop <- prop.table(table(Data_winter$Active_location))
Data_winter_prop <- data.frame(Data_winter_prop)
colnames(Data_winter_prop)[1] <- "Active_location"
colnames(Data_winter_prop)[2] <- "Percentage"
Data_winter_prop$Round_percentage <- Data_winter_prop$Percentage*100
########Preparing for figures#####################
library(miscTools)
Data_winter_prop_full <- data.frame(matrix(ncol = 2, nrow = 4))
colnames(Data_winter_prop_full) <- c('Active_location', 'Round_percentage')
rocks <- 1.812191+5.930807+5.766063
bushes <- 9.829764+25.974739+4.887424
open <- 32.454695
shade <- 13.344316
Data_winter_prop_full$Active_location <- c("Rocks", "Bushes", "Open", "shade")
Data_winter_prop_full$Round_percentage <- c(rocks,bushes,open,shade)
##################################################
#Figures:
library(lubridate)
library(ggplot2)
Data_summer <-read.csv(file.choose())
Data_winter <-read.csv(file.choose())
#Figure for time spent on/under substrate:
New_data_summer <- tidyr::separate(Data_summer, Time_Date, c("Date", "Time"), sep = " ",remove = FALSE) #separates the time and date column
str(New_data_summer)
t <- hm((New_data_summer$Time))
#round_date(t, "30 minutes")
Data$Month <- month(t)
New_data_summer$tnum = hour(t)+minute(t)/60
write.table(New_data_summer, file="newest_summer_microclimate_with_mesalina_temps 27.03.22.csv", row.names = F, col.names = T, sep=",")

d <- ggplot(Data_winter, aes(x=tnum, y=Active_location_num))+
  geom_point(aes(color=Active_location), size=5)
d
d1 <- d+theme_bw()+theme(axis.text=element_text(face='bold', size=15), 
                         axis.text.x=element_text(face='bold',size=15)
                         ,axis.title.x=element_text(face='bold', size=20),
                         axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  scale_x_continuous(name="Time of the day",breaks=seq(8,16,1),
                     labels=c("8" = "8AM","9" = "9AM",
                              "10" = "10AM","11" = "11AM", 
                              "12" = "12PM","13" = "13PM", 
                              "14" = "14PM", "15" = "15PM",
                              "16" = "16PM"))+
  scale_y_continuous(name="Microhabitat", breaks=seq(1,8,1)
                     ,labels=c("1" = "open", "2" = "shade",
                               "3" = "small-bush", "4" = "medium-bush",
                               "5" = "large-bush", "6" = "small rock",
                               "7" = "medium rock", "8" = "large rock"))+
  scale_color_manual(values = c("chartreuse4","chartreuse3","chartreuse2",
                                "orangered1", "azure4","azure3","azure2","gray26"))
d1+  theme(axis.line = element_line(color='black'),
           plot.background = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank())


p<-ggplot(data=Data_winter_prop, aes(x=Active_location, y=Round_percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Specific activity (or non activity) location of lizard")+
  scale_y_continuous("Percentage of time spent in location during winter")+
  scale_fill_manual(values = c("chartreuse4","chartreuse3","chartreuse2",
                               "orangered1", "azure4","azure3","azure2","gray26"))
p
p1 <- p+theme_bw()+theme(axis.text=element_text(face='bold', size=15), axis.text.x=element_text(size=15)
                         ,axis.title.x=element_text(face='bold', size=15),
                         axis.title.y=element_text(face='bold', size=15),
                         legend.text=element_text(size=15))
p2 <- p1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
p2

e<-ggplot(data=Data_winter_prop, aes(x=Active_location, y=Round_percentage)) +
  geom_bar(aes(fill=Active_location),stat="identity")+
  scale_x_discrete("Specific activity (or non activity) location of lizard")+
  scale_y_continuous("Percentage of time spent in location")+
  scale_fill_manual(values = c("chartreuse4","chartreuse3","chartreuse2","orangered1","gray26"))
e
e1 <- e+theme_bw()+theme(axis.text=element_text(face='bold', size=15), axis.text.x=element_text(size=15)
                         ,axis.title.x=element_text(face='bold', size=15),
                         axis.title.y=element_text(face='bold', size=15),
                         legend.text=element_text(size=15))
e2 <- e1+ theme(legend.position = "none")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
e2


library(ggplot2)
#Summer:
active_plot_ground <- ggplot(Data_summer, aes(Active_location,Ground_temp_sun)) + geom_boxplot(aes(fill = Active_location),position = position_dodge(0.9))+
  scale_x_discrete("Activity frequency in (summer) specific location")+
  scale_y_continuous("Ground temperature")
active_plot_ground
active_plot_ground1 <- active_plot_ground+theme_bw()+theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(size=10)
                                                           ,axis.title.x=element_text(face='bold', size=20),
                                                           axis.title.y=element_text(face='bold', size=20),
                                                           legend.text=element_text(size=20))
summer_ground <- active_plot_ground1+ theme(legend.position = "none")
summer_ground

active_plot_air <- ggplot(Data_summer, aes(Active_location, Temperature_Avg)) + geom_boxplot(aes(fill = Active_location),position = position_dodge(0.9))+
  scale_x_discrete("Activity frequency in (summer) specific location")+
  scale_y_continuous("Air temperature")
active_plot_air
active_plot_air1 <- active_plot_air+theme_bw()+theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(size=10)
                                                     ,axis.title.x=element_text(face='bold', size=20),
                                                     axis.title.y=element_text(face='bold', size=20),
                                                     legend.text=element_text(size=20))
summer_air <- active_plot_air1+ theme(legend.position = "none")
summer_air

#winter:
active_plot_ground2 <- ggplot(Data_winter, aes(Active_location, IR_Temp_Avg)) + geom_boxplot(aes(fill = Active_location),position = position_dodge(0.9))+
  scale_x_discrete("Activity frequency in (winter) specific location")+
  scale_y_continuous("Ground temperature")
active_plot_ground2
active_plot_ground3 <- active_plot_ground2+theme_bw()+theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(size=10)
                                                            ,axis.title.x=element_text(face='bold', size=20),
                                                            axis.title.y=element_text(face='bold', size=20),
                                                            legend.text=element_text(size=20))
winter_ground <- active_plot_ground3+ theme(legend.position = "none")
winter_ground

active_plot_air2 <- ggplot(Data_winter, aes(Active_location, Temperature_Avg)) + geom_boxplot(aes(fill = Active_location),position = position_dodge(0.9))+
  scale_x_discrete("Activity frequency in (winter) specific location")+
  scale_y_continuous("Air temperature")
active_plot_air2
active_plot_air3 <- active_plot_air2+theme_bw()+theme(axis.text=element_text(face='bold', size=20), axis.text.x=element_text(size=10)
                                                      ,axis.title.x=element_text(face='bold', size=20),
                                                      axis.title.y=element_text(face='bold', size=20),
                                                      legend.text=element_text(size=20))
winter_air <- active_plot_air3+ theme(legend.position = "none")
winter_air


