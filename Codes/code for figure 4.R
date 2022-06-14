library(dplyr)
library(plyr)
###Summer###
setwd("C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation/Paper/New datasets for figures")
Data <- read.csv('Data/Summer_microhabitat_selection.csv')
Data$Activity <-ifelse(Data$Active_location!="burrow", "yes", "no")
Data$sumDE <- Data$mean_de*Data$n_hours

Data_summer <- ddply(Data, .(habitat_loss,climate_change,
                             Activity), summarise, 
                     activity_percentage=sum(Round_percentage), sum_DE=sum(sumDE),
                     sum_hours=sum(n_hours))
Data_summer
Data_summer$mean_activity_DE <- Data_summer$sum_DE/Data_summer$sum_hours
Data_summer <- filter(Data_summer, Activity == "yes")
Data_summer$activity_Percentage <- Data_summer$activity_percentage*100
###Figure###   activity_Percentage/mean_activity_DE
tiff(file="Figure 5ummer.tiff", width=3800, height=2500, res=300, compression="lzw")
summer <- ggplot(Data_summer, aes(x=climate_change, y=mean_activity_DE))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=2)
summer
summer1 <- summer+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(face='bold', size=25), 
        axis.text.x=element_text(face='bold',size=25)
        ,axis.title.x=element_text(face='bold', size=25),
        axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Climate change scenario", breaks=seq(0, 6.5, by = 0.5))+
  scale_y_continuous(name="Mean activity DE", breaks=seq(0, 3, 0.1))+
  scale_color_manual(values = c("chartreuse2",
                                "orangered1","azure3"))
summer1
dev.off()

########################################################################################
###Winter###
Data1 <- read.csv('Data/Winter_microhabitat_selection.csv')
Data1$Activity <-ifelse(Data1$Active_location!="burrow", "yes", "no")
Data1$sumDE <- Data1$mean_de*Data1$n_hours

Data_winter <- ddply(Data, .(habitat_loss,climate_change,
                             Activity), summarise, 
                     activity_percentage=sum(Round_percentage), sum_DE=sum(sumDE),
                     sum_hours=sum(n_hours))
Data_winter
Data_winter$mean_activity_DE <- Data_winter$sum_DE/Data_winter$sum_hours
Data_winter <- filter(Data_winter, Activity == "yes")
Data_winter$activity_Percentage <- Data_winter$activity_percentage*100
###Figure###
tiff(file="Figure 5 winter.tiff", width=3800, height=2500, res=300, compression="lzw")
winter <- ggplot(Data_winter, aes(x=climate_change, y=mean_activity_DE))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=2)
winter
winter1 <- winter+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(face='bold', size=25), 
        axis.text.x=element_text(face='bold',size=25)
        ,axis.title.x=element_text(face='bold', size=25),
        axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Climate change scenario", breaks=seq(0, 6.5, by = 0.5))+
  scale_y_continuous(name="Mean activity DE", breaks=seq(2, 4, 0.1))+
  scale_color_manual(values = c("chartreuse2",
                                "orangered1","azure3"))
winter1
dev.off()
