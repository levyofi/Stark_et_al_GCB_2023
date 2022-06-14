library(ggplot2)
library(readr)
library(ggpubr)
setwd("C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation/Paper/New datasets for figures")
data_summer_no_loss <- read_csv('Data/No_Loss_Summer_microhabitat_selection.csv')
data_summer_no_bush <- read_csv('Data/No_Bush_Summer_microhabitat_selection.csv')
data_summer_no_rock <- read_csv('Data/No_Rock_Summer_microhabitat_selection.csv')
data_winter_no_loss <- read_csv('Data/No_Loss_Winter_microhabitat_selection.csv')
data_winter_no_bush <- read_csv('Data/No_Bush_Winter_microhabitat_selection.csv')
data_winter_no_rock <- read_csv('Data/No_Rock_Winter_microhabitat_selection.csv')

tiff(file="Figure 4ASummer.tiff", width=3800, height=2500, res=300, compression="lzw")
summer_no_loss <- ggplot(data_summer_no_loss, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=Mean_De), alpha= 0.8)+
  scale_size_continuous(range = c(0, 12))+
  geom_line(aes(group = Active_location, color=Active_location))
summer_no_loss
summer_no_loss1 <- summer_no_loss+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(face='bold', size=25), 
                         axis.text.x=element_text(face='bold',size=25)
                         ,axis.title.x=element_text(face='bold', size=25),
                         axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Climate change scenario", breaks=seq(0, 6.5, by = 0.5))+
  scale_y_continuous(name="Percentage of time spent in location", breaks=seq(0, 90, by = 5))+
  scale_color_manual(values = c("gray26",
                                "chartreuse4","chartreuse3","chartreuse2",
                                "orangered1","azure4","azure3","azure2"))
summer_no_loss1
dev.off()

tiff(file="Figure 4CSummer.tiff", width=3800, height=2500, res=300, compression="lzw")
summer_no_bush <- ggplot(data_summer_no_bush, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=Mean_De), alpha= 0.8)+
  scale_size_continuous(range = c(0, 12))+
  geom_line(aes(group = Active_location, color=Active_location))
summer_no_bush
summer_no_bush1 <- summer_no_bush+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(face='bold', size=25),axis.text.x=element_text(face='bold',size=25)
  ,axis.title.x=element_text(face='bold', size=25),
  axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Climate change scenario", breaks=seq(0, 6.5, by = 0.5))+
  scale_y_continuous(name="Percentage of time spent in location", breaks=seq(0, 90, by = 5))+
  scale_color_manual(values = c("gray26","orangered1","azure4","azure3","azure2"))                              
summer_no_bush1
dev.off()

tiff(file="Figure 4ESummer.tiff", width=3800, height=2500, res=300, compression="lzw")
summer_no_rock <- ggplot(data_summer_no_rock, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=Mean_De), alpha= 0.8)+
  scale_size_continuous(range = c(0, 12))+
  geom_line(aes(group = Active_location, color=Active_location))
summer_no_rock
summer_no_rock1 <- summer_no_rock+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(face='bold', size=25), 
   axis.text.x=element_text(face='bold',size=25)
   ,axis.title.x=element_text(face='bold', size=25),
   axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Climate change scenario", breaks=seq(0, 6.5, by = 0.5))+
  scale_y_continuous(name="Percentage of time spent in location", breaks=seq(0, 90, by = 5))+
  scale_color_manual(values = c("gray26", "chartreuse4","chartreuse3","chartreuse2",
                                "orangered1"))
summer_no_rock1
dev.off()

tiff(file="Figure 4Bwinter.tiff", width=3800, height=2500, res=300, compression="lzw")
winter_no_loss <- ggplot(data_winter_no_loss, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=Mean_De), alpha= 0.8)+
  scale_size_continuous(range = c(0, 12))+
  geom_line(aes(group = Active_location, color=Active_location))
winter_no_loss
winter_no_loss1 <- winter_no_loss+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(face='bold', size=25), 
  axis.text.x=element_text(face='bold',size=25)
  ,axis.title.x=element_text(face='bold', size=25),
  axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Climate change scenario", breaks=seq(0, 6.5, by = 0.5))+
  scale_y_continuous(name="Percentage of time spent in location", breaks=seq(0, 90, by = 5))+
  scale_color_manual(values = c("gray26",
                                "chartreuse4","chartreuse3","chartreuse2",
                                "orangered1","azure4","azure3","azure2"))
winter_no_loss1
dev.off()


tiff(file="Figure 4Dwinter.tiff", width=3800, height=2500, res=300, compression="lzw")
winter_no_bush <- ggplot(data_winter_no_bush, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=Mean_De), alpha= 0.8)+
  scale_size_continuous(range = c(0, 12))+
  geom_line(aes(group = Active_location, color=Active_location))
winter_no_bush
winter_no_bush1 <- winter_no_bush+theme_bw()+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(face='bold', size=25), 
  axis.text.x=element_text(face='bold',size=25)
  ,axis.title.x=element_text(face='bold', size=25),
  axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Climate change scenario", breaks=seq(0, 6.5, by = 0.5))+
  scale_y_continuous(name="Percentage of time spent in location", breaks=seq(0, 90, by = 5))+
  scale_color_manual(values = c("gray26","orangered1","azure4","azure3","azure2"))
winter_no_bush1
dev.off()

tiff(file="Figure 4Fwinter.tiff", width=3800, height=2500, res=300, compression="lzw")
winter_no_rock <- ggplot(data_winter_no_rock, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=Mean_De), alpha= 0.8)+
  scale_size_continuous(range = c(0, 12))+
  geom_line(aes(group = Active_location, color=Active_location))
winter_no_rock
winter_no_rock1 <- winter_no_rock+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(face='bold', size=25), 
 axis.text.x=element_text(face='bold',size=25)
  ,axis.title.x=element_text(face='bold', size=25),
 axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Climate change scenario", breaks=seq(0, 6.5, by = 0.5))+
  scale_y_continuous(name="Percentage of time spent in location", breaks=seq(0, 90, by = 5))+
  scale_color_manual(values = c("gray26", "chartreuse4","chartreuse3","chartreuse2",
                                "orangered1"))
winter_no_rock1
dev.off()


