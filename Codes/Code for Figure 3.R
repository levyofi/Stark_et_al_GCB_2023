library(lubridate)
library(ggplot2)
library(ggpubr)
setwd("C:/Users/gavin/Desktop/PhD/Chapter 2 -Microhabitat and Thermoregulation/Modelling Lizard's Activity Time")
data = read.csv("specific_summer_July_days_12_6_22.csv", header = T) #Upload database (summer/winter)
###Preparing database###
data = data[data$Date %in% c("29/01/2022", "30/01/2022", "31/01/2022"),]
data$dt = dmy_hm(data$round_dt)
data$day = difftime(data$dt,dmy("06/07/2020"),units="days") 
write.csv(data, file = paste("specific_summer_days",today(), "csv", sep="." ), row.names = FALSE)
data_summer = read.csv("specific_summer_days.2022-06-12.csv", header = T) #Upload database (summer/winter)
data_summer$microhabitat <- factor(data_summer$microhabitat, 
                            levels=c("open", "burrow", "Rock-Large",
                                     "Rock-Medium", "Rock-Small", 
                                     "Bush-Large","Bush-Medium", "Bush-Small"))
data_winter = read.csv("specific_winter_days.2022-06-12.csv", header = T) #Upload database (summer/winter)
data_winter$microhabitat <- factor(data_winter$microhabitat, 
                                   levels=c("open", "burrow", "Rock-Large",
                                            "Rock-Medium", "Rock-Small", 
                                            "Bush-Large","Bush-Medium", "Bush-Small"))
###Create Figure###
tiff(file="Figure 3A.tiff", width=4500, height=2800, res=300, compression="lzw")
summer <- ggplot(data_summer, aes(x=day, y=microhabitat_num))+
  geom_point(aes(color=microhabitat), size=5)+
  geom_line()
  
summer
summer1 <- summer+theme_bw()+theme(axis.text=element_text(face='bold', size=25), 
                         axis.text.x=element_text(face='bold',size=25)
                         ,axis.title.x=element_text(face='bold', size=25),
                         axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Days",breaks=seq(0,3,0.5))+
  scale_y_continuous(name="Microhabitat", breaks=seq(1,8,1)
                     ,labels=c("1" = "open", "2" = "burrow",
                               "3" = "small bush", "4" = "medium bush",
                               "5" = "large bush", "6" = "small rock"
                               ,"7" = "medium rock",
                               "8" = "large rock"))+
  scale_color_manual(values = c("orangered1","gray26",
                                "azure4","azure3","azure2",
                                "chartreuse4","chartreuse3","chartreuse2"
                               ))
summer1+  theme(axis.line = element_line(color='black'),
           plot.background = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank())
dev.off()

tiff(file="Figure 3B.tiff", width=4500, height=2800, res=300, compression="lzw")
Winter <- ggplot(data_winter, aes(x=day, y=microhabitat_num))+
  geom_point(aes(color=microhabitat), size=5)+
  geom_line()

Winter
Winter1 <- Winter+theme_bw()+theme(axis.text=element_text(face='bold', size=25), 
                                   axis.text.x=element_text(face='bold',size=25)
                                   ,axis.title.x=element_text(face='bold', size=25),
                                   axis.title.y=element_text(face='bold', size=25,vjust = 1.5))+
  scale_x_continuous(name="Days",breaks=seq(0,3,0.5))+
  scale_y_continuous(name="Microhabitat", breaks=seq(1,8,1)
                     ,labels=c("1" = "open", "2" = "burrow",
                               "3" = "small bush", "4" = "medium bush",
                               "5" = "large bush", "6" = "small rock"
                               ,"7" = "medium rock",
                               "8" = "large rock"))+
  scale_color_manual(values = c("orangered1","gray26",
                                "azure4","azure3","azure2",
                                "chartreuse4","chartreuse3","chartreuse2"))
Winter1+  theme(axis.line = element_line(color='black'),
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank())


dev.off()

figure_3 <- ggarrange(summer1, Winter1,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)
figure_3