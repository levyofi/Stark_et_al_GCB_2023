library(dplyr)
library(plyr)
###Summer###
Data <- read.csv('Stark_et_al_ELE/Data/microhabitat_selection/Summer_microhabitat_selection.csv')
Data$Activity <-ifelse(Data$Active_location!="burrow", "yes", "no")
Data$sumDE <- Data$mean_de*Data$n_hours
Data$habitat_loss = factor(Data$habitat_loss, levels=c("Bush", "Rock", "none"), labels = c("Bush", "Rock", "None"), ordered = T)

xbreaks = seq(0, 6.5, by = 0.5)
xlabels = ifelse(xbreaks%%1, "", xbreaks)

Data_summer <- ddply(Data, .(habitat_loss,climate_change,
                             Activity), summarise, 
                     activity_percentage=sum(Round_percentage), sum_DE=sum(sumDE),
                     sum_hours=sum(n_hours))
Data_summer
Data_summer$mean_activity_DE <- Data_summer$sum_DE/Data_summer$sum_hours
Data_summer <- filter(Data_summer, Activity == "yes")
Data_summer$activity_Percentage <- Data_summer$activity_percentage*100

###Winter###
Data1 <- read.csv('Stark_et_al_ELE/Data/microhabitat_selection/Winter_microhabitat_selection.csv')
Data1$Activity <-ifelse(Data1$Active_location!="burrow", "yes", "no")
Data1$sumDE <- Data1$mean_de*Data1$n_hours
Data1$habitat_loss = factor(Data1$habitat_loss, levels=c("Bush", "Rock", "none"), labels = c("Bush", "Rock", "None"), ordered = T)

Data_winter <- ddply(Data1, .(habitat_loss,climate_change,
                             Activity), summarise, 
                     activity_percentage=sum(Round_percentage), sum_DE=sum(sumDE),
                     sum_hours=sum(n_hours))
Data_winter
Data_winter$mean_activity_DE <- Data_winter$sum_DE/Data_winter$sum_hours
Data_winter <- filter(Data_winter, Activity == "yes")
Data_winter$activity_Percentage <- Data_winter$activity_percentage*100

#create panels
summer_activity <- ggplot(Data_summer, aes(x=climate_change, y=activity_Percentage))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")
summer1 <- summer_activity+theme_bw()+
  theme_classic()+ theme(axis.text=element_text(size=12), legend.position = "none", plot.title = element_text(hjust = 0.5))+ggtitle("Summer")+
  scale_x_continuous(name="Climate change (°C)", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Activity time (%)", breaks=seq(0, 100, 10))+
  scale_color_manual(values = c("chartreuse2", "azure3", "blue"))
#summer1

winter_activity <- ggplot(Data_winter, aes(x=climate_change, y=activity_Percentage))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")
winter1 <- winter_activity+theme_bw() + 
  theme_classic()+ theme(axis.text=element_text(size=12), legend.position="none", plot.title = element_text(hjust = 0.5))+ggtitle("Winter")+
  scale_x_continuous(name="Climate change (°C)", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Activity time (%)", breaks=seq(0, 100, 10))+
  scale_color_manual(values = c("chartreuse2", "azure3", "blue"))
winter1

summer_de <- ggplot(Data_summer, aes(x=activity_Percentage, y=mean_activity_DE))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")
summer2 <- summer_de+theme_bw() + 
  theme_classic()+ theme(axis.text=element_text(size=12), legend.position = c(0.8, 0.8), legend.key.height= unit(0.5, 'cm'),
                         legend.key.width= unit(1, 'cm'))+ 
  scale_x_continuous(name="Activity time (%)", breaks=seq(0, 100, 10))+
  scale_y_continuous(name="Mean activity DE (°C)", breaks=seq(0, 2, 0.2))+
  scale_color_manual(values = c("chartreuse2", "azure3", "blue"))
#summer2

winter_de <- ggplot(Data_winter, aes(x=activity_Percentage, y=mean_activity_DE))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")
winter2 <- winter_de+theme_bw() + 
  theme_classic()+ theme(axis.text=element_text(size=12), legend.position = "none")+
  scale_x_continuous(name="Activity time (%)", breaks=seq(0, 100, 10))+
  scale_y_continuous(name="Mean activity DE (°C)", breaks=seq(0, 2, 0.2))+
  scale_color_manual(values = c("chartreuse2", "azure3", "blue"))
winter2

###Figure###   activity_Percentage/mean_activity_DE
library(ggpubr)

tiff("Figure 4.tif", width=2500, height = 2500*3/4, compression = "lzw", res=330)
ggarrange(summer1, winter1, summer2, winter2, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
dev.off()
