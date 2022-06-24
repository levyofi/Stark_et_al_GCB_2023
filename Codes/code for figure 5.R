library(ggplot2)
library(readr)
library(ggpubr)

xbreaks = seq(0, 6.5, by = 1)
xlabels = ifelse(xbreaks%%2, "", xbreaks)
ybreaks = seq(0, 100, by = 10)
ylabels = ifelse(ybreaks%%20, "", ybreaks)

summer = read.csv("Stark_et_al_ELE/Data/microhabitat_selection/Summer_microhabitat_selection.csv")
summer$Active_location = factor(summer$Active_location,
                                levels = c("Bush-Small", "Bush-Medium", "Bush-Large","Rock-Small", "Rock-Medium", "Rock-Large", "open", "burrow"),
                                labels = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open", "Burrow") )
summer$Round_Percentage = summer$Round_percentage * 100
data_summer_no_loss <- summer[summer$habitat_loss=="none",]
data_summer_no_bush <- summer[summer$habitat_loss=="Bush",]
data_summer_no_rock <- summer[summer$habitat_loss=="Rock",]
winter = read.csv("Stark_et_al_ELE/Data/microhabitat_selection/Winter_microhabitat_selection.csv")
winter$Active_location = factor(winter$Active_location,
                                levels = c("Bush-Small", "Bush-Medium", "Bush-Large","Rock-Small", "Rock-Medium", "Rock-Large", "open", "burrow"),
                                labels = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open", "Burrow") )
winter$Round_Percentage = winter$Round_percentage * 100
data_winter_no_loss <- winter[winter$habitat_loss=="none",]
data_winter_no_bush <- winter[winter$habitat_loss=="Bush",]
data_winter_no_rock <- winter[winter$habitat_loss=="Rock",]


summer_no_loss <- ggplot(data_summer_no_loss, aes(x=climate_change, y=Round_Percentage))+
  geom_line(aes(group = Active_location, color=Active_location)) +
  geom_point(aes(color=Active_location, size=mean_de), alpha= 0.8)+
  scale_size_continuous(range = c(0, 4), limits = c(0,3))
summer_no_loss
summer_no_loss1 <- summer_no_loss+theme_bw() + 
  theme_classic()+ theme(axis.text=element_text(size=12),plot.title = element_text(hjust = 0.5), legend.position="none") +ggtitle("Summer")+
  scale_x_continuous(name="Climate change (°C)", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Activity time (%)", breaks=ybreaks, labels = ylabels)+ 
  scale_color_manual(breaks = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open", "Burrow"),
    values = c("Small Bush"="chartreuse2","Medium Bush"="chartreuse3","Large Bush"="chartreuse4","Small Rock"="azure3","Medium Rock"="azure4",  "Large Rock"="grey35", "Open"="orangered1", "Burrow"="black")) 

summer_no_loss1

summer_no_bush <- ggplot(data_summer_no_bush, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=mean_de), alpha= 0.8)+
  scale_size_continuous(range = c(0, 6), limits = c(0,3))+
  geom_line(aes(group = Active_location, color=Active_location))
summer_no_bush
summer_no_bush1 <- summer_no_bush+theme_bw() + 
  theme_classic()+ theme(axis.text=element_text(size=12),legend.position="none")+
  scale_x_continuous(name="Climate change (°C)", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Activity time (%)", breaks=ybreaks, labels = ylabels)+ 
  scale_color_manual(breaks = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open", "Burrow"),
                     values = c("Small Bush"="chartreuse2","Medium Bush"="chartreuse3","Large Bush"="chartreuse4","Small Rock"="azure3","Medium Rock"="azure4",  "Large Rock"="grey35", "Open"="orangered1", "Burrow"="black")) 

summer_no_bush1

summer_no_rock <- ggplot(data_summer_no_rock, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=mean_de), alpha= 0.8)+
  scale_size_continuous(range = c(0, 6), limits = c(0,3))+
  geom_line(aes(group = Active_location, color=Active_location))
summer_no_rock
summer_no_rock1 <- summer_no_rock+theme_bw() + 
  theme_classic()+ theme(axis.text=element_text(size=12),legend.position="none")+
  scale_x_continuous(name="Climate change (°C)", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Activity time (%)", breaks=ybreaks, labels = ylabels)+ 
  scale_color_manual(breaks = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open", "Burrow"),
                     values = c("Small Bush"="chartreuse2","Medium Bush"="chartreuse3","Large Bush"="chartreuse4","Small Rock"="azure3","Medium Rock"="azure4",  "Large Rock"="grey35", "Open"="orangered1", "Burrow"="black")) 
summer_no_rock1

winter_no_loss <- ggplot(data_winter_no_loss, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=mean_de), alpha= 0.8)+
  scale_size_continuous(range = c(0, 6), limits = c(0,3))+
  geom_line(aes(group = Active_location, color=Active_location))
winter_no_loss
winter_no_loss1 <- winter_no_loss+theme_bw() + 
  theme_classic()+ theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position="none")+ggtitle("Winter")+
  scale_x_continuous(name="Climate change (°C)", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Activity time (%)", breaks=ybreaks, labels = ylabels)+ 
  scale_color_manual(breaks = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open", "Burrow"),
                     values = c("Small Bush"="chartreuse2","Medium Bush"="chartreuse3","Large Bush"="chartreuse4","Small Rock"="azure3","Medium Rock"="azure4",  "Large Rock"="grey35", "Open"="orangered1", "Burrow"="black")) 

winter_no_loss1

winter_no_bush <- ggplot(data_winter_no_bush, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=mean_de), alpha= 0.8)+
  scale_size_continuous(range = c(0, 6), limits = c(0,3))+
  geom_line(aes(group = Active_location, color=Active_location))
winter_no_bush
winter_no_bush1 <- winter_no_bush+theme_bw()+ 
  theme_classic()+ theme(axis.text=element_text(size=12),legend.position="none")+
  scale_x_continuous(name="Climate change (°C)", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Activity time (%)", breaks=ybreaks, labels = ylabels)+ 
  scale_color_manual(breaks = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open", "Burrow"),
                     values = c("Small Bush"="chartreuse2","Medium Bush"="chartreuse3","Large Bush"="chartreuse4","Small Rock"="azure3","Medium Rock"="azure4",  "Large Rock"="grey35", "Open"="orangered1", "Burrow"="black")) 
winter_no_bush1

winter_no_rock <- ggplot(data_winter_no_rock, aes(x=climate_change, y=Round_Percentage))+
  geom_point(aes(color=Active_location, size=mean_de), alpha= 0.8)+
  scale_size_continuous(range = c(0, 6), limits = c(0,3))+
  geom_line(aes(group = Active_location, color=Active_location))
winter_no_rock
winter_no_rock1 <- winter_no_rock+theme_bw() + 
  theme_classic()+ theme(axis.text=element_text(size=12),legend.position="none")+
  scale_x_continuous(name="Climate change (°C)", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Activity time (%)", breaks=ybreaks, labels = ylabels)+ 
  scale_color_manual(breaks = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open", "Burrow"),
                     values = c("Small Bush"="chartreuse2","Medium Bush"="chartreuse3","Large Bush"="chartreuse4","Small Rock"="azure3","Medium Rock"="azure4",  "Large Rock"="grey35", "Open"="orangered1", "Burrow"="black")) 

winter_no_rock1

#create a plot for the legend
p <- ggplot(data_summer_no_loss, aes(x=climate_change, y=Round_Percentage))+
  geom_line(aes(group = Active_location, color=Active_location)) +
  geom_point(aes(color=Active_location, size=mean_de), alpha= 0.8)+
  scale_size_continuous(range = c(0, 4), limits = c(0,3)) + theme_bw() + 
  theme_classic()+ theme(plot.title = element_text(hjust = 0.5), legend.position=c(0.5, 0.5), legend.box.just = "center", legend.box = "horizontal") +
  guides(
    colour = guide_legend(title = "Microhabitat", nrow = 3, title.hjust = 0.5),
    size = guide_legend(title = "DE", nrow=1, title.hjust = 0.5),
  ) + 
  scale_color_manual(c("Bush-Small", "Bush-Medium", "Bush-Large","Rock-Small", "Rock-Medium", "Rock-Large", "open", "borrow"),
                     labels = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open", "Borrow"),
                     values = c("chartreuse2","chartreuse3","chartreuse4","azure3","azure4", "grey35", "orangered1", "black")) 

color_legend <- cowplot::get_legend(p)

tiff("Figure 5.tif", width=2500, height = 2500*5/4, compression = "lzw", res=330)
ggarrange(summer_no_loss1, winter_no_loss1, summer_no_bush1, winter_no_bush1, summer_no_rock1, winter_no_rock1, 
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3, common.legend = T, legend.grob = color_legend )
dev.off()



