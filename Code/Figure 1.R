library(ggplot2)
library(lubridate)
library(plyr)
library(magrittr)
library(ggpubr)

### Summer
#calculate Tpref per hour 
Data_summer <- read.csv("Data/Summer microclimate fieldata.csv")
Data_summer$round_dt = ymd_hms(Data_summer$round_dt)
#aggregate across all ids
shade_data = aggregate(iButton_Temp ~ round_dt+Object+Size, data = Data_summer, mean)
#aggregate across all ids
shade_data$hr = hour(ymd_hms(shade_data$round_dt))
shade_data = ddply(shade_data, .(hr,Object,Size), summarise,
                   mean_temp = mean(iButton_Temp), sd_temp = sd(iButton_Temp))
open_data = aggregate(IR_Temp_Avg ~ round_dt, data = Data_summer, mean)
open_data$hr = hour(ymd_hms(open_data$round_dt))
open_data = ddply(open_data, .(hr), summarise,
                  mean_temp = mean(IR_Temp_Avg), sd_temp = sd(IR_Temp_Avg))
open_data$Object="Open"
open_data$Size=""
summer_micro = rbind(shade_data, open_data)

#Subseting according to Rock/Bush
sub_model_rock_summer <-subset(summer_micro, Object == "Rock" | Object == "Open") 
str(sub_model_rock_summer)
sub_model_bush_summer <-subset(summer_micro, Object == "Bush" | Object == "Open") 
str(sub_model_bush_summer)

#calculate Tpref per hour 
tpref = read.csv("Data/Raw Tpref from Lab and Field Database.csv")
str(tpref)
tpref = tpref[,c(10, 15:23)]
names(tpref) = c("Season", 9:17)
tpref = reshape2::melt(tpref, id="Season", variable.name = "hr", value.name = "Tpref" )
tpref$hr = as.numeric(as.character(tpref$hr))
tpref=tpref[tpref$Season=="Summer",]
tpref_mean = aggregate(Tpref ~ hr, data = tpref, mean)
tpref$hr_jitter = tpref$hr+runif(length(tpref$hr),-0.3,0.3)


a = ggplot() + 
  geom_errorbar(data = sub_model_bush_summer, 
                aes(hr, mean_temp, ymin = mean_temp-sd_temp, ymax = mean_temp+sd_temp, color = Size),
                position = position_dodge(0.5), width = 1
  )+
  geom_point(data = sub_model_bush_summer,  aes(hr, mean_temp, color = Size), position = position_dodge(0.5)) +
  geom_line(data = sub_model_bush_summer, aes(hr, mean_temp, color = Size), position = position_dodge(0.5)) +
  scale_color_manual(values = c("orangered1","chartreuse2","chartreuse3","chartreuse4"))+theme_bw() +theme_classic() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +ggtitle("Bushes") +
  scale_x_continuous(name="Time of day (hour)", breaks=seq(5, 20, by = 1), limits = c(4.5, 20.5))+
  scale_y_continuous(name="Temperature (°C)", limits = c(10, 55))+
  geom_point(data = tpref, aes(hr_jitter, Tpref), colour="blue", size = 0.7)+
  geom_point(data = tpref_mean, aes(hr, Tpref), color="purple", size=2) 

b = ggplot() + 
  geom_errorbar(data = sub_model_rock_summer, 
                aes(hr, mean_temp, ymin = mean_temp-sd_temp, ymax = mean_temp+sd_temp, color = Size),
                position = position_dodge(0.5), width = 1
  )+
  geom_point(data = sub_model_rock_summer,  aes(hr, mean_temp, color = Size), position = position_dodge(0.5)) +
  geom_line(data = sub_model_rock_summer, aes(hr, mean_temp, color = Size), position = position_dodge(0.5)) +
  scale_color_manual(values = c("orangered1","grey20","azure4","azure3"))+theme_bw() +theme_classic()+ theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.tag=element_text(angle=-90, hjust = 0.5, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) +ggtitle("Rocks") +
  scale_x_continuous(name="Time of day (hour)", breaks=seq(5, 20, by = 1), limits = c(4.5, 20.5))+
  scale_y_continuous(name="Temperature (°C)", limits = c(10, 55))+
  geom_point(data = tpref, aes(hr_jitter, Tpref), colour="blue", size = 0.7)+
  geom_point(data = tpref_mean, aes(hr, Tpref), color="purple", size=2) +
  labs(tag='Summer')

### Winter
Data_winter <- read.csv("Data/Winter microclimate fieldata.csv")
Data_winter$round_dt = ymd_hms(Data_winter$round_dt)
#aggregate across all ids
shade_data = aggregate(iButton_Temp ~ round_dt+Object+Size, data = Data_winter, mean)
#aggregate across all ids
shade_data$hr = hour(ymd_hms(shade_data$round_dt))
shade_data = ddply(shade_data, .(hr,Object,Size), summarise,
                   mean_temp = mean(iButton_Temp), sd_temp = sd(iButton_Temp))
open_data = aggregate(IR_Temp_Avg ~ round_dt, data = Data_winter, mean)
open_data$hr = hour(ymd_hms(open_data$round_dt))
open_data = ddply(open_data, .(hr), summarise,
                  mean_temp = mean(IR_Temp_Avg), sd_temp = sd(IR_Temp_Avg))
open_data$Object="Open"
open_data$Size=""
winter_micro = rbind(shade_data, open_data)

#Subseting according to Rock/Bush
sub_model_rock_winter <-subset(winter_micro, Object == "Rock" | Object == "Open") 
str(sub_model_rock_winter)
sub_model_bush_winter <-subset(winter_micro, Object == "Bush" | Object == "Open") 
str(sub_model_bush_winter)

#calculate Tpref per hour 
tpref = read.csv("Data/Raw Tpref from Lab and Field Database.csv")
str(tpref)
tpref = tpref[,c(10, 15:23)]
names(tpref) = c("Season", 9:17)
tpref = reshape2::melt(tpref, id="Season", variable.name = "hr", value.name = "Tpref" )
tpref$hr = as.numeric(as.character(tpref$hr))
tpref=tpref[tpref$Season=="Winter",]
tpref_mean = aggregate(Tpref ~ hr, data = tpref, mean)
tpref$hr_jitter = tpref$hr+runif(length(tpref$hr),-0.3,0.3)


c = ggplot() + xlim(7.5, 18.5) + ylim(10, 55) +
  geom_errorbar(data = sub_model_bush_winter, 
                aes(hr, mean_temp, ymin = mean_temp-sd_temp, ymax = mean_temp+sd_temp, color = Size),
                position = position_dodge(0.5), width = 1
  )+
  geom_point(data = sub_model_bush_winter,  aes(hr, mean_temp, color = Size), position = position_dodge(0.5)) +
  geom_line(data = sub_model_bush_winter, aes(hr, mean_temp, color = Size), position = position_dodge(0.5)) +
  scale_color_manual(values = c("orangered1","chartreuse2","chartreuse3","chartreuse4"))+theme_bw() +theme_classic() + theme(legend.position="none", legend.box = "horizontal") +  
  scale_x_continuous(name="Time of day (hour)", breaks=seq(5, 20, by = 1), limits = c(4.5, 20.5))+
  scale_y_continuous(name="Temperature (°C)", limits = c(10, 55))+
  geom_point(data = tpref, aes(hr_jitter, Tpref), colour="blue", size = 0.7)+
  geom_point(data = tpref_mean, aes(hr, Tpref), color="purple", size=2) 

c

d = ggplot() +
  geom_errorbar(data = sub_model_rock_winter, 
                aes(hr, mean_temp, ymin = mean_temp-sd_temp, ymax = mean_temp+sd_temp, color = Size),
                position = position_dodge(0.5), width = 1
  )+
  geom_point(data = sub_model_rock_winter,  aes(hr, mean_temp, color = Size), position = position_dodge(0.5)) +
  geom_line(data = sub_model_rock_winter, aes(hr, mean_temp, color = Size), position = position_dodge(0.5)) +
  scale_color_manual(values = c("orangered1","grey20","azure4","azure3"))+theme_bw() +theme_classic()+theme(legend.position="none", plot.tag=element_text(angle=-90, hjust = 0.6, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) +
  scale_x_continuous(name="Time of day (hour)", breaks=seq(5, 20, by = 1), limits = c(4.5, 20.5))+
  scale_y_continuous(name="Temperature (°C)", limits = c(10, 55))+
  geom_point(data = tpref, aes(hr_jitter, Tpref), colour="blue", size = 0.7)+
  geom_point(data = tpref_mean, aes(hr, Tpref), color="purple", size=2) +
  labs(tag='Winter')

### put everything together
library(ggpubr)
both_habitats = rbind(sub_model_rock_winter, sub_model_bush_winter)
both_habitats$microhabitat = ifelse(both_habitats$Object!="Open", paste(both_habitats$Object, both_habitats$Size, sep="-"), "Open")
p = ggplot() +
  geom_errorbar(data = both_habitats, 
                aes(hr, mean_temp, ymin = mean_temp-sd_temp, ymax = mean_temp+sd_temp, color = microhabitat),
                position = position_dodge(0.5), width = 1
  )+
  geom_point(data = both_habitats,  aes(hr, mean_temp, color = microhabitat), position = position_dodge(0.5)) +
  geom_line(data = both_habitats, aes(hr, mean_temp, color = microhabitat), position = position_dodge(0.5)) +
  theme_bw() +theme_classic()+theme(legend.position="top") +
  guides(colour = guide_legend(title = "Microhabitat", nrow = 3, title.hjust = 0.5 ))+ 
  scale_color_manual(c("Bush-Small", "Bush-Medium", "Bush-Large","Rock-Small", "Rock-Medium", "Rock-Large", "open"),
                     labels = c("Small Bush", "Medium Bush", "Large Bush", "Small Rock", "Medium Rock", "Large Rock", "Open"),
                     values = c("chartreuse2","chartreuse3","chartreuse4","azure3","azure4", "grey35", "orangered1")) 

color_legend <- cowplot::get_legend(p)

  

tiff("Figure 1.tif", width=2400, height = 1800, compression = "lzw", res=300)
ggarrange(a, b, c, d, 
          labels = c("(a)", "(b)", "(c)", "(d)"),
          ncol = 2, nrow = 2, legend.grob = color_legend ) +
  theme(plot.margin = margin(1,1,1,1, "cm"))
dev.off()
