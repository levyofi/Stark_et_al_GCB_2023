library(lubridate)
library(ggplot2)
setwd("C:/Users/gavin/Desktop/PhD/Chapters 2&3 - Modelling Lizard's Activity Time")
data = read.csv("new_winter_microclimate_with_mesalina_temps 06.04.22.csv", header = T)
data = data[data$Date %in% c("29/01/2022", "30/01/2022", "31/01/2022"),]
data$dt = dmy_hm(data$round_dt)
data$day = difftime(data$dt,dmy("29/01/2022"),units="days") 
write.csv(data, file = paste("specific_winter_microclimate_with_mesalina_temps 6.4.2022",today(), "csv", sep="." ), row.names = FALSE)
new_data <- read.csv(file.choose())
data$Active_location <- factor(data$Active_location, 
                               levels=c("open", "shade", "large rock",
                                        "medium rock", "small rock", 
                                        "large bush","mdeium bush", "small bush"))
d <- ggplot(new_data, aes(x=day, y=Active_location_num))+
  geom_point(aes(color=Active_location), size=5)
d
d1 <- d+theme_bw()+theme(axis.text=element_text(face='bold', size=15), 
                         axis.text.x=element_text(face='bold',size=15)
                         ,axis.title.x=element_text(face='bold', size=20),
                         axis.title.y=element_text(face='bold', size=20,vjust = 1.5))+
  scale_x_continuous(name="Days",breaks=seq(0,3,0.5))+
  scale_y_continuous(name="Microhabitat", breaks=seq(1,8,1)
                     ,labels=c("1" = "open", "2" = "shade",
                               "3" = "small bush", "4" = "medium bush",
                               "5" = "large bush", "6" = "small rock"
                               ,"7" = "medium rock",
                               "8" = "large rock"))+
  scale_color_manual(values = c("chartreuse4","chartreuse3","chartreuse2",
                                "orangered1", "azure4","azure3","azure2"))
d1+  theme(axis.line = element_line(color='black'),
           plot.background = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank())
