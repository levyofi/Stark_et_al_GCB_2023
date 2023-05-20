library(ggplot2)
library(plotrix)
library(lubridate)
library(dplyr)
library(ggpubr)
require(grid)   # for the textGrob() function

update_geom_defaults("bar",   list(fill = "white"))

plot_hourly = function(file, panel_letter, light=5, dark=19, mar=c(0,1,0,1), legend=FALSE){
  #light=5;dark=20
  #browser()
  #file = "microhabitat_selection_3_Summer_future_microclimate_and_operative_temperatures_all_habitats.csv"
  Data = read.csv(paste0("Data/microhabitat_selection/",file))
  #Data = Data[!is.na(Data$de),]
  Data$round_dt = ymd_hms(Data$round_dt)
  Data$hour = hour(Data$round_dt)
  Data$microhabitat = factor(Data$microhabitat, levels = c("burrow", "open", "Bush-Small", "Bush-Medium", "Bush-Large", "Rock-Small", "Rock-Medium", "Rock-Large"), ordered = T)
  colors_table = data.frame(microhabitat = c("open", "burrow", "Bush-Small", "Bush-Medium", "Bush-Large", "Rock-Small", "Rock-Medium", "Rock-Large"),
                            color = c("orangered1","brown",
                                      "chartreuse2","chartreuse3","chartreuse4",
                                      "azure3","azure4","grey20"))
  
  Data = merge(Data, colors_table, by="microhabitat", x.all=T)                      
  
  Data_summary = Data %>%
    dplyr::group_by(hour,microhabitat) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(freq = n / sum(n))
  
  Data_summary$microhabitat = factor(Data_summary$microhabitat, levels = c("open", "burrow", "Bush-Small", "Bush-Medium", "Bush-Large", "Rock-Small", "Rock-Medium", "Rock-Large"), ordered = T)
  
  Data_summary = merge(Data_summary, colors_table, by="microhabitat", x.all=T)                      
  Data_summary$microhabitat = factor(Data_summary$microhabitat, levels = c("burrow", "open", "Bush-Small", "Bush-Medium", "Bush-Large", "Rock-Small", "Rock-Medium", "Rock-Large"), ordered = T)
  p = ggplot(Data_summary, aes(hour, freq,  fill = microhabitat)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = c("Bush-Small"="chartreuse2","Bush-Medium"="chartreuse3","Bush-Large"="chartreuse4","Rock-Small"="azure2",
                                  "Rock-Medium"="azure3",  "Rock-Large"="azure4", "open"="orangered1", "burrow"="white"),
                      labels = c("open", "Small Bush","Medium Bush", "Large Bush","Small Rock", "Medium Rock",  "Large Rock"),
                      breaks = c("open", "Bush-Small", "Bush-Medium", "Bush-Large", "Rock-Small", "Rock-Medium", "Rock-Large")) +
    theme_bw()+ theme_classic()+ theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position=ifelse(legend,"top", FALSE))+ 
    theme(plot.margin = margin(0,0,0,0, "cm")) + scale_x_continuous("", breaks = light:dark, limits = c(4.5, 19.5), labels = ifelse(light:dark%%2==0, light:dark, ""))+
    scale_y_continuous(name="", breaks=0:5*20/100, labels = 0:5*20)
  
  return(p)
}


#summer 

#current climate
s1 = plot_hourly("microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_all_habitats.csv",panel_letter="(a)", mar=top)
s1 = s1 + theme(plot.title = element_text(vjust = 2))+ ggtitle('All microhabitats')
s2 = plot_hourly("microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_no_Bush.csv", ,panel_letter="(b)", mar=top)
s2 = s2 + theme(plot.title = element_text(vjust = 2))+ ggtitle('Absence of vegetation')
s3 = plot_hourly("microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(c)" , mar=top)
s3 = s3 + theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position="none", plot.tag=element_text(angle=-90, hjust = 0.5, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) + ggtitle('Absence of rocks') + labs(tag='SCurrent climate')

#climate change +6.5C
s4 = plot_hourly("microhabitat_selection_6.5_Summer_future_microclimate_and_operative_temperatures_all_habitats.csv", panel_letter="(d)", mar=middle)
s4 = s4 + ggtitle('')
s5 = plot_hourly("microhabitat_selection_6.5_Summer_future_microclimate_and_operative_temperatures_no_Bush.csv",panel_letter="(e)", mar=middle)
s5 = s5 + ggtitle('')
s6 = plot_hourly("microhabitat_selection_6.5_Summer_future_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(f)",  mar=middle)
s6 = s6 + theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position="none", plot.tag=element_text(angle=-90, hjust = 0.5, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) + ggtitle('') + labs(tag='2080-2100 (+6.5°C)')

#summer 

#current climate
w1 = plot_hourly("microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_all_habitats.csv",panel_letter="(a)", mar=top)
w1 = w1 + theme(plot.title = element_text(vjust = 2))+ ggtitle('All microhabitats')
w2 = plot_hourly("microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_no_Bush.csv", ,panel_letter="(b)", mar=top)
w2 = w2 + theme(plot.title = element_text(vjust = 2))+ ggtitle('Absence of vegetation')
w3 = plot_hourly("microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(c)" , mar=top)
w3 = w3 + theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position="none", plot.tag=element_text(angle=-90, hjust = 0.5, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) + ggtitle('Absence of rocks') + labs(tag='Current climate')

#climate change +6.5C
w4 = plot_hourly("microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_all_habitats.csv", panel_letter="(d)", mar=middle)
w4 = w4 + ggtitle('')
w5 = plot_hourly("microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_no_Bush.csv",panel_letter="(e)", mar=middle)
w5 = w5 + ggtitle('')
w6 = plot_hourly("microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(f)",  mar=middle)
w6 = w6 + theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position="none", plot.tag=element_text(angle=-90, hjust = 0.5, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) + ggtitle('') + labs(tag='2080-2100 (+4.9°C)')

wlegend = plot_hourly("microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_no_Rock.csv",panel_letter="(f)",  mar=middle, legend = TRUE)

color_legend <- get_legend(wlegend)

tiff(filename = "Figure 3 rev2.tiff", width=2500, height = 3200, compression = "lzw", res=300)
figure = ggarrange(s1, s2, s3, s4, s5, s6, w1, w2, w3, w4, w5, w6, 
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"), label.y = 0.9 ,label.x = 0.9,
          ncol = 3, nrow = 4, common.legend = T, legend.grob = color_legend, legend="top") +
  theme(plot.margin = margin(0.5,1,0.5,0.5, "cm"))  
  annotate_figure(figure, left = textGrob("Proportion of activity (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.3), x=1),
                bottom = textGrob("Time of day (hour)", gp = gpar(cex = 1.3), y=2),
                right = textGrob("Current climate                     +3°C                               +6°C", hjust = 0.55, vjust = 0.8, rot = -90, gp = gpar(cex = 1.3)))
dev.off()

summer = ggarrange(s1, s2, s3, s4, s5, s6, 
                   labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), label.y = 0.9 ,label.x = 0.9,
                   ncol = 3, nrow = 2, legend=F) +
  theme(plot.margin = margin(0.5,1,0.5,0.5, "cm"))  
summer = annotate_figure(summer, left = textGrob("Proportion of activity (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.3), x=1),
                bottom = textGrob("Time of day (hour)", gp = gpar(cex = 1.3), y=2),
                right = textGrob("Summer", hjust = 0.55, vjust = 0.8, rot = -90, gp = gpar(cex = 1.3)))
summer
winter = ggarrange(w1, w2, w3, w4, w5, w6,
                   labels = c("(g)", "(h)", "(i)", "(j)", "(k)", "(l)"), label.y = 0.9 ,label.x = 0.9,
                   ncol = 3, nrow = 2, legend=F) +
                   theme(plot.margin = margin(0.5,1,0.5,0.5, "cm"))
winter = annotate_figure(winter, left = textGrob("Proportion of activity (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.3), x=1),
                         bottom = textGrob("Time of day (hour)", gp = gpar(cex = 1.3), y=2),
                         right = textGrob("Winter", hjust = 0.55, vjust = 0.8, rot = -90, gp = gpar(cex = 1.3)))

winter

tiff(filename = "Figure 3.tiff", width=2500, height = 3200, compression = "lzw", res=300)
all = ggarrange(summer, winter, ncol = 1, nrow = 2, common.legend = T, legend.grob = color_legend, legend="top") +
  theme(plot.margin = margin(0.5,1,0.5,0.5, "cm"))
all
dev.off()

