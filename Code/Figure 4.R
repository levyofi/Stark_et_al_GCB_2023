library(ggplot2)
library(scales)
library(magrittr)
library(dplyr)
library(reshape2)
library(data.table)

breaks = seq(0, 0.45, 0.05)
labels = breaks*100
file = "microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_all_habitats.csv"
summer_all = read.csv(paste0("Data/microhabitat_selection/",file))
summer_all$scenario = "all"
file = "microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_no_Bush.csv"
summer_no_bush = read.csv(paste0("Data/microhabitat_selection/",file))
summer_no_bush$scenario = "no bush"
file = "microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_no_Rock.csv"
summer_no_rock = read.csv(paste0("Data/microhabitat_selection/",file))
summer_no_rock$scenario = "no rock"

p_summer_current = ggplot() + 
  geom_density(data=summer_all, aes(x=df),fill="blue") + 
  geom_density(data = summer_no_bush, aes(x=df),colour="green", size=1.5, linetype="dashed") +
  geom_density(data = summer_no_rock, aes(x=df),colour="grey40", size=1.5, linetype="dotted") + 
  theme(plot.margin = margin(0,0,0,0, "cm")) +
  theme_bw() +theme_classic() + scale_x_continuous("", breaks = -4:4, limits = c(-4, 4), labels = -4:4)  +   scale_y_continuous("", labels = labels, breaks = breaks, limits = c(0,0.455)) +
  theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position="none", plot.tag=element_text(angle=-90, hjust = 0.5, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) + ggtitle('Current climate')
  
# ggplot(data = data,  aes(x=df, fill=scenario)) x`x`z`z`+
#   geom_density(alpha=0.4, na.rm = T) + scale_color_manual(values=c("blue", "grey", "green"))
#   #scale_fill_manual(values=c("blue", "NA", "NA")) +
#   theme_bw() +theme_classic() + scale_x_continuous("", breaks = -3:3, limits = c(-3, 3), labels = -3:3)  +   scale_y_continuous(labels = percent_format()) +
#   labs(fill="")
# # geom_histogram( alpha=0.6, binwidth = 0.2, aes(y = ..density..), position ="identity") + 

file = "microhabitat_selection_6.5_Summer_future_microclimate_and_operative_temperatures_all_habitats.csv"
summer_all = read.csv(paste0("Data/microhabitat_selection/",file))
summer_all$scenario = "all"
file = "microhabitat_selection_6.5_Summer_future_microclimate_and_operative_temperatures_no_Bush.csv"
summer_no_bush = read.csv(paste0("Data/microhabitat_selection/",file))
summer_no_bush$scenario = "no bush"
file = "microhabitat_selection_6.5_Summer_future_microclimate_and_operative_temperatures_no_Rock.csv"
summer_no_rock = read.csv(paste0("Data/microhabitat_selection/",file))
summer_no_rock$scenario = "no rock"

p_summer_future = ggplot() + 
  geom_density(data=summer_all, aes(x=df),fill="blue") + 
  geom_density(data = summer_no_bush, aes(x=df),colour="green", size=1.5, linetype="dashed") +
  geom_density(data = summer_no_rock, aes(x=df),colour="grey40", size=1.5, linetype="dotted") + 
  theme_bw() +theme_classic() + scale_x_continuous("", breaks = -4:4, limits = c(-4, 4), labels = -4:4)  +   scale_y_continuous("", labels = labels, breaks = breaks, limits = c(0,0.45)) +
  theme(plot.margin = margin(0,0,0,0, "cm")) +
  theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position="none", plot.tag=element_text(angle=-90, hjust = 0.5, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) + ggtitle('2080-2100 (+6.5°C)') + labs(tag='Summer')

# 
# 
# ggplot(data = data,  aes(x=df, fill=scenario)) +
#   geom_density(alpha=0.4) +
#   scale_fill_manual(values=c("chartreuse2", "azure3", "blue")) +
#   theme_bw() +theme_classic() + scale_x_continuous("", breaks = -3:3, limits = c(-3, 3), labels = -3:3)  +   scale_y_continuous(labels = percent_format()) +
#   labs(fill="")
# # geom_histogram( alpha=0.6, binwidth = 0.2, aes(y = ..density..), position ="identity") + 
# 
file = "microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_all_habitats.csv"
Winter_all = read.csv(paste0("Data/microhabitat_selection/",file))
Winter_all$scenario = "all"
file = "microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_no_Bush.csv"
Winter_no_bush = read.csv(paste0("Data/microhabitat_selection/",file))
Winter_no_bush$scenario = "no bush"
file = "microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_no_Rock.csv"
Winter_no_rock = read.csv(paste0("Data/microhabitat_selection/",file))
Winter_no_rock$scenario = "no rock"

p_winter_current = ggplot() + 
  geom_density(data=Winter_all, aes(x=df),fill="blue") + 
  geom_density(data = Winter_no_bush, aes(x=df),colour="green", size=1.5, linetype="dashed") +
  geom_density(data = Winter_no_rock, aes(x=df),colour="grey40", size=1.5, linetype="dotted") + 
  theme(plot.margin = margin(0,0,0,0, "cm")) +
  theme_bw() +theme_classic() + scale_x_continuous("", breaks = -4:4, limits = c(-4, 4), labels = -4:4)  +   scale_y_continuous("", labels = labels, breaks = breaks, limits = c(0,0.45))  +
  theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position="none", plot.tag=element_text(angle=-90, hjust = 0.5, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) + ggtitle('Current climate') 


# 
# ggplot(data = data,  aes(x=df, fill=scenario)) +
#   geom_density(alpha=0.4) +
#   scale_fill_manual(values=c("chartreuse2", "azure3", "blue")) +
#   theme_bw() +theme_classic() + scale_x_continuous("", breaks = -3:3, limits = c(-3, 3), labels = -3:3)  +   scale_y_continuous(labels = percent_format()) +
#   labs(fill="")
# # geom_histogram( alpha=0.6, binwidth = 0.2, aes(y = ..density..), position ="identity") + 
# 

file = "microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_all_habitats.csv"
Winter_all = read.csv(paste0("Data/microhabitat_selection/",file))
Winter_all$scenario = "all"
file = "microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_no_Bush.csv"
Winter_no_bush = read.csv(paste0("Data/microhabitat_selection/",file))
Winter_no_bush$scenario = "no bush"
file = "microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_no_Rock.csv"
Winter_no_rock = read.csv(paste0("Data/microhabitat_selection/",file))
Winter_no_rock$scenario = "no rock"

p_winter_future =ggplot() + 
  geom_density(data=Winter_all, aes(x=df) ,fill="blue") + 
  geom_density(data = Winter_no_bush, aes(x=df),colour="green", size=1.5, linetype="dashed") +
  geom_density(data = Winter_no_rock, aes(x=df),colour="grey40", size=1.5, linetype="dotted") + 
  theme(plot.margin = margin(0,0,0,0, "cm")) +
  theme_bw() +theme_classic() + scale_x_continuous("", breaks = -4:4, limits = c(-4, 4), labels = -4:4)  +   scale_y_continuous("",labels = labels, breaks = breaks, limits = c(0,0.45)) +
  theme(axis.text=element_text(size=12), plot.title = element_text(hjust = 0.5), legend.position="none", plot.tag=element_text(angle=-90, hjust = 0.5, vjust = 0.5), plot.tag.position=c(1.1, 0.5)) + ggtitle('2080-2100 (+4.9°C)') + labs(tag='Winter')

require(grid)   # for the textGrob() function

tiff("Figure 4.tif", width=3200, height = 2500, compression = "lzw", res=300)
summer = ggarrange(p_summer_current, p_summer_future, label.y = 0.95 ,label.x = 0.15,
          labels = c("(a)", "(b)"), ncol = 2, nrow = 1, common.legend = T) + theme(plot.margin = margin(0.5,1,0.5,0.5, "cm"))
winter = ggarrange(p_winter_current, p_winter_future, label.y = 0.95 ,label.x = 0.15,
                   labels = c("(c)", "(d)"), ncol = 2, nrow = 1, common.legend = T) + theme(plot.margin = margin(0.5,1,0.5,0.5, "cm"))
all = ggarrange(summer, winter, ncol = 1, nrow = 2)+  theme(plot.margin = margin(0.5,1,0.5,0.5, "cm"))
all = annotate_figure(all, left = textGrob("Proportion of activity (%)", rot = 90, vjust = 1, hjust = 0.5, gp = gpar(cex = 1.1), x=2),
                      bottom = textGrob("Difference from preferred body temperature (°C)", gp = gpar(cex = 1.1), y=2))
all
dev.off()


