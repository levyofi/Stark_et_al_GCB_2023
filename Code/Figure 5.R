library(dplyr)
library(plyr)
library(lubridate)
library(data.table)
library(ggplot2)
#### data for panels A and B ####
get_weighted_means = function(Data){

  Data$Activity <-ifelse(Data$Active_location!="burrow", "yes", "no")
  
  Data$habitat_loss = factor(Data$habitat_loss, levels=c("Bush", "Rock", "none"), labels = c("Bush", "Rock", "None"), ordered = T)
  Data_summary <- ddply(Data, .(habitat_loss,climate_change, Activity), summarise, 
                       activity_percentage=sum(Round_percentage), 
                       mean_activity_DE=weighted.mean(mean_DE,n_hours),
                       mean_activity_DF=weighted.mean(mean_DF,n_hours),
                       CI25DE = weighted.mean(DE.CI.25,n_hours),
                       CI975DE = weighted.mean(DE.CI97.5,n_hours),
                       CI25DF = weighted.mean(DF.CI.25,n_hours),
                       CI975DF = weighted.mean(DF.CI97.5,n_hours))
                       
  Data_summary <- filter(Data_summary, Activity == "yes")
  Data_summary$activity_Percentage <- Data_summary$activity_percentage*100
  return(Data_summary)
}


###Summer###
Data <- read.csv('Stark_et_al_GCB_revision/Data/microhabitat_selection/Summer_microhabitat_selection.csv')
Data_summer = get_weighted_means(Data)

###Winter###
Data <- read.csv('Stark_et_al_GCB_revision/Data/microhabitat_selection/Winter_microhabitat_selection.csv')
Data_winter = get_weighted_means(Data)

#### end of data for panels A and B ####


#### data for panels C to F
get_DF = function(file, season, scenario){
  data = read.csv(paste0("Stark_et_al_GCB_revision/Data/microhabitat_selection/",file))
  #browser()
  data$belowTpref = as.factor(ifelse(data$df>0, "above", "below"))
  data = data[!is.na(data$df),]
  mean_and_sd = data %>% 
    group_by(belowTpref) %>%
    summarise(mean_DF = mean(df, na.rm=T), CI_DF_25 = quantile(df, 0.25), CI_DF_75 = quantile(df, 0.75)) %>%
    ungroup()
  mean_and_sd$season=season
  mean_and_sd$scenario = scenario
  return(mean_and_sd)
}

get_season_results = function(season, habitat_to_remove = NA){
  result = list()
  #microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_no_Bush"
  file_name = paste0("microhabitat_selection_",season, "_current_microclimate_and_operative_temperatures_", ifelse(is.na(habitat_to_remove), "all_habitats", paste0("no_", habitat_to_remove)), ".csv")
  result[["current"]] = get_DF(file_name, season, 0)
  for (i in seq(0.5,6.5, 0.5)){
    file_name = paste0("microhabitat_selection_",i, "_", season, "_future_microclimate_and_operative_temperatures_", ifelse(is.na(habitat_to_remove), "all_habitats", paste0("no_", habitat_to_remove)), ".csv")
    result[[paste("change",i)]] = get_DF(file_name, season, i)
  }
  all_data = rbindlist(result)
  all_data$habitat_loss = ifelse(is.na(habitat_to_remove), "none", habitat_to_remove)
  return(all_data)
}

detach(package:plyr)

summer_dfs = get_season_results(season = "Summer")
summer_dfs = rbind(summer_dfs, get_season_results(season = "Summer", habitat_to_remove="Rock"))
summer_dfs = rbind(summer_dfs, get_season_results(season = "Summer", habitat_to_remove="Bush"))

winter_dfs = get_season_results(season = "Winter")
winter_dfs = rbind(winter_dfs, get_season_results(season = "Winter", habitat_to_remove="Rock"))
winter_dfs = rbind(winter_dfs, get_season_results(season = "Winter", habitat_to_remove="Bush"))


#### making the plot ####
xbreaks = seq(0, 6.5, by = 0.5)
xlabels = ifelse(xbreaks%%1, "", xbreaks)

#create panels
summer_activity <- ggplot(Data_summer, aes(x=climate_change, y=activity_Percentage))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")
summer1 <- summer_activity+theme_bw()+
  theme_classic()+ theme(text = element_text(size=14), axis.text=element_text(size=12), legend.position = "none", plot.title = element_text(hjust = 0.5))+#ggtitle("Summer")+
  scale_x_continuous(name="", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Activity time (%)", breaks=seq(0, 80, 10), limits = c(0,80))+
  scale_color_manual(values = c("chartreuse2", "azure3", "blue")) +
  theme(plot.margin = margin(0,0,0,0, "cm")) 

summer1

winter_activity <- ggplot(Data_winter, aes(x=climate_change, y=activity_Percentage))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")

winter1 <- winter_activity+theme_bw() + 
  theme_classic()+ theme(text=element_text(size=14), axis.text=element_text(size=12), legend.position="none", plot.title = element_text(hjust = 0.5))+#ggtitle("Winter")+
  scale_x_continuous(name="", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="", breaks=seq(0, 80, 10), limits = c(0,80))+
  scale_color_manual(values = c("chartreuse2", "azure3", "blue")) +
  theme(plot.margin = margin(0,0,0,0, "cm")) 
winter1


panel_data = summer_dfs[summer_dfs$belowTpref=="above",]
summer_df <- ggplot(panel_data, aes(x=scenario, y=mean_DF))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")
summer2 <- summer_df+theme_bw() + 
  theme_classic()+ theme(text = element_text(size=14), axis.text=element_text(size=12), legend.position = "none", legend.key.height= unit(0.5, 'cm'),
                         legend.key.width= unit(1, 'cm'))+ 
  geom_ribbon(aes(ymin = CI_DF_25, ymax = CI_DF_75, color=habitat_loss), alpha=0.02)+
  scale_x_continuous(name="", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Mean activity DE (°C)", breaks=seq(0, 3, 0.5))+
  scale_color_manual(values = c("chartreuse2","blue",  "azure3")) +
  theme(plot.margin = margin(0,0,0,0, "cm")) 
summer2

panel_data = winter_dfs[winter_dfs$belowTpref=="above",]
winter_df <- ggplot(panel_data, aes(x=scenario, y=mean_DF))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")
winter2 <- winter_df+theme_bw() + 
  theme_classic()+ theme(text = element_text(size=14), axis.text=element_text(size=12), legend.position = "none", legend.key.height= unit(0.5, 'cm'),
                         legend.key.width= unit(1, 'cm'))+ 
  geom_ribbon(aes(ymin = CI_DF_25, ymax = CI_DF_75, color=habitat_loss), alpha=0.02)+
  scale_x_continuous(name="", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="", breaks=seq(0, 3, 0.5))+
  scale_color_manual(values = c("chartreuse2","blue",  "azure3")) +
  theme(plot.margin = margin(0,0,0,0, "cm")) 
winter2

panel_data = summer_dfs[summer_dfs$belowTpref=="below",]
summer_df <- ggplot(panel_data, aes(x=scenario, y=mean_DF))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")
summer3 <- summer_df+theme_bw() + 
  theme_classic()+ theme(text = element_text(size=14), axis.text=element_text(size=12), legend.position = "none", legend.key.height= unit(0.5, 'cm'),
                         legend.key.width= unit(1, 'cm'))+ 
  geom_ribbon(aes(ymin = CI_DF_25, ymax = CI_DF_75, color=habitat_loss), alpha=0.02)+
  scale_x_continuous(name="", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="Mean activity DE (°C)", breaks=seq(0, -3, -0.5))+
  scale_color_manual(values = c("chartreuse2","blue",  "azure3")) +
  theme(plot.margin = margin(0,0,0,0, "cm")) 
summer3

panel_data = winter_dfs[winter_dfs$belowTpref=="below",]
winter_df <- ggplot(panel_data, aes(x=scenario, y=mean_DF))+
  geom_line(aes(group = habitat_loss, color=habitat_loss), size=1.5)+labs(color = "Microhabitat loss")
winter3 <- winter_df+theme_bw() + 
  theme_classic()+ theme(text = element_text(size=14), axis.text=element_text(size=12), legend.position = "none", legend.key.height= unit(0.5, 'cm'),
                         legend.key.width= unit(1, 'cm'))+ 
  geom_ribbon(aes(ymin = CI_DF_25, ymax = CI_DF_75, color=habitat_loss), alpha=0.02)+
  scale_x_continuous(name="", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="", breaks=seq(0, -3, -0.5))+
  scale_color_manual(values = c("chartreuse2","blue",  "azure3")) +
  theme(plot.margin = margin(0,0,0,0, "cm")) 
winter3

# get legend
p <- winter_df+theme_bw() + 
  theme_classic()+ theme(text = element_text(size=14), axis.text=element_text(size=12), legend.position = "top", legend.key.height= unit(0.5, 'cm'),
                         legend.key.width= unit(1, 'cm'))+ 
  geom_ribbon(aes(ymin = CI_DF_25, ymax = CI_DF_75, color=habitat_loss), alpha=0.02)+
  scale_x_continuous(name="", breaks=xbreaks, labels = xlabels)+
  scale_y_continuous(name="", breaks=seq(0, -3, -0.5))+
  scale_color_manual(values = c("chartreuse2","blue",  "azure3")) +
  theme(plot.margin = margin(0,0,0,0, "cm")) 


color_legend <- cowplot::get_legend(p)


###Figure###   activity_Percentage/mean_activity_DE
library(ggpubr)
library(grid)
tiff("Figure 5.tif", width=2500, height = 2500*5/4, compression = "lzw", res=300)
all = ggarrange(summer1, winter1, summer2, winter2, summer3, winter3,  label.x = 0.15,
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
          ncol = 2, nrow = 3) + theme(plot.margin = margin(1.5,1.5,.5,0.5, "cm"))
all = annotate_figure(all, bottom = textGrob("Temperature change (°C)", gp = gpar(cex = 1.3), y=2), 
      top = textGrob("Summer                                        Winter", gp = gpar(cex = 1.5), y=-1))
with_legend = ggarrange(all, legend.grob = color_legend )
with_legend
dev.off()

#### end of making the plot ####
