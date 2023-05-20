library(reshape2)
library(dplyr)
library(data.table)

#winter
#Current climate

get_DF = function(file, season, scenario){
  data = read.csv(paste0("Stark_et_al_GCB_revision/Data/microhabitat_selection/",file))
  data$belowTpref = ifelse(data$df>0, "above", "below")
  data = data[!is.na(data$df),]
  mean_and_sd = data %>% 
    group_by(belowTpref) %>%
    summarise(mean_DF = mean(df, na.rm=T), sd_DF = sd(df, na.rm =T)) %>%
    ungroup()
  mean_and_sd$season=season
  mean_and_sd_for_print = dcast(setDT(mean_and_sd), season~belowTpref, value.var =c("mean_DF", "sd_DF"))
  mean_and_sd_for_print$str_below = sprintf("%.2f ± %.2f", mean_and_sd_for_print$mean_DF_below, mean_and_sd_for_print$sd_DF_below)
  mean_and_sd_for_print$str_above = sprintf("%.2f ± %.2f", mean_and_sd_for_print$mean_DF_above, mean_and_sd_for_print$sd_DF_above)
  mean_and_sd_for_print$scenario = scenario
  return(mean_and_sd_for_print)
}

# Winter - current
file = "microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_all_habitats.csv"
Winter_all = get_DF(file, season = "Winter", scenario = "all")
file = "microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_no_Bush.csv"
Winter_no_bush = get_DF(file, season = "Winter", scenario = "no bush")
file = "microhabitat_selection_Winter_current_microclimate_and_operative_temperatures_no_Rock.csv"
Winter_no_rock = get_DF(file, season = "Winter", scenario = "no rock")
Winter_current = rbind(Winter_all, Winter_no_bush, Winter_no_rock)
Winter_current$time = "current"


# Winter - future
file = "microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_all_habitats.csv"
Winter_all = get_DF(file, season = "Winter", scenario = "all")
file = "microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_no_Bush.csv"
Winter_no_bush = get_DF(file, season = "Winter", scenario = "no bush")
file = "microhabitat_selection_4.9_Winter_future_microclimate_and_operative_temperatures_no_Rock.csv"
Winter_no_rock = get_DF(file, season = "Winter", scenario = "no rock")
Winter_future = rbind(Winter_all, Winter_no_bush, Winter_no_rock)
Winter_future$time = "future"

# Summer - current
file = "microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_all_habitats.csv"
Summer_all = get_DF(file, season = "Summer", scenario = "all")
file = "microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_no_Bush.csv"
Summer_no_bush = get_DF(file, season = "Summer", scenario = "no bush")
file = "microhabitat_selection_Summer_current_microclimate_and_operative_temperatures_no_Rock.csv"
Summer_no_rock = get_DF(file, season = "Summer", scenario = "no rock")
Summer_current = rbind(Summer_all, Summer_no_bush, Summer_no_rock)
Summer_current$time = "current"


# Summer - future
file = "microhabitat_selection_6.5_Summer_future_microclimate_and_operative_temperatures_all_habitats.csv"
Summer_all = get_DF(file, season = "Summer", scenario = "all")
file = "microhabitat_selection_6.5_Summer_future_microclimate_and_operative_temperatures_no_Bush.csv"
Summer_no_bush = get_DF(file, season = "Summer", scenario = "no bush")
file = "microhabitat_selection_6.5_Summer_future_microclimate_and_operative_temperatures_no_Rock.csv"
Summer_no_rock = get_DF(file, season = "Summer", scenario = "no rock")
Summer_future = rbind(Summer_all, Summer_no_bush, Summer_no_rock)
Summer_future$time = "future"

all_data = rbind(Winter_current, Winter_future, Summer_current, Summer_future)
write.csv(all_data, file="table_1_data.csv", quote = F, row.names = F)
