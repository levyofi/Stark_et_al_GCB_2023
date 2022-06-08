library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(lubridate)
library(data.table)
options(width=500)

calculate_proportions_of_microhabitat_selection = function(input_file, Vtmin, Vtmax, Vtmean, season, climate_change, habitat_to_remove = NA){
  Data_season = read.csv(input_file)
  #### Stage 1 - implement habitat loss if applicable
  if (!is.na(habitat_to_remove)){
    Data_season = Data_season[Data_season$Object!=habitat_to_remove,]
  }
  #### end of stage 1
  
  #### Stage 2 - Convert to hourly resolution - averaging between the location id's
  Data_season_agg <- ddply(Data_season, .(round_dt, Object, Size), summarise,
                           avg_ToSun = mean(ToSun), avg_ToShade = mean(ToShade), avg_ToBoulder = mean(iButton_Temp), avg_Tair = mean(Temperature_Avg), avg_Tsurface_open = mean(IR_Temp_Avg), avg_radiation = mean(Radiation_Avg))
  #### end of stage 2
  
  #### Stage 3 - Calculate the preferred microhabitat for each hour
  #Check activity of lizard if it is possible in the sun 
  #by calculating the Temperature distance in the sun from Vtmean:
  #  browser()
  Data_season_agg$active_sun <- "No"
  Data_season_agg[Data_season_agg$avg_ToSun>Vtmin & Data_season_agg$avg_ToSun<Vtmax, ]$active_sun = "Yes"
  Data_season_agg$distance_from_vtmean_sun = Data_season_agg$avg_ToSun-Vtmean
  
  #Check whether activity is possible in the shade AND AT WHAT OBJECT AND SIZE 
  #by calculating the Temperature distance in the shade from Vtmean:
  Data_season_agg$active_shade <- "No"
  Data_season_agg$active_shade = ifelse(Data_season_agg$avg_ToShade>Vtmin & Data_season_agg$avg_ToShade<Vtmax, paste(Data_season_agg$Object, Data_season_agg$Size, sep="-"), "No")
  Data_season_agg$distance_from_vtmean_shade = Data_season_agg$avg_ToShade-Vtmean
  
  #convert to absolute values:
  Data_season_agg$absolute_distance_sun <- abs(Data_season_agg$distance_from_vtmean_sun)
  Data_season_agg$absolute_distance_shade <- abs(Data_season_agg$distance_from_vtmean_shade)
  
  #for each time point, find the location with minimum distance to Tvmean
  Data_season_agg <- ddply(Data_season_agg, .(round_dt), mutate,
                           minimum_absolute_distance_shade = min(absolute_distance_shade), minimum_absolute_distance_open = min(absolute_distance_sun))
  # order the data by time, shade element (object), and size
  #browser()
  Data_season_agg = Data_season_agg[order(Data_season_agg$round_dt, Data_season_agg$Object, Data_season_agg$Size),]
  
  # loop through the data and select the best location for each time point
  data_microhabitat = data.frame(round_dt = unique(Data_season_agg$round_dt), microhabitat=NA, avg_Tsurface_open=NA, avg_ToSun = NA, avg_ToShade = NA, avg_radiation = NA, de=NA)
  print(nrow(data_microhabitat))
  for (idt in 1:nrow(data_microhabitat)){
    # by default - we set the location to "burrow"
    #browser()
    activity_microhabitat = "burrow"
    de=NA # thermal accuracy
    # get the current row
    current_dt = filter(Data_season_agg, round_dt ==data_microhabitat[idt,]$round_dt)
    #check if activity is possible in the sun
    if ("Yes" %in% current_dt$active_sun){
      activity_microhabitat = "open"
      de = current_dt$minimum_absolute_distance_open[1]
    }
    # get the best shade location
    best_shade_location = filter(current_dt, abs(minimum_absolute_distance_shade-absolute_distance_shade)<0.000001)
    if (nrow(best_shade_location)>1){ # only one result should return, so if we have more than 1 we need to check the code
      #Choose the smallest habitat
      if ("Small" %in% best_shade_location$Size) {
        best_shade_location = best_shade_location[best_shade_location$Size=="Small",]
      } else if ("Medium" %in% best_shade_location$Size){
        best_shade_location = best_shade_location[best_shade_location$Size=="Medium",]
      } 
      best_shade_location = best_shade_location[1,]
    }
    #choose the best microhabitat between the best shade location and the sun, but only if the shade conditions enable activity
    if (best_shade_location$absolute_distance_shade< min(current_dt$absolute_distance_sun) & best_shade_location$active_shade!="No"){
      activity_microhabitat = best_shade_location$active_shade
      de = current_dt$minimum_absolute_distance_shade[1]
    }
    #add the result to the new table
    # if (nrow(current_dt)<6) {
    #   print(paste("warning! not all microhabitats are present in this hour"))
    # }
    data_microhabitat[idt,]$microhabitat=activity_microhabitat
    data_microhabitat[idt,]$avg_Tsurface_open=mean(current_dt$avg_Tsurface_open)
    data_microhabitat[idt,]$avg_ToSun=mean(current_dt$avg_ToSun)
    data_microhabitat[idt,]$avg_ToShade=mean(current_dt$avg_ToShade)
    data_microhabitat[idt,]$avg_radiation=mean(current_dt$avg_radiation)
    data_microhabitat[idt,]$de = de
    #print(data_microhabitat[idt,])
    #print(paste("selected habitat:", activity_microhabitat))
  }
  # write results to a table - will be used for figures
  data_microhabitat$season=season
  data_microhabitat$climate_change  = climate_change
  write.table(data_microhabitat, file = paste0("microhabitat_selection_",basename(input_file)), row.names = F, col.names = T, sep=",")
  data_microhabitat$one=1
  #### Stage 4 - Calculate the proportions of preference for each microhabitat 
  Data_season_prop = ddply(data_microhabitat, .(microhabitat), summarise,
                           avg_ToSun = mean(avg_ToSun), avg_ToShade = mean(avg_ToShade), avg_Tsurface_open = mean(avg_Tsurface_open), avg_radiation = mean(avg_radiation), mean_de = mean(de, na.rm=T), 
                           n_hours = sum(one))
  Data_season_prop$Round_percentage = Data_season_prop$n_hours/sum(Data_season_prop$n_hours)
  colnames(Data_season_prop)[1] <- "Active_location"
  Data_season_prop$season = season
  Data_season_prop$habitat_loss = ifelse(is.na(habitat_to_remove), "none", habitat_to_remove)
  Data_season_prop$climate_change  = climate_change
  return(Data_season_prop)
}


get_file_results = function(input_file, Vtmin, Vtmax, Vtmean, season, climate_change){
  result = list()
  result[["prop_no_habitat_loss"]] <- calculate_proportions_of_microhabitat_selection(input_file, Vtmin, Vtmax, Vtmean, season, climate_change)
  result[["prop_no_rocks"]] <- calculate_proportions_of_microhabitat_selection(input_file, Vtmin, Vtmax, Vtmean, season, climate_change, habitat_to_remove = "Rock")
  result[["prop_no_bushes"]] <- calculate_proportions_of_microhabitat_selection(input_file, Vtmin, Vtmax, Vtmean, season, climate_change, habitat_to_remove = "Bush")
  return(rbindlist(result))
}

get_season_results = function(Vtmin, Vtmax, Vtmean, season){
  result = list()
  result[["current"]] = get_file_results(paste0("Stark_et_al_ELE/Data/operative_temperatures/", season,"_current_microclimate_and_operative_temperatures.csv"), Vtmin, Vtmax, Vtmean, season, 0)
  for (i in seq(0.5,6.5, 0.5)){
    result[[paste("change",i)]] = get_file_results(paste0("Stark_et_al_ELE/Data/operative_temperatures/", paste(i, season, "future_microclimate_and_operative_temperatures.csv", sep="_")), Vtmin, Vtmax, Vtmean, season, i)
  }
  return(rbindlist(result))
}

summer_results = get_season_results(Vtmin = 28.66, Vtmax = 37.30, Vtmean = 33.39, season = "Summer")
write.table(summer_results, file="Summer_microhabitat_selection.csv", row.names = F, col.names = T, sep=",")

winter_results = get_season_results(Vtmin = 25.3, Vtmax = 36.3, Vtmean = 31.7, season = "Winter")
write.table(winter_results, file="Winter_microhabitat_selection.csv", row.names = F, col.names = T, sep=",")
