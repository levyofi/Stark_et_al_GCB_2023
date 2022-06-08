library(dplyr)
library(stringr)
library(lubridate)
options(scipen = 999)

############Code can be applied for both summer and winter data, comment/uncomment the next lines accordingly (example below refer to winter data)############
#Season = "Winter"
Season = "Summer"

ibutton_file = paste(Season, "ground temperatures.csv")
meterological_file = paste(Season, "station data.csv")


# read ibuttons data and fix the am-pm problem in the ibutton dataset (happens if data is saved in excel):
ibutton = read.csv(paste0("Stark_et_al_ELE/Data/", ibutton_file))
ibutton = unique(ibutton) #making sure no lines are the same 
with_AM = ibutton[str_detect(ibutton$Time_Date, "M"),]
with_AM$dt = mdy_hms(with_AM$Time_Date)
without_AM = ibutton[!str_detect(ibutton$Time_Date, "M"),]
without_AM$dt = mdy_hm(without_AM$Time_Date)
fixed_ibutton = rbind(with_AM, without_AM)
fixed_ibutton = fixed_ibutton[order(fixed_ibutton$dt),]

# read meteorological data
meterological_data <- read.csv(paste0("Stark_et_al_ELE/Data/", meterological_file), sep=",", header = T)
meterological_data$dt <- ymd_hms(meterological_data$TIMESTAMP)

#merge the tables based on time:
ibutton_data = fixed_ibutton
ibutton_data$round_dt = round_date(ibutton_data$dt, "hour")
meterological_data$round_dt <- round_date(meterological_data$dt, "hour")
ibutton_meterological <- merge(ibutton_data, meterological_data, by="round_dt")

## keep only rows with cloest times
#calculate time distances
ibutton_meterological$TIMESTAMP = ymd_hms(ibutton_meterological$TIMESTAMP)
ibutton_meterological$dt_distance = abs(difftime(ibutton_meterological$dt.x,ibutton_meterological$TIMESTAMP, units="mins") )
#keep only the row with minimal time distance for every object, location, and id and half an hour
ibutton_meterological <- ddply(ibutton_meterological, .(round_dt, Object, Size, Location, id), mutate,
                     minimum_dt_distance = min(dt_distance))
ibutton_meterological = ibutton_meterological[order(ibutton_meterological$round_dt, ibutton_meterological$Object, ibutton_meterological$Size, ibutton_meterological$Location),]
ibutton_meterological_cleaned = ibutton_meterological[ibutton_meterological$minimum_dt_distance==ibutton_meterological$dt_distance,]

#sometimes there are two meteorological data rows chosen (exactly 5 minutes from an ibutton log) -let's simply choose the earlier one 
ibutton_meterological_cleaned = ddply(ibutton_meterological_cleaned, .(round_dt, Object, Size, Location, id), mutate, min_TIMESTAMP = min(TIMESTAMP))
ibutton_meterological_cleaned = ibutton_meterological_cleaned[ibutton_meterological_cleaned$TIMESTAMP==ibutton_meterological_cleaned$min_TIMESTAMP,]

# write the results to a file
write.table(ibutton_meterological_cleaned, file=paste(Season, "microclimate fieldata.csv"), row.names = F, col.names = T, sep=",")

# data sanity check - count how much data exist per microhabitat per hour - should be between 1 (if some failed or lost) to 4 (2 at each site) 
data_per_hour = ddply(ibutton_meterological_cleaned, .(round_dt, Object, Size), summarise, n_observations = length(round_dt))
write.table(data_per_hour, file=paste(Season, "data per microhabitat.csv"), row.names = F, col.names = T, sep=",")
