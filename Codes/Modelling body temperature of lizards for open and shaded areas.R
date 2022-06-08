# Author: Ofir & Gavin

library(tidyverse)
library(humidity)

#### Set model parameters ####
alpha_L = 0.65 # thermal absoptivity, using golden spiny mice's data for the Messalina gutulatta
h_L=10.45 #convective heat transfetr ceofficient (W m-2 K-1) (Fei et al. 2012, J Ther Biol, 37: 56-64, Porter et al. 1973?)
epsilon_lizard=0.95 # emissivity of lizard's skin
K_lizard = 0.5 #thermal conductivity (W K-1 m-1)
lambda = 0.01 # lizard mean thickness in meters (diameter)
c_lizard = 3762 #specific heat capacity (J kg-1)
sigma=5.67e-8;  # Stefan-Boltzman constant (W/m^2/K^4)
dt =  60 #in seconds
mass = 1.81# lizard mass (grams) 
mass_kg=mass/1000. #in kg
A_L= 0.0314*3.14159*mass_kg^(2./3.) #surface area m2
#solar radiation
A_p = 0.4*A_L #projected lizard area for direct and scattered solar radiation

#### End of setting model parameters ####

#### function for calculating vapor pressure ####
vapor_pressure = function(RH, T){#RH - relative humidity [0-100 %]; T - air temperature [Kelvin]
  SVP = SVP(T) #return hPa 
  SVP*RH/100*100 #return Pa
}

calculate_To <- function(TAH, Tsurface, SWDOWN, GLW, shade, To){
  #browser()
  TaV = TAH
  Ts = Tsurface #ground temperature TODO: search in lizard file
  Ta = Tsurface #air temperature at lizards height - assume equals to surface temperature since the lizard is near (<3 cm) the ground
  Solar = SWDOWN # solar radiation 
  lw = GLW # longwave radiation
  ####end of load data			
  
  dQ_solar = (1-shade)*alpha_L*A_p*Solar
  
  
  #set projected lizard area for radiation from the ground
  A_downs=0.0*A_L # zero because the lizard in lying on the ground, would be 0.4*A_L if standing
  
  #set projected lizard area that contacts the ground
  A_contacts = 0.35*A_L # for standing posture, use A_contacts = 0.05*A_L
  
  #net longwave radiation
  A_up = 0.6*A_L #area of the skin facing toward the sky
  epsilon_ac= 9.2*10^-6*(Ta)^2 # (10.11) clear sky emissivity
  print(paste("solar:", dQ_solar))
  for (i in 1:(600/dt)) { # 10 minutes as the resolution of the data
    dQ_IR = epsilon_lizard*A_downs*sigma*(Ts^4. - To^4.) + epsilon_lizard*A_up*((1-shade)*lw + shade*sigma*TaV^4.) - epsilon_lizard*A_up*sigma*To^4.
    print(paste("IR:", dQ_IR))	
    #conduction
    dQ_cond = A_contacts*K_lizard*(Ts - To)/(lambda/2)
    print(paste("cond:", dQ_cond))
    #convection, assuming no wind
    Aair = 0.9*A_L # skin area that is exposed to air
    dQ_conv=h_L*Aair*(Ta-To)
    print(paste("conv:", dQ_conv))
    #Metabolism
    ew = exp(-10.0+0.51*log(mass)+0.115*(To-273)) *3 #Buckley 2008
    dQ_meta = ew/3600. #metabolic rate (j/s)
    
    dQe = (dQ_solar + dQ_IR + dQ_meta + dQ_cond + dQ_conv)
    
    dTe = dQe/((mass_kg)*c_lizard)
    print(dTe)
    To = To + dTe*dt 
    
  }
  
  To = To - 273
  To
}

get_shade_and_open_To_for_table = function(input_file, climate_change=0){
  Data <- read.csv(input_file)
  Data <- na.omit(Data)
  Data = Data[Data$Radiation_Avg>5,]
  
  #set model input
  TAH <- Data$Temperature_Avg + 273.15 #air temp near vegetation - assume equals to air temperature
  Tsurface <- Data$IR_Temp_Avg + 273.15 #surface temperature in the open
  TsurfaceShade = Data$iButton_Temp+273  #surface temperature in the shade
  SWDOWN <- Data$Radiation_Avg #downwards solar radiation
  RH = Data$RH_Avg #relative humidity
  
  #set climate change - assume air temperature, ground temperature, and shade temperature increase at the same amount
  TAH = TAH + climate_change
  Tsurface = Tsurface + climate_change
  TsurfaceShade = TsurfaceShade + climate_change
  
  #calculate sky longwave radiation
  TAIR = TAH
  SKYEMISS = 1.72*( (vapor_pressure(RH, TAIR)/1000)/TAIR)^(1/7)
  sigma=5.67e-8;  # Stefan-Boltzman constant (W/m^2/K^4)
  GLW = SKYEMISS*sigma*(TAIR)**4 #assuming clear sky
  
  #Assume initial body temperature equals 
  To = TAH
  Data$ToShade=NA
  Data$ToSun = NA
  for (irow in 1:nrow(Data)){
    Data$ToShade[irow] = calculate_To(TAH[irow], TsurfaceShade[irow], SWDOWN[irow], GLW[irow], shade=1, TAH[irow])
    Data$ToSun[irow] = calculate_To(TAH[irow], Tsurface[irow], SWDOWN[irow], GLW[irow], 0, TAH[irow])
  }
  Data$climate_change=climate_change
  return(Data)
}


summer_data_current = get_shade_and_open_To_for_table(input_file = "Stark_et_al_ELE/Data/Summer microclimate fieldata.csv")
write.table(summer_data_current, file="Summer_current_microclimate_and_operative_temperatures.csv", row.names = F, col.names = T, sep=",")

winter_data_current = get_shade_and_open_To_for_table(input_file = "Stark_et_al_ELE/Data/Winter microclimate fieldata.csv")
write.table(winter_data_current, file="Winter_current_microclimate_and_operative_temperatures.csv", row.names = F, col.names = T, sep=",")

for (i in seq(0.5,6.5, 0.5)){
  summer_data_future = get_shade_and_open_To_for_table(input_file = "Stark_et_al_ELE/Data/Summer microclimate fieldata.csv", climate_change = i)
  write.table(summer_data_future, file=paste0(i,"_Summer_future_microclimate_and_operative_temperatures.csv"), row.names = F, col.names = T, sep=",")
  winter_data_future = get_shade_and_open_To_for_table(input_file = "Stark_et_al_ELE/Data/Winter microclimate fieldata.csv", climate_change = i)
  write.table(winter_data_future, file=paste0(i,"_Winter_future_microclimate_and_operative_temperatures.csv"), row.names = F, col.names = T, sep=",")
}
