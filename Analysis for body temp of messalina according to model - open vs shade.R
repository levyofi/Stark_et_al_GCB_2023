# Author: Ofir & Gavin
library(tidyverse)
###############################################################################
Data <- read.csv("new_winter_microclimate_iButton.csv")
Data <- read.csv("summer_microclimate_all.csv")
Data <- na.omit(Data)

#run only on rock, assuming the temperature represent the temperature in the shade
#Data1 = Data1[Data1$Object=="Rock" & Data1$Size=="Medium",]
#print(Data1)
alpha_L = 0.65 # thermal absoptivity, using golden spiny mice's data for the Messalina gutulatta
h_L=10.45 #convective heat transfetr ceofficient (W m-2 K-1) (Fei et al. 2012, J Ther Biol, 37: 56-64, Porter et al. 1973?)
epsilon_lizard=0.95 # emissivity of lizard's skin
K_lizard = 0.5 #thermal conductivity (W K-1 m-1)
lambda = 0.01 # lizard mean thickness in meters (diameter)
c_lizard = 3762 #specific heat capacity (J kg-1)
dt =  60 #in seconds
		
####load lizard's data
mass = 1.81# lizard mass (grams) 
mass_kg=mass/1000. #in kg
A_L= 0.0314*3.14159*mass_kg^(2./3.) #surface area m2
#solar radiation
A_p = 0.4*A_L #projected lizard area for direct and scattered solar radiation

TAH <- Data$Temperature_Avg + 273.15 #air temp near vegetation
Tsurface <- Data$IR_Temp_Avg + 273.15
TsurfaceShade = Data$iButton_Temp+273
SWDOWN <- Data$Radiation_Avg
RH = Data$RH_Avg

library(humidity)

vapor_pressure = function(RH, T){#RH - relative humidity [0-100 %]; T - air temperature [Kelvin]
  SVP = SVP(T) #return hPa 
  SVP*RH/100*100 #return Pa
}

#calculate sky longwave radiation
TAIR = TAH
SKYEMISS = 1.72*( (vapor_pressure(RH, TAIR)/1000)/TAIR)^(1/7)
sigma=5.67e-8;  # Stefan-Boltzman constant (W/m^2/K^4)
GLW = SKYEMISS*sigma*(TAIR)**4 #assuming clear sky
To = TAH
calculate_To <- function(TAH, Tsurface, SWDOWN, GLW, shade, To){
	#browser()
  TaV = TAH
	Ts = Tsurface #ground temperature TODO: search in lizard file
	Ta = Tsurface #air temperature at lizards height 
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
Data$ToShade=NA
Data$ToSun = NA
for (irow in 1:nrow(Data)){
Data$ToShade[irow] = calculate_To(TAH[irow], TsurfaceShade[irow], SWDOWN[irow], GLW[irow], shade=1, TAH[irow])
Data$ToSun[irow] = calculate_To(TAH[irow], Tsurface[irow], SWDOWN[irow], GLW[irow], 0, TAH[irow])
}
write.table(Data, file="New_winter_microclimate_with_mesalina_temps.csv", row.names = F, col.names = T, sep=",")

###########################################################################################################################
library(lubridate)
ymd_hms(Data1$round_dt)
Data1$julian_day = yday(Data1$round_dt)
Data1$numeric_hour = Data1$julian_day+hour(Data1$round_dt)/24+minute(Data1$round_dt)/(60*24)

Data1 = Data1[Data1$numeric_hour>230,]

plot(Data1$ToSun~Data1$numeric_hour, col="red")
points(Data1$Temperature_Avg~Data1$numeric_hour, col="green")
points(Data1$iButton_Temp~Data1$numeric_hour)
points(Data1$ToShade~Data1$numeric_hour, col="blue")

Vtmin <- 28.66
Vtmax <- 37.30
Vtmean <- 33.39

Pinsect_shade = 0
Pinsect_open = 0
#nocturnal and diurnal
for (irow in 1:nrow(Data1)){
  if (between(Data1$ToShade[irow], Vtmin, Vtmax))
    Pinsect_shade = Pinsect_shade + 1
  if (between(Data1$ToSun[irow], Vtmin, Vtmax))
    Pinsect_open = Pinsect_open + 1 
}
Pshade = Pinsect_shade/(Pinsect_shade+Pinsect_open)
Popen = 1-Pshade 
print(paste(Popen, Pshade))

#diurnal - cold adapted
Pinsect_shade = 0
Pinsect_open = 0
for (irow in 1:nrow(Data1)){
  if (Data1$Radiation_Avg[irow]>50){
    if (between(Data1$ToShade[irow], Vtmin+2, Vtmax-2))
      Pinsect_shade = Pinsect_shade + 1
    if (between(Data1$ToSun[irow], Vtmin+2, Vtmax-2))
      Pinsect_open = Pinsect_open + 1 
  }
}
Pshade = Pinsect_shade/(Pinsect_shade+Pinsect_open)
Popen = 1-Pshade 
print(paste(Popen, Pshade))

#diurnal - cold adapted
Pinsect_shade = 0
Pinsect_open = 0
warm_adapted = 3
for (irow in 1:nrow(Data1)){
  if (Data1$Radiation_Avg[irow]>50){
    if (between(Data1$ToShade[irow], Vtmin + warm_adapted, Vtmax+ warm_adapted ))
      Pinsect_shade = Pinsect_shade + 1
    if (between(Data1$ToSun[irow], Vtmin+ warm_adapted, Vtmax+ warm_adapted ))
      Pinsect_open = Pinsect_open + 1 
  }
}
Pshade = Pinsect_shade/(Pinsect_shade+Pinsect_open)
Popen = 1-Pshade 
print(paste(Popen, Pshade))

plot(Data2$ToSun-Data2$IR_Temp_Avg)

#conclusions - nocturnal and diurnal 50%/50%
#diurnal cold-adapted 33% open/66% cover
#diurnal warm adapted +5 40% open /60% cover

#cold adapted-narrow 

