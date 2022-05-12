This are codes for the analysis, modelling and visualization of the manuscript: "CHANGE - Looking  for some shade? The importance of rocks compared to vegetation as a thermoregulatory refuge for desert lizards" 

# Code for Simulation Study

## Current climate

### Stage 1 - merging ibutton data and meteorological station data
First, we merge the ibutton data using `Summer data for ground temperature.csv` for summer ground temps or 'Winter data for ground temperature.csv` with the meteorological data of summer using 'Summer_microclimate.csv' or winter using 'Winter_microclimate.csv'. For this process we used the code: `
### Stage 2 - calculating operative temperatures using the merged data

we calculate the operative temperature of lizards in the shade and open using the `Modelling body temperature of lizards for open and shaded areas.R` and one of the `microclimate_with_mesalina_temps` csv files from the Data folder. This code generated a new table with the operative temperatures in the shade and open for the lizard.

### Stage 3 - calculating proportions of microhabitat selection

1. No habitat loss - 
2. Vegetation cover loss - 
3. Rock cover loss - 

## Climate change

### Stage 1 - simulating ibutton data and meteorological station data for the future

### Stage 2 - calculating operative temperatures using the merged data

### Stage 3 - calculating proportions of microhabitat selection

1. No habitat loss  
2. Climate change with vegetation cover loss
3. Climate change with rock cover loss

