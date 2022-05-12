These are codes for the analysis, modelling and visualization of the study: "" 

# Code for Simulation Study

## Current climate

### Stage 1 - merging ibutton data and meteorological station data
First, we merge the ibutton data using `Summer data for ground temperature.csv` for summer ground temps or `Winter data for ground temperature.csv` with the meteorological data of summer using `Summer_microclimate.csv` or winter using `Winter_microclimate.csv`. For this process we used the code: `Combining meterological data with iButton data.R`.
### Stage 2 - calculating operative temperatures using the merged data

We calculate the operative temperature of lizards in the shade and open using the `Modelling body temperature of lizards for open and shaded areas.R` and with the datasets `combined_summer_microclimate_and_ground_temps 12.05.2022.csv` for summer or `combined_winter_microclimate_and_ground_temps 12.05.2022.csv` for winter. This code generated a new table with the operative temperatures in the shade and open for the lizard.

### Stage 3 - calculating proportions of microhabitat selection

1. No habitat loss - We used the newly generated datasets `summer_microclimate_with_mesalina_temps 11.05.2022.csv` and `new_winter_microclimate_with_mesalina_temps 11.05.2022.csv` and the code `Modelling activity and shelter choice of a desert lizard.R`.  
2. Vegetation cover loss - In the end of the above code, there are two lines of codes
3. Rock cover loss - 

## Climate change

### Stage 1 - simulating ibutton data and meteorological station data for the future

### Stage 2 - calculating operative temperatures using the merged data

### Stage 3 - calculating proportions of microhabitat selection

1. No habitat loss  
2. Climate change with vegetation cover loss
3. Climate change with rock cover loss

