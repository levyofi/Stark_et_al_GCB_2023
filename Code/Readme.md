These are codes for the analysis, modelling and visualization of the study: 

# Code for Simulation Study

## Stage 1 - combining ibutton data and meteorological station data
First, we merge the ibutton data using `Summer data for ground temperature.csv` for summer ground temps or `Winter data for ground temperature.csv` with the meteorological data of summer using `Summer_microclimate.csv` or winter using `Winter_microclimate.csv`(all found in the Data folder). For this process we used the code: `Combining meterological data with ground temperature (iButton) data.R`.

## Stage 2 - calculating operative temperatures using the merged data

Second, we calculate the operative temperature of lizards in the various shaded microhabitats and in the open microhabitat using the `Modelling body temperature of lizards for open and shaded areas.R` and with the datasets `Summer microclimate fieldata.csv` for summer or `Winter microclimate fieldata.csv` for winter (found in the Data folder). This code generates the output tables of operative temperatures in the shade and open for the lizard (located in `Data/operative_temperatures`).

## Stage 3 - calculating proportions of microhabitat selection under different habitat loss and climate change scenarios

Third, we calculate the preferred microhabitat for activity (where operative temperatures are closest to the preferred temperature). If activity is not possible (too warm or too cold for activity), lizards will stay in the burrow. We use the `analyze_microhabitat_selection.R` code that runs this algorithm for all habitat loss and climate change scenarios. The code uses the tables in `Data/operative_temperatures` as input and generates output tables for each scenario (located in `Data/microhabitat_selection`).

# Other files in this folder include:
## Analysis
`Analysis for supplemantry lab vs field Tpref stats and figure.R` :Statistical comparison of field body temperatures and laboratory preferred temperatures. Code for figure S3 
`Calculate_climate_change_data.R` :Code for calculating the mean increase in summer and winter temperatures by 2100 using worldclim's published global circulation models.

## Figures and Tables
`Figure 1.R`, `Figure 2.R`, `Figure 3.R`, `Figure 4.R`, `Figure 5.R`, `Figure 6.R`
`Table 1.R`, `Table S2.R`

