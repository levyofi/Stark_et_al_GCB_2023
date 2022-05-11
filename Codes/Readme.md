This are codes for the analysis, modelling and visualization of the manuscript: "Looking  for some shade? The importance of rocks compared to vegetation as a thermoregulatory refuge for desert lizards" 

# Code for Simulation Study

Jeffrey W. Doser, Andrew O. Finley, Aaron S. Weed, Elise F. Zipkin 

For questions about code, contact the first author (doserjef@msu.edu).

1. `acoustic-cov-abundance-jags.txt`: file containing the jags code necessary to run all of four models using acoustic data and point count data to estimate abundance. The specific type of model that is run is switched on and off when setting the data inputs when running JAGS through R. 
2. `main.R`: script file to run the the four abundance models in jags through R. 
3. `model-AC-main-sims.R`: file to run 100 simulations for 48 different scenarios for Model AC. 
4. `model-AVC-main-sims.R`: file to run 100 simulations for 48 different scenarios for Model AVC.
5. `model-AV-main-sims.R`: file to run 100 simulations for 48 different scenarios for Model AV.
6. `model-C-main-sims.R`: file to run 100 simulations for 48 different scenarios for Model C. 
7. `summary.R`: file to summarize simulations from 48 different scenarios across all 4 models. Includes code for Figure 2. 
8. `sim-acoustic-cov.R`: file to simulate data under any of the four models. 
9. `model-AC-main-2-sims.R`: file to run simulations shown in Figure 3. 
10. `model-AC-summary-2.R`: file to summarize simulations from `model-AC-main-2-sims.R`. Includes code for Figure 3.
11. `covariate-simulation-results-beta-0.csv`: summary file of simulation results needed to produce Figure 2.
12. `covariate-simulation-results-beta-1.csv`: summary file of simulation results needed to produce Figure S2.
13. `spatial-simulation-results.csv`: summary file of simulation results needed to produce Figure 3.
