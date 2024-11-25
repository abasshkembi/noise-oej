# noise-oej
Estimating census tract-level prevalence of workplace and transportation noise pollution in the US, and investigating racial/ethnic inequities.

--------------------------------------------------------------------------------

*Code for*

**Shkembi A**, Patel K, Smith LM, Meier HCS, Neitzel RL. Racial/ethnic inequities to noise pollution from transportation- and work-related sources in the US.

I (Abas Shkembi) conducted the analysis. For questions about this analysis, you can reach me at ashkembi@umich.edu. Keshav Patel (keshavnp@umich.edu) aided in the simulation of the workplace and transportation noise estimates.

### Code

The R scripts in this repository can be used to semi-replicate our study findings. The scripts document the steps for using for performing the workplace and transportation noise simulations at the census tract level, processing the data, running the regression models, and analyzing the results.

  * **00_helper_functions/01A_data_preprocessing.R** - provides R code to assign point data to a polygon

  * **01_simulation/final_simulation_env_noise_bystate.R** - simulation code to estimate census tract level prevalence of transportation noise exposure for states with road, rail, and aircraft noise pollution
  * **01_simulation/final_simulation_env_noise_no_rail.R** - simulation code to estimate census tract level prevalence of transportation noise exposure for states with only road and aircraft noise pollution
  * **01_simulation/final_simulation_occ_noise_bystate.R** - simulation code to estimate census tract level prevalence of workplace noise exposure for each state
  
  * **02_data_processing/00_acs2015_2019_extraction.R** - extracting relevant sociodemographic characteristics from the 2015-2019 American Community Survey using the `tigris` R package
  * **02_data_processing/01_combining data.R** - integrating the simulated data with the other data inputs

  * **03_analysis/00_descriptives.R** - descriptive statistics of the data
  * **03_analysis/01_overall_models.R** - the main models of the analysis: (1) logistic regression of the odds of high workplace and transportation noise pollution, (2) Poisson regression of the prevalence of workplace noise pollution, and (3) Poisson regression of the prevalence of transportation noise pollution
  * **03_analysis/02_redlining_model.R** - linear regression model of the average prevalence of workplace and transportation noise pollution using 1930s redlining and 1990s mortgage discrimination as the main predictors
  * **03_analysis/03_sensitivity models.R** - sensitivity analysis of the models in 01_overall_models.R after accounting for two indicators of socioeconomic status

### Data

The R scripts in this repository can be used to semi-replicate our study findings. The scripts document the steps for using for performing the workplace and transportation noise simulations at the census tract level, processing the data, running the regression models, and analyzing the results.

#### Inputs

  * **ACS_SOC_crosswalk.xlsx** - provides a crosswalk between ACS and SOC occupational codes
  * **acs2017_demos_tract.RData** - sociodemographic characteristics from the 2015-2019 American Community Survey using the tigris package. Associated code can be found in *02_data_processing/00_acs2015_2019_extraction.R*
  * **Historic Redlining Score 2010B.xlsx** - historical redlining scores by 2010 census tracts
  * **hmda_decades.rda** - Home Mortgage Disclosure Act Longitudinal Dataset by census tract, which spatially and temporally harmonized mortgage lending data since 1981, and data on household counts by race/ethnicity from the 2000 decennial Census.
  * **ruca2010revised.xlsx** - census tracts were defined as urban or non-urban using the US Department of Agriculture Rural-Urban Commuting Area Codes
  * **tract_occ_ACS_2017.csv** - census tract estimates of major occupational groups in 2017 from the 2015-2019 American Community Survey
  
#### Generated Data

  * **Final state aggregated data.zip** - contains the final estimates of workplace and transportation noise pollution prevalence by census tract. The zipped file is broken down into two folders (Transportation and Workplace), which each contain the final estimates by state stored in an .rds file
  * **final_noise_df.csv** - the final, integrated data used for analysis in .csv format, as processed in *Code/02_data_processing/01_combining_data.R*
  * **final_noise_df.RDS** - the final, integrated data used for analysis in .rds format
  * **final_noise_sf.zip** - the final, integrated data used for analysis in shapefile format
  
For detailed data of each simulation iteration for workplace and transportation noise, please contact me (Abas, ashkembi@umich.edu) to access the data. Raster files from the DOT National Transportation Noise Map are not included in this repository due to the large file size and because the DOT stores the data on their website (https://www.bts.gov/geospatial/national-transportation-noise-map).
