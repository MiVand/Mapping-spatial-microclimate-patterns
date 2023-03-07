# Mapping-spatial-microclimate-patterns 
## project files for mapping temperature offset in forests based on LiDAR data, temperature records, and boosted regression trees in the R version 4.2.2

## included files:
### input and uutput data from the project:

#### "disturbance_year_1986-2020_epsg25832_cropped.tif"
##### - map containing the year of the most severe forest disturbance from 1986 until 2020
##### - obtained from: 
#####   Senf, Cornelius, and Rupert Seidl. 2021. 'Mapping the forest disturbance regimes of Europe', Nature Sustainability, 4: 63-70.
##### - cropped and reprojected to match the study area and raster files

#### "Forest_Plot_Info.zip
##### - Coordinates and Elevation for forest plots, including shapefile of the locations

#### "forest_types2020_epsg31464.tif" 
##### - raster file containing the current distribution of forest types (from 2021) 
##### - obtained from: 
#####   Thom, Dominik, Werner Rammer, Patrick Laux, Gerhard Smiatek, Harald Kunstmann, Sebastian Seibold, and Rupert Seidl. 2022. 'Will forest dynamics continue to accelerate throughout the 21st century in the Northern Alps?', Global Change Biology, 28: 3260-74.

#### "lidar_metrics_r12_6.csv"
##### - LiDAR based predictor variables representing forest structure
##### - used as input for the boosted regression trees
##### - standard metrics from the lidR package (stdmetrics)
##### - see also: 
#####   Roussel, J.-R.; Auty, D.; Coops, N. C.; Tompalski, P.; Goodbody, T. R. H.; Meador, A. S.; Bourdon, J.-F.; de Boissieu, F.; Achim, A. lidR: An R package for analysis of Airborne Laser Scanning (ALS) data. Remote Sensing of Environment 2020, 251, 112061. DOI: https://doi.org/10.1016/j.rse.2020.112061.

#### "temperature_data.csv"
##### - pre-processed microclimatic and macroclimatic temperatures for the study period
##### - used to calculate the temperature metrics

#### "temperature_metrics.csv"
##### - 95th percentile and mean temperature offsets calculated for the study period and for each forest plot 
##### - The metrics to be predicted by the models

#### "topography_metrics_r50.csv"
##### - LiDAR based predictor variables (based on a LiDAR derived DEM) representing topography
##### - used as input for the boosted regression trees
##### - see also: 
#####   Frey, S. J.; Hadley, A. S.; Johnson, S. L.; Schulze, M.; Jones, J. A.; Betts, M. G. Spatial models reveal the microclimatic buffering capacity of old-growth forests. Science advances 2016, 2 (4), e1501392.
#####   Hofierka, J.; Suri, M. The solar radiation model for Open source GIS: implementation and applications. In Proceedings of the Open source GIS-GRASS users ##### ##### conference, 2002; Vol. 2002, pp 51-70.

#### temperature_offset_rasters.zip: 
##### - The temperature offset maps that were produced 
 
### R scripts that were necessary for the project:
#### "1_temperature_data_processing.R" 
##### - for summarizing the microclimatic and macroclimatic temperature data into hourly values
##### - for calculating linear regression models based on weather station data to derive adiabativ lapse rates
##### - input: microclimatic and macroclimatic temperture records (not included)
##### - output: "temperature_data.csv"

#### "2_lidar_predictor_variables_r12_6.R"
##### - For calculating the forest structure predictor variables (stdmetrics from lidR)
##### - input: high resilution LiDAR data (not included)
##### - output: "lidar_metrics_r12_6.csv"

#### "3_temperature_metrics.R"
##### - to calculate the temperature metrics to be predicted by the boosted regression trees
##### - input: "temperature_data.csv"
##### - output: "temperature_metrics.csv"

#### "4_topography_predictor_variables_r50.R
##### - for calculating the topography predictor variables
##### - input: "Forest_Plot_Info.zip" and digital elevation model (not included)
##### - output: "topography_metrics_r50.csv"

#### "5_raster_stack_topographic_predictors.R"
##### - for producing the raster stack used for spatial predictions
##### - input: DEM and precalculated raster stack containing forest structure predictors (not included) 
##### - output: raster stack for spatial predictions (not included) 

#### "6_model_predictions.R"
##### - for predicting the models 

#### "7_forest_type_and_disturbance_analysis.R"
##### - statistical analysis of the influence of forest type and disturbance on temperature offset
##### - input: "temperature_offset_rasters.zip", "disturbance_year_1986-2020_epsg25832_cropped.tif", "forest_types2020_epsg31464.tif"
##### - output: violin and boxplots quantifying the influence of forest disturbance and forest type on temperature offset (see publication)
  
#### "8_Visualization_results.R"
##### - visualization of crosssvalidation: observed vs predicted values from the boosted regression trees
##### - input: "temperature_metrics.csv", "topography_metrics_r50.csv", and "lidar_metrics_r12_6.csv"
##### - output: Scatterplots visualizing observed vs predicted values (see publication)  
  

