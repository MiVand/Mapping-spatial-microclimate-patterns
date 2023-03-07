### note: the digital elevation model and r.sun output rasters necessary for running this script are not available
# however the output-file ("topography_metrics_r50.csv") is included

library(raster)
library(rgdal)
library(exactextractr)
library(sf)
library(rstudioapi)

## working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# we need information on the forest plots to extract information on the coordinates
plot_info <- read.csv("Forest_Plot_Info/NP_BGD_Biodiv_Alm_study_sites_UTM32.csv", sep=";", dec=".")
lid_dat_r50 <- read.csv("lidar_metrics_r50.csv", sep=",", dec=".")
id <- lid_dat_r50$Plot_ID
rm(lid_dat_r50) # only Plot_IDs needed from this file

# 
dem <- raster("npb_dgm_1m_epsg25832.tif") # File note available. Line loads the digital elevation model (DEM)
topo_m_r50 <- data.frame(matrix(ncol = 9, nrow = 150)) # data frame with the metrics we are going to extract from the DEM
names(topo_m_r50) <- c("Plot_ID", 
                       "Range_ELEV", # elevation range: max(H) - min(H)
                       "SD_ELEV", #Standard deviation of Elevation: sd(H)
                       "Mean_Slope", # mean(slope)
                       "SD_Slope", # standard deviation of the slope: sd(slope)
                       "Mean_N", # mean northness: mean(cos(aspect))
                       "Mean_E", # mean eastness: mean(sin(aspect))
                       "LTI", # Local Topography Index: H_centre - mean(H)
                       "H") # Elevation: H at the centre of the plot (= H_centre) 

coords <- c()
for (i in 1:length(id)){
  coords <- rbind(coords, plot_info[plot_info$Plot == id[i], c("Plot","E","N")])
  topo_m_r50$H[i] <- plot_info$H[plot_info$Plot == id[i]]
}
names(coords)[1] <- "Plot_ID"

topo_m_r50$Plot_ID <- id

slope <- terrain(dem, "slope") # raster file containing the slope for each cell
slope_deg <- terrain(dem, "slope", unit = "degrees") # raster file containing the slope for each cell
aspect <- terrain(dem, "aspect") # raster file contraining the aspect for each cell
aspect_deg <- terrain(dem, "aspect", unit = "degrees") # raster file contraining the aspect for each cell
# slope <- raster("slope_dem.TIF")
# slope_deg <- raster("slope_dem_degrees.TIF")
# aspect <- raster("aspect_dem.TIF")
# aspect_deg <- raster("aspect_dem_degrees.TIF")


##### the values are extracted from the respective rasters using exact extract ######
### therefore buffers are created in sf format

# the buffer function required spatial points
sp_plots <- SpatialPointsDataFrame(coords = c(coords[,c("E", "N")]),
                                   proj4string = CRS("+init=EPSG:25832"),
                                   data = coords[,c("E","N")]
)

# buffers created using st_buffer from sf package
buffer_plots <- st_buffer(st_as_sf(sp_plots), dist = 50)


topo_m_r50$Range_ELEV <- exact_extract(dem # digital elevation model
                                       , buffer_plots # buffers around the plots
                                       , "max" # function we want to use
) - exact_extract(dem, buffer_plots, "min") # same thing for the minimum function which is substracted from max to get the range

topo_m_r50$SD_ELEV <- exact_extract(dem, buffer_plots, "stdev") # standard deviation of the elevation

topo_m_r50$Mean_Slope <-  exact_extract(slope, buffer_plots, "mean") # mean of the slope

topo_m_r50$SD_Slope <- exact_extract(slope, buffer_plots, "stdev") # standard deviation of the slope

### for the mean northness and mean eastness metrics we need individual functions

topo_m_r50$Mean_N <- exact_extract(aspect, buffer_plots, function(values, coverage_fraction)
  mean(coverage_fraction * cos(values))) # a function for mean northness

topo_m_r50$Mean_E <- exact_extract(aspect, buffer_plots, function(values, coverage_fraction)
  mean(coverage_fraction * sin(values))) # a function for mean eastness

topo_m_r50$LTI <- topo_m_r50$H - exact_extract(dem, buffer_plots, "mean") # local topography index

####### Following lines are used for generating the necessary raster files for the r.sun.incidout algorithm in Grass GIS. 

### it is necessary to lower the resolution of the digital elevation model to run this algorithm
dem_coarse <- aggregate(dem, fact = 10)
res(dem_coarse)
slope_deg_coarse <- aggregate(slope_deg, fact = 10)
res(slope_deg_coarse)
aspect_deg_coarse <- aggregate(aspect_deg, fact = 10)
res(aspect_deg_coarse)

### saving all the raster files
raster::writeRaster(slope, "slope_dem.TIF")
raster::writeRaster(aspect, "aspect_dem.TIF")
# raster::writeRaster(slope_deg, "slope_dem_degrees.TIF")
# raster::writeRaster(aspect_deg, "aspect_dem_degrees.TIF")
raster::writeRaster(dem_coarse, "DGM/dem_res_10.TIF")
raster::writeRaster(slope_deg_coarse, "slope_deg_res_10.TIF")
raster::writeRaster(aspect_deg_coarse, "aspect_deg_res_10.TIF")

########### Grass GIS algorithm r.sun.incidout algorithm was executed in QGIS
### rasters "aspect_deg_res10_epsg25832.TIF", "slope_deg_res10_epsg25832.TIF", and "dem_res10_epsg25832.TIF" were used
### No. of day of the year: 198
### local (solar) time: 8, 12, and 16
### all the optional fields were left blank

######## 8h rasters
### incidence angle 
inc_ang_8h <- raster::raster("incidence_angle_198d_8h.TIF")
### global irradiance
glob_irr_8h <- raster::raster("global_irradiance_198d_8h.TIF")
### beam irradiance
beam_irr_8h <- raster::raster("beam_irradiance_198d_8h.TIF")
### diffuse irradiance
diff_irr_8h <- raster::raster("diffuse_irradiance_198d_8h.TIF")
### ground reflected irradiance
grf_irr_8h <- raster::raster("ground_reflected_irradiance_198d_8h.TIF")

##### extracting the calculated values for each plot

topo_m_r50$inc_ang_8h <- raster::extract(inc_ang_8h, 
                                         coords[,c(2,3)], 
                                         df = T)[,2]

topo_m_r50$glob_irr_8h <- raster::extract(glob_irr_8h, 
                                          coords[,c(2,3)], 
                                          df = T)[,2]

topo_m_r50$beam_irr_8h <- raster::extract(beam_irr_8h, 
                                          coords[,c(2,3)], 
                                          df = T)[,2]

topo_m_r50$diff_irr_8h <- raster::extract(diff_irr_8h, 
                                          coords[,c(2,3)], 
                                          df = T)[,2]

topo_m_r50$grf_irr_8h <- raster::extract(grf_irr_8h, 
                                         coords[,c(2,3)], 
                                         df = T)[,2]

######## 12h rasters

### incidence angle 
inc_ang_12h <- raster::raster("incidence_angle_198d_12h.TIF")
### global irradiance
glob_irr_12h <- raster::raster("global_irradiance_198d_12h.TIF")
### beam irradiance
beam_irr_12h <- raster::raster("beam_irradiance_198d_12h.TIF")
### diffuse irradiance
diff_irr_12h <- raster::raster("diffuse_irradiance_198d_12h.TIF")
### ground reflected irradiance
grf_irr_12h <- raster::raster("ground_reflected_irradiance_198d_12h.TIF")

##### extracting the calculated values for each plot

topo_m_r50$inc_ang_12h <- raster::extract(inc_ang_12h, 
                                          coords[,c(2,3)], 
                                          df = T)[,2]

topo_m_r50$glob_irr_12h <- raster::extract(glob_irr_12h, 
                                           coords[,c(2,3)], 
                                           df = T)[,2]

topo_m_r50$beam_irr_12h <- raster::extract(beam_irr_12h, 
                                           coords[,c(2,3)], 
                                           df = T)[,2]

topo_m_r50$diff_irr_12h <- raster::extract(diff_irr_12h, 
                                           coords[,c(2,3)], 
                                           df = T)[,2]

topo_m_r50$grf_irr_12h <- raster::extract(grf_irr_12h, 
                                          coords[,c(2,3)], 
                                          df = T)[,2]

######## 16h rasters

### incidence angle 
inc_ang_16h <- raster::raster("incidence_angle_198d_16h.TIF")
### global irradiance
glob_irr_16h <- raster::raster("global_irradiance_198d_16h.TIF")
### beam irradiance
beam_irr_16h <- raster::raster("beam_irradiance_198d_16h.TIF")
### diffuse irradiance
diff_irr_16h <- raster::raster("diffuse_irradiance_198d_16h.TIF")
### ground reflected irradiance
grf_irr_16h <- raster::raster("ground_reflected_irradiance_198d_16h.TIF")

##### extracting the calculated values for each plot

topo_m_r50$inc_ang_16h <- raster::extract(inc_ang_16h, 
                                          coords[,c(2,3)], 
                                          df = T)[,2]

topo_m_r50$glob_irr_16h <- raster::extract(glob_irr_16h, 
                                           coords[,c(2,3)], 
                                           df = T)[,2]

topo_m_r50$beam_irr_16h <- raster::extract(beam_irr_16h, 
                                           coords[,c(2,3)], 
                                           df = T)[,2]

topo_m_r50$diff_irr_16h <- raster::extract(diff_irr_16h, 
                                           coords[,c(2,3)], 
                                           df = T)[,2]

topo_m_r50$grf_irr_16h <- raster::extract(grf_irr_16h, 
                                          coords[,c(2,3)], 
                                          df = T)[,2]

### the incident angle metrics have Nas, which are replaced with 0s with the following lines:
topo_m_r50$inc_ang_8h[is.na(topo_m_r50$inc_ang_8h)] <- 0
topo_m_r50$inc_ang_12h[is.nan(topo_m_r50$inc_ang_12h)] <- 0
topo_m_r50$inc_ang_16h[is.na(topo_m_r50$inc_ang_16h)] <- 0

base::rm(list=setdiff(base::ls(), c("topo_m_r50"))) #to remove unnecessary variables

topo_m_r50[, 2:ncol(topo_m_r50)] <- round(topo_m_r50[, 2:ncol(topo_m_r50)], 4) # rounding to 4 digits

write.csv(topo_m_r50, "topography_metrics_r50.csv", row.names = F)
topo_m_r50 <- read.csv("topography_metrics_r50.csv")


