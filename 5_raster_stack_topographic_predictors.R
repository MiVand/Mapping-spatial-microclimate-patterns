### note: the dem and the lidar data necessary for generating the raster stack is not available.
# however a raster stack containing all the predictor variables is included 

library(raster)
library(rgdal)
library(exactextractr)
library(sf)
library(tictoc)
library(rstudioapi)

## working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# rasterOptions(tmpdir = "/home/mvdwiele/NAS/@Recycle/R_project/tmp_R")
lidar_metrics <- raster::stack("metrics_berchtesgaden.tif") # this file is not available: would contain the raster stack with LiDAR predictor variables 
r1 <- lidar_metrics[[1]]
### just some tests:
names(lidar_metrics)
res(r1)
res(lidar_metrics)

ncell(r1)
### getting the centre points from each cell 
cell_centre <- xyFromCell(r1, cell = 1:ncell(r1), spatial = T)
### getting 50m buffer around each cell centre 
cell_buffer <- st_buffer(st_as_sf(cell_centre), dist = 50)
### saving the centres and buffers
# st_write(st_as_sf(cell_centre), "output/shape_files/cell_centre.shp",  driver="ESRI Shapefile")
# st_write(cell_buffer, "output/shape_files/cell_buffer.shp",  driver="ESRI Shapefile")

###### resampling the dem
dem <- raster("npb_dgm_1m_epsg25832.tif") # This file is not available: line is for loading the digital elevation model (DEM)
extent(r1)
extent(dem)
dem_res20m <- resample(dem, r1, "bilinear")
extent(dem_res20m)

###### calculating predictors from the buffers using exact_extract on the resampled dem

### starting with range
num_Range_ELEV <- exact_extract(dem_res20m, cell_buffer, "max") - exact_extract(dem_res20m, cell_buffer, "min")

### standard deviation
tic()
num_SD_ELEV <- exact_extract(dem_res20m, cell_buffer, "stdev") 
toc()

####### creating slope raster from resampled dem
slope_res20m <- raster::terrain(dem_res20m, "slope")


####### calculating predictors for slope raster

### calculating mean
tic()
num_Mean_Slope <- exact_extract(slope_res20m, cell_buffer, "mean") 
toc()

### calculating standard deviation 
tic()
num_SD_Slope <- exact_extract(slope_res20m, cell_buffer, "stdev") 
toc()

####### creating aspect raster from resampled dem
aspect_res20m <- raster::terrain(dem_res20m, "aspect")


### creating functions for nothness and eastness

# The summary-operations provided by exact_extract ignore NAs by default. 
# For northness and eastness a function that ignores NAs is necessary, so that the modelled area does not get smaller. 
# this will create values, where before there were NAs, which will be removed by masking during a later stage
northness <- function(arg1, arg2){
  x <- c()
  for (i in 1:length(arg1)) {
    if (is.na(arg1[i])) {
      # do nothing
    }
    else {
      x <- c(x, arg2[i] * cos(arg1[i]))
    }
  }
  x <- mean(x)
  return(x)
}

eastness <- function(arg1, arg2){
  x <- c()
  for (i in 1:length(arg1)) {
    if (is.na(arg1[i])) {
      # do nothing
    }
    else {
      x <- c(x, arg2[i] * sin(arg1[i]))
    }
  }
  x <- mean(x)
  return(x)
}

### calculating mean northness
tic()
num_Mean_N <- exact_extract(aspect_res20m, cell_buffer, northness) # a function for mean northness
toc()

### calculating mean eastness
tic()
num_Mean_E <- exact_extract(aspect_res20m, cell_buffer, eastness) # a function for mean northness
toc()

### calculating local topography index
mean_ELEV <- exact_extract(dem_res20m, cell_buffer, "mean") # this is not a predictor, but a temporary variable to calculate the LTI
tic()
num_LTI <- dem_res20m[1:ncell(dem_res20m)] - mean_ELEV
toc()



#### saving the numeric values to the assigned raster cells using rasterlayers extracted from lidar_metrics

### for LTI
r1 <- lidar_metrics[[1]]
r1[1:ncell(r1)] <- num_LTI
LTI <- r1

### for Mean_E
r1 <- lidar_metrics[[1]]
r1[1:ncell(r1)] <- num_Mean_E
Mean_E <- r1

### for Mean_N 
r1 <- lidar_metrics[[1]]
r1[1:ncell(r1)] <- num_Mean_N
Mean_N <- r1

### for Mean_Slope
r1 <- lidar_metrics[[1]]
r1[1:ncell(r1)] <- num_Mean_Slope
Mean_Slope <- r1

### for Range_ELEV
r1 <- lidar_metrics[[1]]
r1[1:ncell(r1)] <- num_Range_ELEV
Range_ELEV <- r1

### for SD_ELEV
r1 <- lidar_metrics[[1]]
r1[1:ncell(r1)] <- num_SD_ELEV
SD_ELEV <- r1

### for SD_Slope
r1 <- lidar_metrics[[1]]
r1[1:ncell(r1)] <- num_SD_Slope
SD_Slope <- r1


####### we can now save the raster files
writeRaster(LTI, "LTI.tif")
writeRaster(Mean_E, "Mean_E.tif")
writeRaster(Mean_N, "Mean_N.tif")
writeRaster(Mean_Slope, "Mean_Slope.tif")
writeRaster(Range_ELEV, "Range_ELEV.tif")
writeRaster(SD_ELEV, "SD_ELEV.tif")
writeRaster(SD_Slope, "SD_Slope.tif")
writeRaster(dem_res20m, "H.tif")

rm(num_LTI, num_Mean_Slope, num_Range_ELEV, num_SD_ELEV, num_SD_Slope, num_Mean_E, num_Mean_N, mean_ELEV)

########## Reading the rasters created in GIS
######## 8h rasters
### incidence angle 
inc_ang_8h <- raster::raster("incidence_angle_198d_8h.tif")
### global irradiance
glob_irr_8h <- raster::raster("global_irradiance_198d_8h.tif")
### beam irradiance
beam_irr_8h <- raster::raster("beam_irradiance_198d_8h.tif")
### diffuse irradiance
diff_irr_8h <- raster::raster("diffuse_irradiance_198d_8h.tif")
### ground reflected irradiance
grf_irr_8h <- raster::raster("ground_reflected_irradiance_198d_8h.tif")

######## 12h rasters

### incidence angle 
inc_ang_12h <- raster::raster("incidence_angle_198d_12h.tif")
### global irradiance
glob_irr_12h <- raster::raster("global_irradiance_198d_12h.tif")
### beam irradiance
beam_irr_12h <- raster::raster("beam_irradiance_198d_12h.tif")
### diffuse irradiance
diff_irr_12h <- raster::raster("diffuse_irradiance_198d_12h.tif")
### ground reflected irradiance
grf_irr_12h <- raster::raster("ground_reflected_irradiance_198d_12h.tif")


######## 16h rasters

### incidence angle 
inc_ang_16h <- raster::raster("incidence_angle_198d_16h.tif")
### global irradiance
glob_irr_16h <- raster::raster("global_irradiance_198d_16h.tif")
### beam irradiance
beam_irr_16h <- raster::raster("beam_irradiance_198d_16h.tif")
### diffuse irradiance
diff_irr_16h <- raster::raster("diffuse_irradiance_198d_16h.tif")
### ground reflected irradiance
grf_irr_16h <- raster::raster("ground_reflected_irradiance_198d_16h.tif")

############# resampling the rasters created in GIS: 
######## 8h rasters

### incidence angle
inc_ang_8h <- raster::resample(inc_ang_8h, r1, "bilinear")
### global irradiance
glob_irr_8h <- raster::resample(glob_irr_8h, r1, "bilinear")
### beam irradiance
beam_irr_8h <- raster::resample(beam_irr_8h, r1, "bilinear")
### diffuse irradiance
diff_irr_8h <- raster::resample(diff_irr_8h, r1, "bilinear")
### ground reflected irradiance
grf_irr_8h <- raster::resample(grf_irr_8h, r1, "bilinear")

######## 12h rasters

### incidence angle
inc_ang_12h <- raster::resample(inc_ang_12h, r1, "bilinear")
### global irradiance
glob_irr_12h <- raster::resample(glob_irr_12h, r1, "bilinear")
### beam irradiance
beam_irr_12h <- raster::resample(beam_irr_12h, r1, "bilinear")
### diffuse irradiance
diff_irr_12h <- raster::resample(diff_irr_12h, r1, "bilinear")
### ground reflected irradiance
grf_irr_12h <- raster::resample(grf_irr_12h, r1, "bilinear")

######## 16h rasters

### incidence angle
inc_ang_16h <- raster::resample(inc_ang_16h, r1, "bilinear")
### global irradiance
glob_irr_16h <- raster::resample(glob_irr_16h, r1, "bilinear")
### beam irradiance
beam_irr_16h <- raster::resample(beam_irr_16h, r1, "bilinear")
### diffuse irradiance
diff_irr_16h <- raster::resample(diff_irr_16h, r1, "bilinear")
### ground reflected irradiance
grf_irr_16h <- raster::resample(grf_irr_16h, r1, "bilinear")

####### saving the rasters
###### 8h rasters

raster::writeRaster(inc_ang_8h, "inc_ang_8h.tif")
### global irradiance
raster::writeRaster(glob_irr_8h, "glob_irr_8h.tif")
### beam irradiance
raster::writeRaster(beam_irr_8h, "beam_irr_8h.tif")
### diffuse irradiance
raster::writeRaster(diff_irr_8h, "diff_irr_8h.tif")
### ground reflected irradiance
raster::writeRaster(grf_irr_8h, "grf_irr_8h.tif")

######## 12h rasters

### incidence angle 
raster::writeRaster(inc_ang_12h, "inc_ang_12h.tif")
### global irradiance
raster::writeRaster(glob_irr_12h, "glob_irr_12h.tif")
### beam irradiance
raster::writeRaster(beam_irr_12h, "beam_irr_12h.tif")
### diffuse irradiance
raster::writeRaster(diff_irr_12h, "diff_irr_12h.tif")
### ground reflected irradiance
raster::writeRaster(grf_irr_12h, "grf_irr_12h.tif")


######## 16h rasters

### incidence angle 
raster::writeRaster(inc_ang_16h, "inc_ang_16h.tif")
### global irradiance
raster::writeRaster(glob_irr_16h, "glob_irr_16h.tif")
### beam irradiance
raster::writeRaster(beam_irr_16h, "beam_irr_16h.tif")
### diffuse irradiance
raster::writeRaster(diff_irr_16h, "diff_irr_16h.tif")
### ground reflected irradiance
raster::writeRaster(grf_irr_16h, "grf_irr_16h.tif")


####### we can now write all the rasters into a single raster stack:
fn <- list.files(path="", full.names = TRUE)
fn
fn <- fn[-7]
# some cleaning up was necessary because of unwanted additional files

rs_prediction <- raster::stack(fn)
names(rs_prediction)
names(rs_prediction)[18:73] <- names(lidar_metrics)
names(rs_prediction)

##### The raster stack is masked to match the forested area in the alpine national park

forest_dummy <- raster::raster("forest_mask.tif")
forest_dummy <- raster::resample(forest_dummy, rs_prediction)
rs_prediction <- mask(rs_prediction, forest_dummy)



##### finally saving the raster stack and associates files that can be used for model predictions

### the names need to be saved seperately as they are not saved with the raster file
names_rs_pred <- as.data.frame(names(rs_prediction))
write.csv(names_rs_pred, "names_rs_prediction.csv", row.names = F)
writeRaster(rs_prediction,"prediction_rs.tif")

### to reread the raster stack
rs_prediction <- raster::stack("prediction_rs.tif")
names_rs_pred <- read.csv("names_rs_prediction.csv")
names(rs_prediction) <- names_rs_pred$names.rs_prediction.

### removing unnecessary variables
base::rm(list=setdiff(base::ls(), c("rs_prediction"))) 







