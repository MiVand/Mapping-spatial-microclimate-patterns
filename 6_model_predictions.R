library(gridExtra)
library(grid)
library(gtable)
library(caret)
library(gbm)
library(rstudioapi)

## working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))



rs_prediction <- raster::stack("prediction_rs.tif") 
names_rs_pred <- read.csv("names_rs_prediction.csv")
names(rs_prediction) <- names_rs_pred$names.rs_prediction.
rm(names_rs_pred)


### creating a df with the R² results

res_R2 <- data.frame(matrix(ncol = 0, nrow = 3))
row.names(res_R2) <- c("TO_mean", "TO_max", "TO_P95")
res_RMSE <- data.frame(matrix(ncol = 0, nrow = 3))
row.names(res_RMSE) <- c("TO_mean", "TO_max", "TO_P95")


### In this Script tests for R² and RMSE values, as well as the temperature variables are implemented for various models. 


###########################################################################
# Model Predictions and Results
###########################################################################


######### reading the temperature metrics (dependant variables)

####### Full Period, only daytime, hourly
temp_m <- read.csv("temperature_metrics.csv")

####### reading the topography metrics

### all the predictors, exact extract, grass gis r50
topo_m <- read.csv("topography_metrics_r50.csv")

####### reading the LiDAR metrics

### r = 12.6, filtered point cloud
lid_m_r12_6 <- read.csv("lidar_metrics_r12_6.csv")


### we merge all the files into a single data frame
dat_m_topo <- merge(temp_m, topo_m, by = "Plot_ID")
dat_m_lid_r12_6 <- merge(temp_m, lid_m_r12_6, by = "Plot_ID")
dat_m <- merge(dat_m_topo, lid_m_r12_6, by= "Plot_ID")
dat_m <- subset(dat_m, select = -c(inc_ang_8h, inc_ang_12h, inc_ang_16h))
rm(dat_m_topo, dat_m_lid_r12_6)



########## Models with all the predictors 

##### starting with TO_mean


dat <- subset(dat_m, select = -c(TO_P95, Plot_ID, TO_max))

set.seed(1)

mod <- caret::train(
  TO_mean
  ~ .,
  data = dat,
  method = 'gbm',
  preProcess = c("center", "scale"),
  verbose = FALSE
)
mod

ri_all_TO_mean <- summary(mod)
names(ri_all_TO_mean)[2] <- "TO_mean"
ri_all_TO_mean <- ri_all_TO_mean[ri_all_TO_mean$TO_mean != 0, ]

res_R2$'Full Period'[1] <- max(as.numeric(unlist(mod$results[6])))
res_RMSE$'Full Period'[1] <- min(as.numeric(unlist(mod$results[5])))


pred_FP_Day_TO_mean <- raster::predict(rs_prediction, mod)
raster::writeRaster(pred_FP_Day_TO_mean, "temperature_offset_rasters/pred_FP_Day_TO_mean.tif")


###### with TO_P95

dat <- subset(dat_m, select = -c(TO_max, Plot_ID, TO_mean))


set.seed(1)

mod <- caret::train(
  TO_P95
  ~ .,
  data = dat,
  method = 'gbm',
  preProcess = c("center", "scale"),
  verbose = FALSE
)
mod

ri_all_TO_P95 <- summary(mod)
names(ri_all_TO_P95)[2] <- "TO_P95"
ri_all_TO_P95 <- ri_all_TO_P95[ri_all_TO_P95$TO_P95 != 0, ]
# 
res_R2$'Full Period'[3] <- max(as.numeric(unlist(mod$results[6])))
res_RMSE$'Full Period'[3] <- min(as.numeric(unlist(mod$results[5])))

pred_FP_Day_TO_P95 <- raster::predict(rs_prediction, mod)
raster::writeRaster(pred_FP_Day_TO_P95, "temperature_offset_rasters/pred_FP_Day_TO_P95.tif")

############# Calculation of relative influence of the groups ############

####### the following lines calculate the relative influence of LiDAR (canopy) and topography metrics on the prediction of all the predictors
#### H is considered independently, because it has a big influence on mean offset

write.csv(ri_all_TO_mean, "ri_all_FP_TO_mean.csv", row.names = F)
write.csv(ri_all_TO_P95, "ri_all_FP_TO_P95.csv", row.names = F)

toponames <- names(topo_m)
toponames <- toponames[toponames != "H"]

ri_lid_TO_mean <- 0
ri_topo_TO_mean <- 0
for (i in 1:nrow(ri_all_TO_mean)){
  if (row.names(ri_all_TO_mean)[i] %in% names(lid_m_r12_6)) {
    ri_lid_TO_mean <- ri_lid_TO_mean + ri_all_TO_mean$TO_mean[i]
  }
  if (row.names(ri_all_TO_mean)[i] %in% toponames) {
    ri_topo_TO_mean <- ri_topo_TO_mean + ri_all_TO_mean$TO_mean[i]
  }
}


ri_lid_TO_P95 <- 0
ri_topo_TO_P95 <- 0
for (i in 1:nrow(ri_all_TO_P95)){
  if (row.names(ri_all_TO_P95)[i] %in% names(lid_m_r12_6)) {
    ri_lid_TO_P95 <- ri_lid_TO_P95 + ri_all_TO_P95$TO_P95[i]
  }
  if (row.names(ri_all_TO_P95)[i] %in% toponames) {
    ri_topo_TO_P95 <- ri_topo_TO_P95 + ri_all_TO_P95$TO_P95[i]
  }
}


### the % of relative  from the respective groups of predictors can now be put into a single table
ri_tab <- data.frame(matrix(c(ri_lid_TO_mean, ri_lid_TO_P95,
                              ri_topo_TO_mean, ri_topo_TO_P95,
                              ri_all_TO_mean$TO_mean[ri_all_TO_mean$var == "H"], ri_all_TO_P95$TO_P95[ri_all_TO_P95$var == "H"])
                            , ncol = 2
                            , byrow = F))
ri_tab <- round(ri_tab, 2)
row.names(ri_tab) <- c("TO_mean", "TO_P95")
names(ri_tab) <- c("Canopy", "Topography", "Elevation")

write.csv(ri_tab, "ri_FP.csv")
ri_tab <- read.csv("ri_FP.csv", row.names = 1)
base::rm(list=setdiff(base::ls(), c("res_R2", "res_RMSE", "rs_prediction"))) # the only variable we want to keep



######### finally the RMSE and R² results are saved ###########



write.csv(res_R2, "model_comparison_R2.csv")
# res_R2 <- read.csv("model_comparison_R2.csv", row.names = 1)
write.csv(res_RMSE, "model_comparison_RMSE.csv")
# res_RMSE <- read.csv("model_comparison_RMSE.csv", row.names = 1)
