library(gridExtra)
library(grid)
library(gtable)
library(ggplot2)
library(caret)
library(gbm)
library(raster)
library(rgdal)
library(rstudioapi)

## working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

res_R2 <- read.csv("model_comparison_R2.csv", row.names = 1)
res_RMSE <- read.csv("model_comparison_RMSE.csv", row.names = 1)

res_R2 <- round(res_R2, 2)
res_RMSE <- round(res_RMSE, 2)

####### R² tables  ########

png("R2_models.png", res=300, width=1800, height=800)

table <- tableGrob(res_R2)
grid.newpage()
grid.draw(table)

dev.off()

##### RMSE #########

png("RMSE_models.png", res=300, width=1800, height=800)

table <- tableGrob(res_RMSE)
grid.newpage()
grid.draw(table)

dev.off()


####### plotting the Scatterplots of the models #########


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
rm(dat_m_topo, dat_m_lid_r12_6)
dat_m <- subset(dat_m, select = -c(inc_ang_8h, inc_ang_12h, inc_ang_16h))

### TO_mean

dat <- subset(dat_m, select = -c(TO_P95, Plot_ID, TO_max))

set.seed(1)

mod_TO_mean  <- caret::train(
  TO_mean
  ~ .,
  data = dat,
  method = 'gbm',
  preProcess = c("center", "scale"),
  verbose = FALSE
)

cv_data <- extractPrediction(list(mod_TO_mean))
axis_lim <- c(min(c(cv_data$obs, cv_data$pred)), max(c(cv_data$obs, cv_data$pred)))
x <- seq(-50, 50, length.out = 1000)

png("FP_TO_mean.png", width=1500, height=1500, res=200)
par(col="steelblue", pch=20 , cex.axis = 1.5, cex.lab = 2, cex.main =2, cex = 1.5, mai=c(1.2, 1.5, 1, 0.1))
plot(cv_data$obs ~ cv_data$pred, xlab="Predicted (°C)", ylab="Observed (°C)", main=expression("TO"["mean"]), xlim = axis_lim, ylim = axis_lim)
lines(x, x, col="seashell4", lwd=3 )
graphics::text(x = axis_lim[1]+0.22*(abs(axis_lim[2]-axis_lim[1])), y = axis_lim[2]-0.2*(abs(axis_lim[2]-axis_lim[1])), col="black", cex=1.4, labels = base::paste0("  R² = ", round(max(as.numeric(unlist(mod_TO_mean$results[6]))), 2)))
graphics::text(x = axis_lim[1]+0.22*(abs(axis_lim[2]-axis_lim[1])), y = axis_lim[2]-0.1*(abs(axis_lim[2]-axis_lim[1])), col="black", cex=1.4, labels = base::paste0("RMSE = ", round(min(as.numeric(unlist(mod_TO_mean$results[5]))), 2), " °C"))
#axis(1, lwd.ticks=4, tck=0.015); axis(2, lwd.ticks=4, tck=0.015)
dev.off()


#### TO_P95

dat <- subset(dat_m, select = -c(TO_max, Plot_ID, TO_mean))


set.seed(1)

mod_TO_P95 <- caret::train(
  TO_P95
  ~ .,
  data = dat,
  method = 'gbm',
  preProcess = c("center", "scale"),
  verbose = FALSE
)


cv_data <- extractPrediction(list(mod_TO_P95))
axis_lim <- c(min(c(cv_data$obs, cv_data$pred)), max(c(cv_data$obs, cv_data$pred)))
x <- seq(-50, 50, length.out = 1000)

png("FP_TO_P95.png", width=1500, height=1500, res=200)
par(col="steelblue", pch=20 , cex.axis = 1.5, cex.lab = 2, cex.main =2, cex = 1.5, mai=c(1.2, 1.5, 1, 0.1))
plot(cv_data$obs ~ cv_data$pred, xlab="Predicted (°C)", ylab="Observed (°C)", main=expression("TO"["P95"]), xlim = axis_lim, ylim = axis_lim)
lines(x, x, col="seashell4", lwd=3 )
graphics::text(x = axis_lim[1]+0.22*(abs(axis_lim[2]-axis_lim[1])), y = axis_lim[2]-0.2*(abs(axis_lim[2]-axis_lim[1])), col="black", cex=1.4, labels = base::paste0("  R² = ", round(max(as.numeric(unlist(mod_TO_P95$results[6]))), 2)))
graphics::text(x = axis_lim[1]+0.22*(abs(axis_lim[2]-axis_lim[1])), y = axis_lim[2]-0.1*(abs(axis_lim[2]-axis_lim[1])), col="black", cex=1.4, labels = base::paste0("RMSE = ", round(min(as.numeric(unlist(mod_TO_P95$results[5]))), 2), " °C"))
#axis(1, lwd.ticks=4, tck=0.015); axis(2, lwd.ticks=4, tck=0.015)
dev.off()






