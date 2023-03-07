library(rstudioapi)

## working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dat_t <- read.csv("temperature_data.csv")
lid_dat_r12_6 <- read.csv("lidar_metrics_r12_6.csv")

### we only consider the temperature offsets during the day from 07:00 until 19:00: 
dat_t <- dat_t[substr(dat_t$DATUM, 12,14) %in% c("07", "08", "09", 10:18), ]

### we only want the data from 08.07.2021 until 31.08.2021 (DD-MM-YYYY)
dat_t <- dat_t[substr(dat_t$DATUM, 6, 7) %in% c("07","08"),] # only those two months
dat_t <- dat_t[!((substr(dat_t$DATUM, 6, 7) %in% c("07")) & # we exclude the first 7 days from July
                   (substr(dat_t$DATUM, 9, 10) %in% c("01","02","03","04","05","06","07"))),]


###### glossary ######
# dat_t: temperature data, the microclimatic and macroclimatic data merged for each plot and time, which can then be used for creating the metrics
# lid_dat_12.6: LiDAR metrics extracted with a 12.6m radius. Here mainly used for the Plot_IDs
# temp_m: temperature metrics calculated for each plot

### we aggregate the microclimate data for each plot 
# data frame that we use to aggregate the climate data
temp_m <- data.frame(matrix(ncol = 3, nrow = 150)) 
#this was a useful error: temp_m$Plot_ID <- c(1:13, 15:26, 28:52, 55:99, 100:150, 152, 154:156)
temp_m$Plot_ID <- lid_dat_r12_6$Plot_ID # same plot_IDs as in the LiDAR_Data
rm(lid_dat_r12_6)
names(temp_m)[1:3] <- c("TO_max", "TO_mean", "TO_P95") # temp_metrics we are interested in

### calculating max, mean, and 95th percentile of temperature offset
for (i in 1:150) {
  temp_m[i, 1] <- max(dat_t$T3[dat_t$Plot_ID == temp_m$Plot_ID[i]]) - max(dat_t$t_macro[dat_t$Plot_ID == temp_m$Plot_ID[i]])
  temp_m[i, 2] <- mean(dat_t$T3[dat_t$Plot_ID == temp_m$Plot_ID[i]]) - mean(dat_t$t_macro[dat_t$Plot_ID == temp_m$Plot_ID[i]])
  temp_m[i, 3] <- quantile(dat_t$T3[dat_t$Plot_ID == temp_m$Plot_ID[i]], probs = 0.95) - quantile(dat_t$t_macro[dat_t$Plot_ID == temp_m$Plot_ID[i]], probs = 0.95)
}



write.csv(temp_m, file="temperature_metrics.csv", row.names = F)
temp_m <- read.csv("temperature_metrics.csv") #; temp_m <- subset(temp_m, select = -X) # only needed if row names in csv
