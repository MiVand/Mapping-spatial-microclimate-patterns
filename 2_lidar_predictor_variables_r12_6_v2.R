### note: the LiDAR data necessary for running this script is not available
# however the output-file ("lidar_metrics_r12_6.csv") is included

options(rgl.debug = TRUE)
library(lidR)
library(raster)
library(sp)
library(rgl)
library(rstudioapi)

## working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


###### glossary ######
# id = "data ID": temporary variable for adding a column with the correct plot_IDs
# las: temporary variable for reading individual las files 
# lid_m_r12_6 = "LiDAR Data for 12.6 m radius": final output of this file --> data frame with the the standard lidR metrics for each plot 
#

### creating a column with the correct IDs for each row

id <- data.frame(matrix(ncol = 1, nrow = 150))
for (i in 1:9) { # first the 1 digit IDs to get the correct file name
  id[i,] <- paste0("F00",i)
} 
for (i in 10:13) { # F014 is missing in the files
  id[i,] <- paste0("F0",i)
}
for (i in 15:26) { # F027 is missing in the files
  id[i-1,] <- paste0("F0",i)
}
for (i in 28:52) { # F053 and F054 are missing in the files
  id[i-2,] <- paste0("F0",i)
}
for (i in 55:99) { # those are the rest of the 2 digit ID files
  id[i-4,] <- paste0("F0",i)
}
for (i in 100:150) { # F151 is missing in the files
  id[i-4,] <- paste0("F",i)
}
# F153 is missing in the files --> no for-loop necessary for 1 line
id[147,] <- "F152"
for (i in 154:156) { 
  id[i-6,] <- paste0("F",i)
}

###### creating the metrics from the LiDAR las files ######
### creating a data frame with the correct number and names of columns 
las <- readLAS(paste0("las_biodiv_plots/F001_r12.6_norm.las"))
las <- filter_poi(las, Z >= 0, Z <= 50) 
lid_m_r12_6 <- as.data.frame(cloud_metrics(las, func = .stdmetrics))

### filling the data frame with the standard metrics from the LidR package 

for (i in 1:150) { # first the 1 digit IDs to get the correct file name
  las <- readLAS(paste0("las_biodiv_plots/", id[i,],"_r12.6_norm.las"))
  las <- filter_poi(las, Z >= 0, Z <= 50)
  lid_m_r12_6[i,] <- as.data.frame(cloud_metrics(las, func = .stdmetrics))
} 

lid_m_r12_6 <-  cbind(id, lid_m_r12_6) # we add the column with the names to the main data frame
names(lid_m_r12_6) <- c("Plot_ID", names(lid_m_r12_6[2:ncol(lid_m_r12_6)])) # we rename the name of the added column
rm(id, las) # removing unnecessary temporary variables


write.csv(lid_m_r12_6, file="lidar_metrics_r12_6.csv")
lid_m_r12_6 <- read.csv("lidar_metrics_r12_6.csv"); lid_m_r12_6 <- subset(lid_m_r12_6, select = -X)
