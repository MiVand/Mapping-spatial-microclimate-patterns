library(raster)
library(rgdal)
library(sf)
library(vioplot)
library(ggplot2)
library(gridExtra)
library(rstudioapi)

## working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####### reading prediction rasters:

##### reading the temperature offset maps
pred_FP_Day_TO_mean <- raster::raster("temperature_offset_rasters/pred_FP_Day_TO_mean.tif")
pred_FP_Day_TO_P95 <- raster::raster("temperature_offset_rasters/pred_FP_Day_TO_P95.tif")


##### reading the forest disturbance map
# Obtained from following Publication: Senf, Cornelius, and Rupert Seidl. 2021. 'Mapping the forest disturbance regimes of Europe', Nature Sustainability, 4: 63-70.
# https://doi.org/10.1038/s41893-020-00609-y
# the disturbance map was reprojected and cropped in QGIS
r_dist <- raster::raster("disturbance_year_1986-2020_epsg25832_cropped.tif") 



### reclassifying the forest disturbance map using space for time substitution to obtain a chronosequence with "years since disturbance"
summary(r_dist)
rcl <- matrix(c(1986:2020, NA, 35:1, 0), ncol = 2, byrow = F) 
r_dist <- raster::reclassify(r_dist, rcl)

### extracting the cell centres from the rasters
ncell(pred_FP_Day_TO_mean)
cell_centre <- xyFromCell(pred_FP_Day_TO_mean, cell = 1:ncell(pred_FP_Day_TO_mean), spatial = T)


###### Reading the raster file containing forest types
# Obtained from following Publication after request: 
# Thom, Dominik, Werner Rammer, Patrick Laux, Gerhard Smiatek, Harald Kunstmann, Sebastian Seibold, and Rupert Seidl. 2022. 'Will forest dynamics continue to accelerate throughout the 21st century in the Northern Alps?', Global Change Biology, 28: 3260-74.
# https://doi.org/10.1111/gcb.16133
forest_type <- raster::raster("forest_types2020_epsg31464.tif")
projection(forest_type)
# reproject the file to CRS of the TO maps
forest_type_repr <- raster::projectRaster(forest_type, crs=projection(pred_FP_Day_TO_mean))

###### comparing the rasters for 5 year groups ##########


comp <- matrix(c(raster::extract(r_dist, cell_centre),
                 raster::extract(pred_FP_Day_TO_mean, cell_centre),
                 raster::extract(pred_FP_Day_TO_max, cell_centre),
                 raster::extract(pred_FP_Day_TO_P95, cell_centre),
                 raster::extract(forest_type_repr, cell_centre),
                 raster::extract(dem, cell_centre)),
               ncol=6, byrow=F)
comp <- as.data.frame(na.omit(comp))
names(comp) <- c("years_since_disturbance", "TO_mean", "TO_max", "TO_P95", "forest_type", "H")
comp$years_since_disturbance[comp$years_since_disturbance == 0] <- "Undisturbed"  
comp$years_since_disturbance[comp$years_since_disturbance %in% c(1:5)] <- "1-5"
comp$years_since_disturbance[comp$years_since_disturbance %in% c(6:10)] <- "6-10"  
comp$years_since_disturbance[comp$years_since_disturbance %in% c(11:15)] <- "11-15"  
comp$years_since_disturbance[comp$years_since_disturbance %in% c(16:20)] <- "16-20"  
comp$years_since_disturbance[comp$years_since_disturbance %in% c(21:25)] <- "21-25"  
comp$years_since_disturbance[comp$years_since_disturbance %in% c(26:30)] <- "26-30"  
comp$years_since_disturbance[comp$years_since_disturbance %in% c(31:35)] <- "31-35"  



comp$years_since_disturbance <- factor(comp$years_since_disturbance, levels = c(#"Undisturbed (>35)", 
  "Undisturbed","1-5", "6-10","11-15","16-20", "21-25", "26-30", "31-35"))
levels(comp$years_since_disturbance)

comp_no_undist <- comp[comp$years_since_disturbance!= "Undisturbed", ] # leave out if undisturbed should be included


############### forest disturbance plots ################


label <- as.character(c(ncell(comp_no_undist$years_since_disturbance[comp_no_undist$years_since_disturbance == "1-5"]),
                        ncell(comp_no_undist$years_since_disturbance[comp_no_undist$years_since_disturbance == "6-10"]),
                        ncell(comp_no_undist$years_since_disturbance[comp_no_undist$years_since_disturbance == "11-15"]),
                        ncell(comp_no_undist$years_since_disturbance[comp_no_undist$years_since_disturbance == "16-20"]),
                        ncell(comp_no_undist$years_since_disturbance[comp_no_undist$years_since_disturbance == "21-25"]),
                        ncell(comp_no_undist$years_since_disturbance[comp_no_undist$years_since_disturbance == "26-30"]),
                        ncell(comp_no_undist$years_since_disturbance[comp_no_undist$years_since_disturbance == "31-35"])))
label <- paste("n = ", label)
label_df <- data.frame(x = c("1-5", "6-10","11-15","16-20", "21-25", "26-30", "31-35"),
                       y_TO_mean = rep(max(comp_no_undist$TO_mean) + 0.05*max(comp_no_undist$TO_mean), 7),
                       y_TO_P95 = rep(max(comp_no_undist$TO_P95) + 0.05*max(comp_no_undist$TO_P95), 7),
                       y_TO_max = rep(max(comp_no_undist$TO_max) + 0.05*max(comp_no_undist$TO_max), 7),
                       label = label)
my_pal <- hcl.colors(nlevels(comp_no_undist$years_since_disturbance) + 2, "Greens 3", rev = T)[2:(nlevels(comp_no_undist$years_since_disturbance) + 1)]




# dev.off()

### TO_Mean


dist_plot <- ggplot() + 
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, name = "years_since_disturbance", guide = "none") +
  labs(x = "years since disturbance", y = expression("TO"["mean"]*" (°C)"), #title = "Influence of disturbance"
  ) +   
  geom_violin(data = comp_no_undist, aes(x = years_since_disturbance, y = TO_mean, fill = years_since_disturbance),
              size = 0.3, bw = bw.nrd(comp_no_undist$TO_mean)
  ) + 
  geom_boxplot(data = comp_no_undist, aes(x = years_since_disturbance, y = TO_mean), size = 0.3, width = 0.2, outlier.shape = NA, fill = "white", alpha = 0.5) +
  geom_hline(yintercept = 0, size = 0.3, colour = "dark red") + 
  theme_minimal() +
  theme(
    axis.text = element_text(size=8),
    axis.title = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(aes(label = label_df$label, y = label_df$y_TO_mean, x=label_df$x), angle = 20, size = 2.3) 

dist_plot
ggsave("disturbance_FP_TO_mean.png", width=3.5, height=2.5, dpi=1000, units="in", bg="white")



### TO_P95


dist_plot <- ggplot() + 
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, name = "years_since_disturbance", guide = "none") +
  labs(x = "years since disturbance", y = expression("TO"["P95"]*" (°C)"), 
  ) +   
  geom_violin(data = comp_no_undist, aes(x = years_since_disturbance, y = TO_P95, fill = years_since_disturbance),
              size = 0.3, bw = bw.nrd(comp_no_undist$TO_P95) # bandwith selector, before it was 0.2
  ) + 
  geom_boxplot(data = comp_no_undist, aes(x = years_since_disturbance, y = TO_P95), size = 0.3, width = 0.2, outlier.shape = NA, fill = "white", alpha = 0.5) +
  geom_hline(yintercept = 0, size = 0.3, colour = "dark red") + 
  theme_minimal() +
  theme(
    axis.text = element_text(size=8),
    axis.title = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(aes(label = label_df$label, y = label_df$y_TO_P95, x=label_df$x), angle = 20, size = 2.3) 


dist_plot
ggsave("disturbance_FP_TO_P95.png", width=3.5, height=2.5, dpi=1000, units="in", bg="white")



########### forest type analysis ##############

comp_undist <- comp_undist[comp_undist$forest_type != 3,] # removing Spruce-fir forests, because they only occupy few raster cells

comp_undist$forest_type[comp_undist$forest_type == 1] <- "Beech"  
comp_undist$forest_type[comp_undist$forest_type == 2] <- "Spruce-fir-beech" 
comp_undist$forest_type[comp_undist$forest_type == 3] <- "Spruce-fir" 
comp_undist$forest_type[comp_undist$forest_type == 4] <- "Spruce" 
comp_undist$forest_type[comp_undist$forest_type == 5] <- "Larch-Swiss stone pine" 
comp_undist$forest_type[comp_undist$forest_type == 6] <- "Dwarf mountain pine" 


comp_undist$forest_type <- factor(comp_undist$forest_type, levels = c(#"Undisturbed (>35)", 
  "Beech" , "Spruce-fir-beech", "Spruce", "Larch-Swiss stone pine", "Dwarf mountain pine"))
levels(comp_undist$forest_type)

comp_undist <- na.omit(as.data.frame(comp_undist)) # necessary again


my_pal <- hcl.colors(nlevels(comp_undist$forest_type) + 4, "Terrain 2", rev = F, alpha = 0.5)[1:nlevels(comp_undist$forest_type)]


# labels for the plots
label <- as.character(c(ncell(comp_undist$forest_type[comp_undist$forest_type == "Beech"]),
                        ncell(comp_undist$forest_type[comp_undist$forest_type == "Spruce-fir-beech"]),
                        ncell(comp_undist$forest_type[comp_undist$forest_type == "Spruce"]),
                        ncell(comp_undist$forest_type[comp_undist$forest_type == "Larch-Swiss stone pine"]),
                        ncell(comp_undist$forest_type[comp_undist$forest_type == "Dwarf mountain pine"])))
label <- paste("n = ", label)
label_df <- data.frame(x = levels(comp_undist$forest_type), 
                       y_TO_mean = rep(max(comp_undist$TO_mean) + 0.05*max(comp_undist$TO_mean), 5),
                       y_TO_P95 = rep(max(comp_undist$TO_P95) + 0.05*max(comp_undist$TO_P95), 5),
                       y_TO_max = rep(max(comp_undist$TO_max) + 0.05*max(comp_undist$TO_max), 5),
                       label = label)


### TO_Mean

ftype_plot <- ggplot() + 
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, name = "Forest Type", guide = "none") +
  labs(x = "Forest Type", y = expression("TO"["mean"]*" (°C)"), 
  ) +   geom_violin(data = comp_undist, aes(x = forest_type, y = TO_mean, fill = forest_type),
                    size = 0.3, bw = bw.nrd(comp_undist$TO_mean)) + 
  geom_boxplot(data = comp_undist, aes(x = forest_type, y = TO_mean), fill= "white", size = 0.3, width = 0.2, outlier.shape = NA, alpha = 0.5) +
  geom_hline(yintercept = 0, size = 0.3, colour = "dark red") + 
  theme_minimal() +
  theme(
    axis.text = element_text(size=8),
    axis.text.x = element_text(angle=25, vjust = 1.1, hjust = 1),
    axis.title = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(aes(label = label_df$label 
                , y = label_df$y_TO_mean, x=label_df$x), size = 2.5) 

ftype_plot
ggsave("ftype_FP_TO_mean.png", width=3.5, height=3.5, dpi=1000, units="in", bg="white")

### TO_P95


ftype_plot <- ggplot() + 
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, name = "Forest Type", guide = "none") +
  labs(x = "Forest Type", y = expression("TO"["P95"]*" (°C)"), 
  ) +   geom_violin(data = comp_undist, aes(x = forest_type, y = TO_P95, fill = forest_type),
                    size = 0.3, bw = bw.nrd(comp_undist$TO_P95)
  ) + 

  geom_boxplot(data = comp_undist, aes(x = forest_type, y = TO_P95), fill= "white", size = 0.3, width = 0.2, outlier.shape = NA, alpha = 0.5) +
  geom_hline(yintercept = 0, size = 0.3, colour = "dark red") + 
  theme_minimal() +
  theme(
    axis.text = element_text(size=8),
    axis.text.x = element_text(angle=25, vjust = 1.1, hjust = 1),
    axis.title = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(aes(label = label_df$label 
                , y = label_df$y_TO_P95, x=label_df$x), size = 2.5) 


ftype_plot
ggsave("ftype_FP_TO_P95.png", width=3.5, height=3.5, dpi=1000, units="in", bg="white")

