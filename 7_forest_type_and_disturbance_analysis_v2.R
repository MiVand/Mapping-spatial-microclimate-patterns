
#### Packages and working directory ####

library(raster)
library(rgdal)
library(sf)
library(vioplot)
library(ggplot2)
library(gridExtra)
library(rstudioapi)

## working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### Get data ####

# Temperature offset maps
pred_TO_mean <- raster("results/prediction_TO_mean.tif")
pred_TO_max <- raster("results/prediction_TO_max.tif")
pred_TO_min <- raster("results/prediction_TO_min.tif")

# Forest disturbance map
# Obtained from following Publication: Senf, Cornelius, and Rupert Seidl. 2021. 'Mapping the forest disturbance regimes of Europe', Nature Sustainability, 4: 63-70.
# https://doi.org/10.1038/s41893-020-00609-y
# the disturbance map was reprojected and cropped in QGIS
r_dist <- raster("disturbance_year_1986-2020_epsg25832_cropped.tif") 

# Reclassifying the forest disturbance map using space for time substitution to obtain a chronosequence with "years since disturbance"
rcl <- matrix(c(1986:2020, NA, 35:1, 0), ncol = 2, byrow = F) 
r_dist <- raster::reclassify(r_dist, rcl)

# Extracting the cell centres from the rasters
cell_centre <- xyFromCell(pred_TO_mean, cell = 1:ncell(pred_TO_mean), spatial = T)

# Forest types
# Obtained from following Publication after request: 
# Thom, Dominik, Werner Rammer, Patrick Laux, Gerhard Smiatek, Harald Kunstmann, Sebastian Seibold, and Rupert Seidl. 2022. 'Will forest dynamics continue to accelerate throughout the 21st century in the Northern Alps?', Global Change Biology, 28: 3260-74.
# https://doi.org/10.1111/gcb.16133
forest_type <- raster::raster("forest_types2020_epsg31464.tif")
forest_type_repr <- raster::projectRaster(forest_type, crs=projection(pred_TO_mean))

#### Comparing offset to disturbances for 5 year groups #####

comp <- matrix(c(raster::extract(r_dist, cell_centre),
                 raster::extract(pred_TO_mean, cell_centre),
                 raster::extract(pred_TO_max, cell_centre),
                 raster::extract(pred_TO_min, cell_centre),
                 raster::extract(forest_type_repr, cell_centre)),
               ncol=5, byrow=F)
comp <- as.data.frame(na.omit(comp))
names(comp) <- c("years_since_disturbance", "TO_mean", "TO_max", "TO_min", "forest_type")
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

### Forest disturbance plots 

my_pal <- hcl.colors(nlevels(comp_no_undist$years_since_disturbance) + 2, "Greens 3", rev = T)[2:(nlevels(comp_no_undist$years_since_disturbance) + 1)]

ggplot(data = comp_no_undist %>%
         gather(key = key,
                value = value,
                -years_since_disturbance,
                -forest_type) %>%
         mutate(
           key = case_when(
             key == "TO_min" ~ "Minimum",
             key == "TO_mean" ~ "Mean",
             key == "TO_max" ~ "Maximum"
           )
         )) + 
  scale_color_manual(values = my_pal, 
                     guide = "none") +
  scale_fill_manual(values = my_pal, 
                    name = "Forest Type", 
                    guide = "none") +
  labs(x = "Years after disturbance", 
       y = "Temperature offset (°C)") +   
  geom_violin(aes(x = years_since_disturbance, 
                  y = value, 
                  fill = years_since_disturbance),
              size = 0.3) + 
  geom_boxplot(aes(x = years_since_disturbance, 
                   y = value), 
               fill= "white", 
               size = 0.3, 
               width = 0.2, 
               outlier.shape = NA, 
               alpha = 0.5) +
  geom_hline(yintercept = 0, 
             size = 0.3, 
             colour = "dark red") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text = element_text(size=8),
    axis.text.x = element_text(angle=25, vjust = 1.1, hjust = 1),
    axis.title = element_text(size = 10),
    legend.position = "none"
  ) +
  facet_wrap(~key, scales = "free")

ggsave("results/disturbance.png", width=7.5, height=3, dpi=1000, units="in", bg="white")

summary(lm(TO_mean ~ years_since_disturbance, data = comp))
summary(lm(TO_max ~ years_since_disturbance, data = comp))
summary(lm(TO_min ~ years_since_disturbance, data = comp))

summary(lm(TO_mean ~ years_since_disturbance, data = comp_no_undist))
summary(lm(TO_max ~ years_since_disturbance, data = comp_no_undist))
summary(lm(TO_min ~ years_since_disturbance, data = comp_no_undist))

### Forest type plot

comp_undist <- comp[comp$years_since_disturbance == "Undisturbed", ]

comp_undist <- comp_undist[comp_no_undist$forest_type != 3,] # removing Spruce-fir forests, because they only occupy few raster cells

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

ggplot(data = comp_undist %>%
         gather(key = key,
                value = value,
                -years_since_disturbance,
                -forest_type) %>%
         mutate(
           key = case_when(
             key == "TO_min" ~ "Minimum",
             key == "TO_mean" ~ "Mean",
             key == "TO_max" ~ "Maximum"
           )
         )) + 
  scale_color_manual(values = my_pal, 
                     guide = "none") +
  scale_fill_manual(values = my_pal, 
                    name = "Forest Type", 
                    guide = "none") +
  labs(x = "Forest Type", 
       y = "Temperature offset (°C)") +   
  geom_violin(aes(x = forest_type, 
                  y = value, 
                  fill = forest_type),
              size = 0.3) + 
  geom_boxplot(aes(x = forest_type, y = value), 
               fill= "white", 
               size = 0.3, 
               width = 0.2, 
               outlier.shape = NA, 
               alpha = 0.5) +
  geom_hline(yintercept = 0, 
             size = 0.3, 
             colour = "dark red") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text = element_text(size=8),
    axis.text.x = element_text(angle=25, vjust = 1.1, hjust = 1),
    axis.title = element_text(size = 10),
    legend.position = "none"
  ) +
  facet_wrap(~key, scales = "free")

ggsave("results/ftype.png", width=7.5, height=3.5, dpi=1000, units="in", bg="white")

comp_undist %>% 
  dplyr::select(-years_since_disturbance) %>%
  gather(key = key, value = value, -forest_type) %>%
  filter(key %in% c("TO_min", "TO_mean", "TO_max")) %>%
  mutate(
    key = case_when(
      key == "TO_min" ~ "Minimum",
      key == "TO_mean" ~ "Mean",
      key == "TO_max" ~ "Maximum"
    )
  ) %>%
  group_by(forest_type, key) %>%
  summarize(mean = mean(value),
            median = median(value),
            below_zero = mean(value < 0))

summary(lm(TO_mean ~ forest_type, data = comp_undist))
summary(lm(TO_max ~ forest_type, data = comp_undist))
summary(lm(TO_min ~ forest_type, data = comp_undist))

