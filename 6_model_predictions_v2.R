
#### Packages and working directory ####

library(gridExtra)
library(grid)
library(gtable)
library(caret)
library(gbm)
library(rstudioapi)
library(blockCV)
library(sf)
library(terra)

# working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### Prepare data ####

# Spatial data for prediction
rs_prediction <- rast("prediction_rs.tif") 
names_rs_pred <- read.csv("names_rs_prediction.csv")
names(rs_prediction) <- names_rs_pred$names.rs_prediction.
rm(names_rs_pred)

# Plot locations for spatial CV
plots <- read_sf("Forest_Plot_Info/Forest_Plots_GIS.shp") %>%
  dplyr::select(Plot_ID = Plot)

# reading the temperature metrics: full Period, only daytime, hourly
temp_m <- read.csv("temperature_metrics.csv")

# Reading the topography metrics: all the predictors, exact extract, grass gis r50
topo_m <- read.csv("topography_metrics_r50.csv")
topo_m <- subset(topo_m, select = -c(inc_ang_8h, inc_ang_12h, inc_ang_16h))

# Reading the LiDAR metrics: r = 12.6, filtered point cloud
lid_m_r12_6 <- read.csv("lidar_metrics_r12_6.csv")

# Merge all the files into a single data frame
dat_m_topo <- merge(temp_m, topo_m, by = "Plot_ID")
dat_m_lid_r12_6 <- merge(temp_m, lid_m_r12_6, by = "Plot_ID")
dat_m <- merge(dat_m_topo, lid_m_r12_6, by= "Plot_ID")
rm(dat_m_topo, dat_m_lid_r12_6)

# plots %>%
#   left_join(dat_m) %>%
#   write_sf(., "/Users/corneliussenf/Desktop/plots_predictors.gpkg")

#### For revision: Model with pre-selected predictors ####

# Function

prediction_function <- function(var) {
  
  dat <- dat_m
  dat$var <- dat[, var]
  var_subscript <- substr(var, 4, 10)
  
  mod <- caret::train(
    var
    ~ pground + zmean + zpcum1 + poly(H, 2) + LTI + Mean_Slope + diff_irr_12h + diff_irr_16h,
    data = dat,
    method = "lm",
    preProcess = c("center", "scale")
  )
  
  accuracies <- data.frame(metric = c("R2", "RMSE"), 
                           cv = c(mod$results["Rsquared"][[1]], mod$results["RMSE"][[1]]))
  
  # Spatial CV
  scv <- cv_cluster(x = plots, k = 10)
  
  dat_predict <- subset(dat, select = var)
  
  k <- 0
  for (i in scv$folds_list) {
    k <- k + 1
    print(k)
    mod_cv_lm <- caret::train(
      var
      ~ pground + zmean + zpcum1 + poly(H, 2) + LTI + Mean_Slope + diff_irr_12h + diff_irr_16h,
      data = dat[i[[1]], ],
      method = 'lm',
      preProcess = c("center", "scale")
    )
    dat_predict[i[[2]], "predict_scv_lm"] <- predict(mod_cv_lm, newdata = dat[i[[2]], ])
  }
  
  r2 <- cor(dat_predict$var, dat_predict$predict_scv_lm)^2
  rmse <- sqrt(mean((dat_predict$var - dat_predict$predict_scv_lm)^2))
  
  accuracies$cv_spatial <- c(r2, rmse)
  
  write.csv(accuracies, paste0("results/accuracies_", var, ".csv"))
  
  axis_lim <- c(min(c(dat_predict$var, dat_predict$predict_scv_lm)), 
                max(c(dat_predict$var, dat_predict$predict_scv_lm)))
  
  png(paste0("results/scatterplot_", var, ".png"), width=1500, height=1500, res=200)
  par(col="steelblue", pch=20 , cex.axis = 1.5, cex.lab = 2, cex.main =2, cex = 1.5, mai=c(1.2, 1.5, 1, 0.1))
  plot(dat_predict$var ~ dat_predict$predict_scv_lm, xlab="Predicted (°C)", ylab="Observed (°C)", main=var_subscript, xlim = axis_lim, ylim = axis_lim)
  abline(0, 1, col="seashell4", lwd=3 )
  dev.off()
  
  return(dat_predict)
  
  # Effect sizes
  
  effects <- broom::tidy(mod$finalModel)
  
  effects$partial_r2 <- c(NA, heplots::etasq(mod$finalModel)[,1][-9])
  
  write.csv(effects, paste0("results/effects_", var, ".csv"), row.names = FALSE)
  
  # Spatial predictions
  pred_spatial <- predict(rs_prediction[[names(mod$trainingData)[-1]]], mod, na.rm = TRUE)
  writeRaster(pred_spatial, paste0("results/prediction_", var, ".tif"), overwrite = TRUE)
  
  # Compare distributions
  stats_sample <- dat %>%
    summarize(mean = mean(var),
              sd = sd(var),
              median = median(var),
              min = min(var),
              q25 = quantile(var, 0.25),
              q75 = quantile(var, 0.75),
              max = max(var),
              prop_cooler = mean(var < 0)) %>%
    mutate(population = "sample")
  
  stats_landscape <- as.data.frame(pred_spatial) %>%
    set_names("var") %>%
    summarize(mean = mean(var),
              sd = sd(var),
              median = median(var),
              min = min(var),
              q25 = quantile(var, 0.25),
              q75 = quantile(var, 0.75),
              max = max(var),
              prop_cooler = mean(var < 0)) %>%
    mutate(population = "landscape")
  
  stats <- rbind(stats_sample, stats_landscape)
  
  write_csv(stats, paste0("results/stats_", var, ".csv"))
  
}

metrics <- c("TO_min", "TO_P01", "TO_P05", "TO_mean", "TO_range", "TO_P95", "TO_P99", "TO_max")
predictions <- vector("list", length(metrics))

for (i in 1:length(metrics)) {
  predictions[[i]] <- prediction_function(metrics[i])
}

predictions %>%
  set_names(metrics) %>%
  bind_rows(.id = "metric") %>%
  filter(metric %in% c("TO_min", "TO_mean", "TO_max")) %>%
  mutate(
    metric = case_when(
      metric == "TO_min" ~ "Minimum",
      metric == "TO_mean" ~ "Mean",
      metric == "TO_max" ~ "Maximum"
    )
  ) %>%
  split(.$metric) %>%
  map(.,
      ~ ggplot(data = .) +
        geom_point(aes(x = var,
                       y = predict_scv_lm)) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = NA, color = "black"),
          axis.text = element_text(size=8),
          axis.title = element_text(size = 10)
        ) + 
        facet_wrap(~metric) +
        geom_abline(intercept = 0, slope = 1, col = "darkgrey", linetype = "dashed") +
        labs(x = ifelse(unique(.$metric == "Mean"), "Observed offset (°C)", ""),
             y = ifelse(unique(.$metric == "Maximum"), "Predicted offset (°C)", "")) +
        xlim(min(c(.$var, .$predict_scv_lm)), 
             max(c(.$var, .$predict_scv_lm))) +
        ylim(min(c(.$var, .$predict_scv_lm)), 
             max(c(.$var, .$predict_scv_lm)))
      ) %>%
  patchwork::wrap_plots(ncol = 3)

ggsave("results/scatterplots.png", width = 7.5, height = 3)

list.files("results/", pattern = glob2rx("effects*csv"), full.names = TRUE) %>%
  map(read_csv) %>%
  set_names(sort(c("TO_min", "TO_P01", "TO_P05", "TO_mean", "TO_range", "TO_P95", "TO_P99", "TO_max"))) %>%
  bind_rows(.id = "metric") %>%
  filter(metric %in% c("TO_min", "TO_mean", "TO_max")) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "zpcum1" ~ "Ground veg. density",
      term == "zmean" ~ "Height",
      term == "pground" ~ "Canopy density",
      term == "Mean_Slope" ~ "Slope",
      term == "LTI" ~ "Topographic index",
      term == "H" ~ "Elevation",
      term == "diff_irr_16h" ~ "Diffuse solar rad. (16:00)",
      term == "diff_irr_12h" ~ "Diffuse solar rad. (12:00)",
    ),
    metric = case_when(
      metric == "TO_min" ~ "Minimum",
      metric == "TO_mean" ~ "Mean",
      metric == "TO_max" ~ "Maximum"
    )
  ) %>%
  ggplot(.) +
  geom_errorbar(aes(x = reorder(term, partial_r2), 
                    ymin = estimate - 2 * std.error, 
                    ymax = estimate + 2 * std.error),
                width = 0,
                position = position_dodge(width = 1)) +
  geom_point(aes(x = reorder(term, partial_r2), 
                 y = estimate,
                 size = partial_r2),
             position = position_dodge(width = 1)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text = element_text(size=8),
    axis.title = element_text(size = 10)
  ) + 
  coord_flip() +
  geom_hline(yintercept = 0, col = "darkgrey", linetype = "dashed") +
  facet_wrap(~metric, scales = "free_x") +
  labs(y = "Standardized estimate",
       x = NULL,
       size = bquote(eta^2))

ggsave("results/effects.png", width = 7.5, height = 3)
