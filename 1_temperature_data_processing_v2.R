### note: the temperature data necessary for running this script is not available
# However, the output file "temperature_data.csv" is included  

library(readxl)
library(tictoc)
library(rstudioapi)

## working directory is set to the dir of the file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

###### glossary ######
# dat_t: temperature data, the microclimatic and macroclimatic data merged for each plot and time, which can then be used for creating the metrics
# mc_t_compl: complete microclimate data (temperature)
# mc_t: temperature data from the forest plots used for modelling 
# cs_t: climate station data (temperature), the data frame where the climate station data is merged, selected and ordered
# cs_XXX: climate station data from a specific climate station, XXX stands for the abbreviation of the name of the respective climate station 
# h: vector containing the heights of the climate stations used for linear models
# count: temporary variable used for counting loops during creation of images of linear model plots
# mod: temporary models of temp vs height used in for loops
# new: data frame containing only the height of the respective forest plot used in predict function
# plot_info: data frame containing information about the forest plots (most relevant: heights and coordinates)
# nr: number of rows in cs_t, cs_t_temp, or mc_t, used in for loops
# nc: number of columns in cs_t, cs_t_temp, or mc_t, used in for loops
# cs_t_interp: data frame containing the interpolated macroclimatic data for the forest plots
# cs_t_temp: temporary climate station data, complete data set, with larger time span than needed
# lid_dat_12.6: LiDAR metrics extracted with a 12.6m radius. Here mainly used for the Plot_IDs
# id: list with the ids of the forest plots used in for loops

###### Microclimate temp loggers ######

mc_t_compl <- read.csv(file="Microclimate_logger/Loggerdaten_Gesamt_decpoint.csv", dec=".", sep=";") # file not included: microclimate data

mc_t <- data.frame(matrix(ncol = ncol(mc_t_compl), nrow = 0)) # data frame that we use for selecting the temp data from the forest plots
names(mc_t) <- names(mc_t_compl) # same column names as the complete data set
lid_dat_12.6 <- read.csv("lidar_metrics_12_6.csv"); lid_dat_12.6 <- subset(lid_dat_12.6, select = -X)
id <- lid_dat_12.6$Plot_ID #the IDs of the plots used in for loops
rm(lid_dat_12.6) #previous command was all we needed the file for

### Selection of the rows with data from the forest plots, since there are also non forest plots in the data
for (i in 1:150){
  mc_t <- rbind(mc_t, mc_t_compl[mc_t_compl$Plot_ID == id[i],])
}
rm(mc_t_compl)

# to merge the micro climate data with the macro climate data frame, we add another column
mc_t$DATUM <- paste0(gsub("\\.", "-", mc_t$date_1), " ", mc_t$date_2) # to get the correct date format

write.csv(mc_t, file="Microclimate_logger/Loggerdaten_Gesamt_decpoint_Forest_Selection.csv")
mc_t <- read.csv("Microclimate_logger/Loggerdaten_Gesamt_decpoint_Forest_Selection.csv"); mc_t <- subset(mc_t, select = -X)
#mc_t <- read.csv("Loggerdaten_Gesamt_decpoint_Forest_Selection.csv"); mc_t <- subset(mc_t, select = -X)

mc_t <- mc_t[,c("DATUM", "Plot_ID", "T1", "T2", "T3")] # these are the variables we are interested in
temp <- mc_t[mc_t$DATUM == "2021-09-01 00:00",] # this is the only date we want to include from September
mc_t <- mc_t[substr(mc_t$DATUM, 6, 7) %in% c("06", "07", "08"),] # we only need the months of June, july and august
mc_t <- rbind(mc_t, temp) # we add the rows
rm(temp) # temporary variable is no longer necessary
mc_t <- mc_t[order(mc_t$Plot_ID, decreasing = F),] # we still need to get the added date from September to their correct place in the data frame

nrow(mc_t[duplicated(mc_t),]) # there are duplicates in the data
# they can be removed with the following line:
mc_t <- mc_t[!duplicated(mc_t), ] 
# the number of rows now matches the expectations

### next the average temperatures for each hour are taken with the following loops:
nr <- nrow(mc_t) # needed for the for loop (scanning mc_t)
nc <- ncol(mc_t)
# new data frame, where the data is to sumarized to get hourly values:
mc_t_new <- data.frame(matrix(nrow=((nr-150)/4), # nr - 150 because of the included measurements at 2021-09-01 00:00
                              ncol=nc)) # otherwise the data frame will be identical to mc_t
names(mc_t_new) <- names(mc_t) # with the same column names 
index <- 1 # index variable to fill the new df
tic()
for (i in 1:nr){
  if (substr(mc_t$DATUM[i], 15, 16) == "00"){ # we want to summarize from :00 to :00 to get identical periods as in the climate station data
    #for the date in the next line, the mean is not taken, because the data for each plot ends here. Therefore the if loop is exited
    if (mc_t$DATUM[i] == "2021-09-01 00:00"){
      # do nothing in this case
    }
    else{
      mc_t_new$DATUM[index] <- substr(mc_t$DATUM[i], 1, 13)
      mc_t_new$Plot_ID[index] <- mc_t$Plot_ID[i]
      for (j in 3:nc){
        mc_t_new[index, j] <- mean(mc_t[i:(i+4), j])
      }  
      index <- index +1 
    }
  }
}
toc()
# 2567.88 sec elapsed

mc_t_new[, 3:ncol(mc_t_new)] <- round(mc_t_new[, 3:ncol(mc_t_new)], 1)
mc_t <- mc_t_new
rm(mc_t_new)

write.csv(mc_t, file="micro_climate_data_hourly.csv")
mc_t <- read.csv("micro_climate_data_hourly.csv"); mc_t <- subset(mc_t, select = -X)


####### Climate Stations Data: files not included ####### 

####### we first have to read the data, which is located in several tables from 2 sources (external and internal data-server)

##### from external data-server: 
### climate station "Hoellgraben" (H = 657):
cs_hoell <- read.csv("data_climate_stations/exmds/exmds_hoellgraben_210301000000_-_211001000000/hoellgraben.csv", sep=",", dec=".", header = T)
names(cs_hoell)[1] <- "DATUM"
cs_hoell <- cs_hoell[-1,] # we remove the first row, because we don't need it
nchar(cs_hoell$DATUM[1]) #19 characters, which is the same for every row
cs_hoell$DATUM <- substr(cs_hoell$DATUM, 1, 16) # last three characters are removed from string

### climate station "Kuehroint" (H = 1411)
cs_kueh <- read.csv("data_climate_stations/exmds/exmds_kuehroint_210301000000_-_210720045000/kuehroint.csv", sep=",", dec=".", header = T)
# this data set is incomplete due to power outage
# cut off at: 2021-07-20 04:50:00
names(cs_kueh)[1] <- "DATUM"
cs_kueh <- cs_kueh[-1,] # we remove the first row, because we don't need it
cs_kueh$DATUM <- substr(cs_kueh$DATUM, 1, 16) # last two characters are removed from string


### climate stations"Steinernes Meer -"  
# "- Hirsch" (H = 1991)
# "-In der Eul" (H = 1893)
cs_stein <- read.csv("data_climate_stations/exmds/exmds_steinernes_meer_210301000000_-_210723071129/steinernesMeer.csv", sep=",", dec=".", header = T)
names(cs_stein)[1] <- "DATUM"
cs_stein <- cs_stein[-1,] # we remove the first row, because we don't need it
cs_stein$DATUM <- substr(cs_stein$DATUM, 1, 16) # last two characters are removed from string


###### climate stations from the national park's internal database 
### climate station "Alpelwand" (H = 851)
cs_alp <- read_excel("data_climate_stations/exdb/exdb_alpelwand_2021.xlsx")

### climate station "Bindalm" (H = 1074)
cs_bind <- read_excel("data_climate_stations/exdb/exdb_bindalm_2021.xlsx")

### climate station "Blaueishütte" (H = 1649)
cs_blau <- read_excel("data_climate_stations/exdb/exdb_blaueis_2021.xlsx")

### climate station "Brunftbergtiefe" (H = 1239)
cs_brunft <- read_excel("data_climate_stations/exdb/exdb_brunftbergtiefe_2021.xlsx")

### climate station "Funtenseetauern LWD" (H = 2521)
cs_funt <- read_excel("data_climate_stations/exdb/exdb_funtenseetauern_2021.xlsx")

### climate station "Hinterberghorn" (H = 2472)
cs_hinterb <- read_excel("data_climate_stations/exdb/exdb_hinterberghorn_2021.xlsx")

### climate station "Hinterseeau" (H = 832)
cs_hinters <- read_excel("data_climate_stations/exdb/exdb_hinterseeau_2021.xlsx")

### climate station "Kühroint LWD" (H = 1411)
cs_kueh2 <- read_excel("data_climate_stations/exdb/exdb_kuehroint_2021.xlsx")
# same as for external data base: incomplete data due to power outage

### climate stations"Reiteralm LWD -"  
# "- Mitte" (H = 1679)
# "- Oben" (H = 1756)
# "- Unten" (H = 1612)
cs_reit <- read_excel("data_climate_stations/exdb/exdb_reiteralpe_2021.xlsx")
# !! Height different in table header!! 

### climate station "Schlunghorn" (H = 2097)
cs_schlu <- read_excel("data_climate_stations/exdb/exdb_schlunghorn_2021.xlsx")

### climate station "Trischübel" (H = 1766)
cs_tri <- read_excel("data_climate_stations/exdb/exdb_trischuebel_2021.xlsx")

### climate station "Watzmanngrat" (H = 2707)
cs_watzg <- read_excel("data_climate_stations/exdb/exdb_watzmanngrat_2021.xlsx")

### climate station "Watzmannhaus" (H = 1924)
cs_watzh <- read_excel("data_climate_stations/exdb/exdb_watzmannhaus_2021.xlsx")


###### next: merging all the loaded data_sets into one
cs_t_temp <- merge(cs_watzh[,1:2], cs_watzg[,1:2], by = "DATUM", all = T) 
names(cs_t_temp)[2:3] <- c("watzh_1924", "watzg_2707")

cs_t_temp <- merge(cs_t_temp, cs_tri[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "tri_1766"  

cs_t_temp <- merge(cs_t_temp, cs_schlu[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "schlu_2097"

# At Reiteralm, there are several climate stations at different heights:
cs_t_temp <- merge(cs_t_temp, cs_reit[,c(1,2,5,7)], by = "DATUM", all = T)
names(cs_t_temp)[6:8] <- c("reit_1679", "reit_1612", "reit_1756")

cs_t_temp <- merge(cs_t_temp, cs_hinters[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "hinters_832"

cs_t_temp <- merge(cs_t_temp, cs_hinterb[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "hinterb_2472"

cs_t_temp <- merge(cs_t_temp, cs_funt[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "funt_2521"

cs_t_temp <- merge(cs_t_temp, cs_brunft[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "brunft_1239"

cs_t_temp <- merge(cs_t_temp, cs_blau[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "blau_1649"

cs_t_temp <- merge(cs_t_temp, cs_bind[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "bind_1074"

cs_t_temp <- merge(cs_t_temp, cs_alp[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "alp_851"

cs_t_temp <- merge(cs_t_temp, cs_kueh2[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "kueh_1411"

# to get the date format as a string we use the following command:
cs_t_temp$DATUM <- strftime(cs_t_temp$DATUM, tz = "UTC", format = "%Y-%m-%d %H:%M")              

### the external climate station data is already in string format
cs_t_temp <- merge(cs_t_temp, cs_hoell[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "hoell_657"

# the cs_ stein df needs to be modified before it can be merged
cs_stein <- cs_stein[!complete.cases(cs_stein$Signal),]
cs_t_temp <- merge(cs_t_temp, cs_stein[,1:2], by = "DATUM", all = T)
names(cs_t_temp)[ncol(cs_t_temp)] <- "stein_1991"

### Following files are excluded:

# cs_kueh2_corr <- merge(cs_kueh2, cs_watzh, by = "DATUM")
# test <- merge(cs_t_temp, cs_kueh2_corr[,1:2], by = "DATUM", all = T)
# names(cs_t_temp)[ncol(cs_t_temp)] <- "kueh_1420"


# cs_t_temp <- merge(cs_t_temp, cs_stein[,c(1,4)], by = "DATUM", all = T)
# names(cs_t_temp)[ncol(cs_t_temp)] <- "stein_1893"   



# saving the data frame
write.csv(cs_t_temp, file="climate_stations_complete.csv", row.names = F)
cs_t_temp <- read.csv("climate_stations_complete.csv")#; cs_t_temp <- subset(cs_t_temp, select = -X)

### we can change the order according to station height 
cs_t_temp <- cs_t_temp[,c("DATUM", "hoell_657", "hinters_832", "alp_851", "bind_1074", "brunft_1239", "kueh_1411", "reit_1612", "blau_1649", "reit_1679", "reit_1756", "tri_1766", "watzh_1924", "stein_1991", "schlu_2097", "hinterb_2472", "funt_2521", "watzg_2707")] 

### only the months of june, july and august are of interest for this study
temp <- cs_t_temp[cs_t_temp$DATUM == "2021-09-01 00:00",] # this is the only date we want to include from September
cs_t_temp <- cs_t_temp[substr(cs_t_temp$DATUM, 6, 7) %in% c("06", "07", "08"),] 
cs_t_temp <- rbind(cs_t_temp, temp)
# here reordering is not necessary as there is only one row added, which is already in place

nrow(cs_t_temp[duplicated(cs_t_temp),]) 
# there are no duplicates in the data: the df has the exact number of entries one would expect
# the df contains the exact number of rows one would expect for the period calculating with 6 measurements per hour 

## following columns were non numeric
cs_t_temp$hoell_657 <- as.numeric(cs_t_temp$hoell_657)
cs_t_temp$stein_1991 <- as.numeric(cs_t_temp$stein_1991)

base::rm(list=setdiff(base::ls(), c("cs_t_temp", "mc_t")))#to remove unnecessary variables

### taking the average temperature for each hour like before (for the microclimate data)
nr <- nrow(cs_t_temp)
nc <- ncol(cs_t_temp)
cs_t <- data.frame(matrix(nrow=(nr/6), ncol=nc))
names(cs_t) <- names(cs_t_temp)
index <- 1 # index variable to fill the new df
tic()
for (i in 1:nr){
  if (substr(cs_t_temp$DATUM[i], 15, 16) == "00"){ # we want to summarize from :00 to :00 to get identical periods as in the climate station data
    #for the date in the next line, the mean is not taken, because the data for each plot ends here. Therefore the if loop is exited
    if (cs_t_temp$DATUM[i] == "2021-09-01 00:00"){
      # do nothing in this case
    }
    else{
      cs_t$DATUM[index] <- substr(cs_t_temp$DATUM[i], 1, 13) #last 3 characters are removed leaving numbers from 0-23 indicating the hours for each day
      for (j in 2:nc){ # for each climate station we take the mean for each hour
        cs_t[index, j] <- mean(cs_t_temp[i:(i+6), j])
      }  
      index <- index +1 
    }
  }
}
toc()
cs_t[, 2:ncol(cs_t)] <- round(cs_t[, 2:ncol(cs_t)], 1) # the result is rounded to contain 1 digit as in the original data


write.csv(cs_t, file="climate_stations_hourly.csv")
cs_t <- read.csv("climate_stations_hourly.csv"); cs_t <- subset(cs_t, select = -X)



base::rm(list=setdiff(base::ls(), c("cs_t", "mc_t")))#to remove unnecessary variables


###### the climate station data needs to be interpolated for each forest plot

### a vector with the heights of the stations in the same order as cs_t 
# h <- c(657, 832, 851, 1074, 1239, 1612, 1649, 1679, 1756, 1766, 1924, 1991, 2097, 2472, 2521, 2707)
# the tests below have shown that 3 climate stations need to be removed from the data, because they worsen the models
h <- c(657, 832, 851, 1074, 1239, 1612, 1649, 1679, 1766, 1924, 2097, 2472, 2521, 2707) 
cs_t <- subset(cs_t, select = -c(reit_1756, stein_1991, kueh_1411)) # those are the three stations that need to be removed

###### first we see how well the temperature correlates with height using linear models
nr <- nrow(cs_t)
###### tests over the entire period

x <- c()
##
for (i in seq(50, nr, 100)){
  mod <- lm(as.numeric(cs_t[i, 2:ncol(cs_t)]) ~ h)
  print("###################")
  print(cs_t$DATUM[i])  
  print("###################")
  print(paste0("### R² = ", summary(mod)$r.squared)) 
  print("### Residuals: "); print(residuals(mod)) 
  x <- c(x, summary(mod)$r.squared)
  x11()
  plot(as.numeric(cs_t[i, 2:ncol(cs_t)]) ~ h, main = paste0(cs_t$DATUM[i]))
  abline(mod) 
}
##
for (i in seq(1, nr, 100)){
  mod <- lm(as.numeric(cs_t[i, 2:ncol(cs_t)]) ~ h)
  print("###################")
  print(cs_t$DATUM[i])  
  print("###################")
  print(paste0("### R² = ", summary(mod)$r.squared)) 
  print("### Residuals: "); print(residuals(mod)) 
  x <- c(x, summary(mod)$r.squared)
  x11()
  plot(as.numeric(cs_t[i, 2:ncol(cs_t)]) ~ h, main = paste0(cs_t$DATUM[i]))
  abline(mod) 
}
### the test results are predominantely good

####### Interpolation for all 150 forest plots

# in this table the height information for each forest plot is contained
plot_info <- read.csv("Forest_Plot_Info/NP_BGD_Biodiv_Alm_study_sites_UTM32.csv", sep=";", dec=".")
lid_dat_12.6 <- read.csv("lidar_metrics_12_6.csv", sep=",", dec="."); lid_dat_12.6 <- subset(lid_dat_12.6, select =  -X)
id <- lid_dat_12.6$Plot_ID #list with the ids of the forest plots
rm(lid_dat_12.6) # this was all we needed the file for
# 
cs_t_interp <- data.frame(matrix(nrow = 150*nrow(cs_t), ncol=3))
names(cs_t_interp) <- c("Plot_ID", "DATUM", "t_macro")
cs_t_interp$DATUM <- rep(cs_t$DATUM, 150)
row_index <- 0
nr <- nrow(cs_t)
nc <- ncol(cs_t)

####### it was supposed to be easier this way, but I can't figure out, why it does not work...
tic()
for (i in 1:150){
  new <- data.frame(h = plot_info$H[plot_info$Plot== id[i]])
  cs_t_interp$Plot_ID[(row_index+1):(row_index+nr)] <- id[i]
  for (j in 1:nr){
    mod <- lm(as.numeric(cs_t[j , 2:nc]) ~ h)
    cs_t_interp$t_macro[row_index + j] <- predict(mod, newdata = new)
  }
  row_index <- row_index + nr
}
toc()
# 2057.32 sec elapsed


write.csv(cs_t_interp, "macro_temperature_interpolated.csv")
cs_t_interp <- read.csv("macro_temperature_interpolated.csv"); cs_t_interp <- subset(cs_t_interp, select = -X)


cs_t_interp[is.na(cs_t_interp$t_macro),] 
# there are no NAs

base::rm(list=setdiff(base::ls(), c("mc_t", "cs_t_interp")))#to remove unnecessary variables
mc_t[is.na(mc_t$T3),] # there are no NAs

dat_t <- merge(cs_t_interp, mc_t[,c(1,2,5)], by.x = c("Plot_ID", "DATUM"))


### finally we can calculate the temperature offset t_macro - t_micro


write.csv(dat_t, "temperature_data.csv", row.names = F)
dat_t <- read.csv("temperature_data.csv")

base::rm(list=setdiff(base::ls(), c("dat_t")))#to remove unnecessary variables

