### 1. Library 

library(tidyverse)
library(rgdal)

# DATA LOAD, OUTPUT FROM "Data exploration & GAM model" 

richness_data = read_csv("richness_all_sites.csv")


### 2. Filter data by Years and Depth 

richness_data = richness_data %>% filter(Year > 1992, Year < 2020)
richness_data = richness_data %>% filter(Bot_depth < 700) # 557 points eliminated, but way better model. 

### 3. LOAD ENVIRONMENTAL DATA 


atemporal_layers <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/Environmental variables/atemporal_layers.grd")

## LOAD ENVIRONMENTAL-TEMPORAL DATA FROM COPERNICUS ##

SST <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SST/SST_mean_area.tif")
SBT <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SBT/SBT_mean_area.tif")
SSS <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SSS/SSS_mean_area.tif")
NWC <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/NWC/NWC_mean_area.tif")
EWC <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/EWC/EWC_mean_area.tif")
ICC <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/ICE_CON/ICC_mean_area.tif")
ICT <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/ICE_THICK/ICT_mean_area.tif")



# 4. EXTRACT DATA PER TRAWLING EVENT # 

results = list()

for (i in 1:27){
  data  = richness_data %>% filter(Year == (i+1992))
  data$SST = raster::extract(SST[[i]],data[,c("Longitude","Latitude")])
  data$SBT = raster::extract(SBT[[i]],data[,c("Longitude","Latitude")])
  data$SSS = raster::extract(SSS[[i]],data[,c("Longitude","Latitude")])
  data$NWC = raster::extract(NWC[[i]],data[,c("Longitude","Latitude")])
  data$EWC = raster::extract(EWC[[i]],data[,c("Longitude","Latitude")])
  data$ICC = raster::extract(ICC[[i]],data[,c("Longitude","Latitude")])
  data$ICT = raster::extract(ICT[[i]],data[,c("Longitude","Latitude")])
  atemporal_data = data.frame(raster::extract(atemporal_layers, data[,c("Longitude", "Latitude")]))
  results[[i]] = data.frame(atemporal_data[,-c(13:15)], data)
  print(paste("Iteration ", i, " out of ", 27 , sep = ""))
}

df_alpha = do.call(rbind, results)

data = na.omit(df_alpha)
data$Depth = -data$Depth # Bathymetry layer is with the negative sign. To make it analogous, I transform the "Depth variable.

### 5. SAVE THE DATASET

write.csv(data, "data_ready_for_brt.csv")
