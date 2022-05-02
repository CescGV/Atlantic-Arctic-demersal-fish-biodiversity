### 1. Library 

library(tidyverse)
library(rgdal)

# DATA LOAD, OUTPUT FROM "Data exploration & GAM model" 

richness_data = read_csv("richness_all_sites.csv")


### 2. Filter data by Years and Depth 

richness_data = richness_data %>% filter(Year > 1992, Year < 2020)
richness_data = richness_data %>% filter(Bot_depth < 700) # 557 points eliminated, but way better model. 

### 3. LOAD ENVIRONMENTAL DATA 

dis <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/Dist_coast/distance_to_coast.tif")
layers <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/Bio-Oracle/layers.grd")
no_balt = readOGR("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/Dist_coast/study_area.shp")
names(layers) = c("Bathymetry","Salinity","Currents","DO"," Sea.Ice","Iron","Nitrate","Prod.Bottom","Prod.Surface", "Temp.Bottom", "Surface.Temp","Temp.Range")

# Crop and mask layers to eliminate the baltic sea area, and to standardise them

layers <- crop(layers, no_balt)
layers <- raster::mask(layers, no_balt)

dis <- raster::projectRaster(dis, layers[[1]], method = "bilinear")
dis[is.na(dis)] <-0

layers_p = stack(layers, dis)

## LOAD ENVIRONMENTAL-TEMPORAL DATA FROM COPERNICUS ##

SST <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SST/SST_mean_area.tif")
SBT <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SBT/SBT_mean_area.tif")
SSS <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SSS/SSS_mean_area.tif")
NWC <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/NWC/NWC_mean_area.tif")
EWC <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/EWC/EWC_mean_area.tif")
ICC <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/ICE_CON/ICC_mean_area.tif")
ICT <- stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/ICE_THICK/ICT_mean_area.tif")



#crs(NWC) = "+proj=longlat +datum=WGS84 +no_defs"
#crs(EWC) = "+proj=longlat +datum=WGS84 +no_defs"
#crs(SSS) = "+proj=longlat +datum=WGS84 +no_defs"

#NWC = raster::projectRaster(NWC, layers[[1]], method = "bilinear")
#EWC = raster::projectRaster(EWC, layers[[1]], method = "bilinear")
#SSS = raster::projectRaster(SSS, layers[[1]], method = "bilinear")
#SST = raster::projectRaster(SST, layers[[1]], method = "bilinear")
#SBT = raster::projectRaster(SBT, layers[[1]], method = "bilinear")
#ICC = raster::projectRaster(ICC, layers[[1]], method = "bilinear")
#ICT = raster::projectRaster(ICT, layers[[1]], method = "bilinear")
#
#ICC[is.na(ICC[])] <- 0 
#ICT[is.na(ICT[])] <- 0 
#
#writeRaster(NWC,"C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/NWC/NWC_mean_area.tif" )
#writeRaster(EWC,"C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/EWC/EWC_mean_area.tif" )
#writeRaster(SSS,"C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SSS/SSS_mean_area.tif" )
#writeRaster(SST,"C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SST/SST_mean_area.tif" )
#writeRaster(SBT,"C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SBT/SBT_mean_area.tif" )
#writeRaster(ICC,"C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/ICE_CON/ICC_mean_area.tif" )
#writeRaster(ICT,"C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/ICE_THICK/ICT_mean_area.tif" )
#


# 4. EXTRACT DATA PER TRAWLING EVENT # 

results = list()

for (i in 1:28){
  data  = richness_data %>% filter(Year == (i+1992))
  data$SST = raster::extract(SST[[i]],data[,c("Longitude","Latitude")])
  data$SBT = raster::extract(SBT[[i]],data[,c("Longitude","Latitude")])
  data$SSS = raster::extract(SSS[[i]],data[,c("Longitude","Latitude")])
  data$NWC = raster::extract(NWC[[i]],data[,c("Longitude","Latitude")])
  data$EWC = raster::extract(EWC[[i]],data[,c("Longitude","Latitude")])
  data$ICC = raster::extract(ICC[[i]],data[,c("Longitude","Latitude")])
  data$ICT = raster::extract(ICT[[i]],data[,c("Longitude","Latitude")])
  
  results[[i]] = data
  print(paste("Iteration ", i, " out of ", 27 , sep = ""))
}

df_alpha = do.call(rbind, results)

# df_alpha contains environmental data with temporal resolution, and layers_p contains environmental data
# from Bio Oracle

data <-  data.frame(raster::extract(layers_p,df_alpha[,c("Longitude", "Latitude")]),
                    Species    = df_alpha$richness,
                    Year       = df_alpha$Year,
                    Depth      = df_alpha$Bot_depth,
                    Longitude  = df_alpha$Longitude,
                    Latitude   = df_alpha$Latitude,
                    swept_area = df_alpha$swept_area,
                    SST        = df_alpha$SST, 
                    SBT        = df_alpha$SBT,
                    SSS        = df_alpha$SSS,
                    NWC        = df_alpha$NWC,
                    EWC        = df_alpha$EWC,
                    ICC        = df_alpha$ICC,
                    ICT        = df_alpha$ICT)

data = na.omit(data)
data$Depth = -data$Depth # Bathymetry layer is with the negative sign. To make it analogous, I transform the "Depth variable.

### 5. SAVE THE DATASET

write.csv(data, "data_ready_for_brt.csv")
