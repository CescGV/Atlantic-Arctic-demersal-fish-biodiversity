###### Library ###### ####
library(tidyverse)

data <- read.csv("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Data/total_completed_data.csv")


colnames(data)
#Time duration differs from calculated time duration... 
data %>% select(Start_time, End_time, Duration) %>% mutate(diff = (End_time - Start_time)/60)

# Quality selection -------------------------------------------------------
data_quality <- data %>% filter(Quality_gear < 3) # 641688
data_quality <- data_quality %>% filter(Quality_haul < 3)#629443
data_quality <- data_quality %>% filter(Total_abund  > 0)#600459
data_quality <- data_quality %>% filter(Total_weight > 0)#561335
data_quality <- data_quality %>% filter(Opening      > 0)#490228
data_quality <- data_quality %>% filter(Bot_depth    > 0)#489572
data_quality <- data_quality %>% filter(Distance     > 0)#488793
data_quality <- data_quality %>% filter(Year         > 1988)# 472464

data_quality <- data_quality %>% 
  filter(Measurement == 1 | Measurement == 5 | Measurement == 6 | Measurement == 9) # 472344

# Gear Type selection: Shrimp trawl -------------------------------------------------------
shrimp_trawl <- data_quality %>% 
  mutate(Distance_in_m = data_quality$Distance *1852,
         swept_area = Opening * Distance_in_m)  %>% 
  filter(Gear %in% c(3270, 3271), 
         Opening < 60, Distance_in_m < 5000) # 314650


### Keep only fish species - Select tax groups -----
shrimp_trawl_fish <- shrimp_trawl %>% filter(grepl(' ', valid_name), class %in% c("Actinopterygii",
                                                                                  "Elasmobranchii", 
                                                                                  "Holocephali",
                                                                                  "Myxini",
                                                                                  "Petromyzonti")) # 253933
### I aggregate species found across time ------

preparation_sp = shrimp_trawl_fish %>%
  group_by(Gear, Bot_depth, Year, swept_area,valid_name, Longitude, Latitude, Month) %>% 
  summarise(Total_abund = sum(Total_abund))

orig_data = preparation_sp %>% # 252 056
  dplyr::filter(Bot_depth < 700, 
                Longitude > -2) %>%
  mutate(Site = paste(Longitude, Latitude, Year, swept_area, Month))


# Regional partitioning of the data

svalb_data = orig_data %>% filter(Latitude > 77, !(Year %in% c(1994, 1995, 1999, 2014, 2016))) %>% mutate(Region = "Svalbard")
ggplot(svalb_data) + geom_point(aes(x = Longitude, y = Latitude)) + facet_wrap(.~ Year)

main_region_data = orig_data %>% filter(Latitude <= 77, Latitude > 62) %>% mutate(Region = "Norwegian-Barents Sea")
ggplot(main_region_data) + geom_point(aes(x = Longitude, y = Latitude), size = 1.2) + facet_wrap(.~ Year)

north_data = orig_data %>% filter(Latitude <= 62, Latitude > 56, !(Year %in%c(1994, 2003))) %>% mutate(Region = "North Sea")
ggplot(north_data) + geom_point(aes(x = Longitude, y = Latitude)) + facet_wrap(.~ Year)

all_data = rbind(svalb_data, main_region_data, north_data)

length(unique(all_data$valid_name)) # 193 species
length(unique(all_data$Site)) # 20 670 sites


write.csv(all_data, "data_filtered.csv")
