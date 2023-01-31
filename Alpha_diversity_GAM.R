# THIRD DATA PARTITIONING #####
data = read_csv("data_filtered.csv")
orig_data = data %>% dplyr::filter(Gear %in% c("3271","3270"),
                                   Bot_depth < 700,
                                   Longitude > -2) %>% 
  mutate(Site = paste(Longitude, Latitude, Year, Month, Bot_depth, swept_area))

length(unique(all_data$valid_name)) # 193 species 

boxplot(all_data$swept_area ~ all_data$Year) # Negative trend of swept area with time

all_data$swept_area = all_data$swept_area/10000

all_sites = all_data %>% dplyr::select(valid_name,Year, Longitude, Latitude, swept_area, Bot_depth, Region) %>% 
  distinct() %>% group_by(Year, Longitude, Latitude, swept_area, Bot_depth, Region) %>%
  summarise(Species = n())

### Data exploration Figure S1 ####
gea = ggplot(all_sites) + geom_histogram(aes(Year),col ="white", binwidth = 1) + facet_wrap(.~ Region, scale=  "free",ncol = 5) +theme(legend.position = "none")
Lat = ggplot(all_sites) + geom_boxplot(aes(x = Year, y = Latitude, group = Year), outlier.shape = NA) + facet_wrap(.~ Region, scale=  "free",ncol = 5)
eff = ggplot(all_sites) + geom_boxplot(aes(x = Year, y = swept_area, group = Year), outlier.shape = NA) + facet_wrap(.~ Region, scale=  "free",ncol = 5)

pdf("Figure S1.pdf", width = 8, height = 6)
grid.arrange(Lat, eff, ncol = 1)
dev.off()

# Data for modelling BRT
all_data = all_data %>% group_by(Year, Longitude, Latitude, swept_area, Bot_depth) %>% summarise(Species = n()) %>% rename(Depth = Bot_depth)
write.csv(all_data, "data_ready_for_brt.csv")

### Regional hauls Figure 1 ######

main_region_data = orig_data %>% filter(Region == "Norwegian-Barents Sea")
north_data = orig_data %>% filter(Region == "North Sea")
sva_data = orig_data %>% filter(Region == "Svalbard")


main_region_sites = main_region_data %>%ungroup() %>%  dplyr::select(Year, Longitude, Latitude) %>% distinct()

pdf("Sites_main_region.pdf", width = 1.3, height = 0.95)

ggplot(main_region_sites) + geom_histogram(aes(Year), fill = "#d5b43c", col ="white", size = 0.2, binwidth = 1) + 
  
  ylab("# of trawls") +
  scale_x_continuous(breaks = seq(1995, 2020, 10)) + 
  theme(axis.text = element_text(size = 6),
        legend.position = "none",
        axis.title = element_text(size = 6),
        axis.title.x = element_text(vjust= 6),
        axis.title.y = element_text(vjust= -4.5),
        axis.text.x = element_text(vjust = 3),
        axis.text.y = element_text(hjust = 1.5),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.3, "mm"),
        
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = - 8,  # Bottom margin
                             l = - 7)) # Left margin

dev.off()




svalb_sites = svalb_data %>% dplyr::select(Year, Month, Longitude, Latitude) %>% distinct()

pdf("Sites_svalb.pdf", width = 1.3, height = 0.95)

ggplot(svalb_sites) + geom_histogram(aes(Year), fill = "#d5b43c", col ="white", size = 0.2, binwidth = 1) + 
  scale_x_continuous(breaks = seq(1995, 2020, 10)) + 
  ylab("# of trawls") +
  theme(axis.text = element_text(size = 6),
        legend.position = "none",
        axis.title = element_text(size = 6),
        axis.title.x = element_text(vjust= 6),
        axis.title.y = element_text(vjust= -4.5),
        axis.text.x = element_text(vjust = 3),
        axis.text.y = element_text(hjust = 1.5),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.3, "mm"),
        
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = - 8,  # Bottom margin
                             l = - 7)) # Left margin

dev.off()


north_sites = north_data %>% dplyr::select(Year, Month, Longitude, Latitude) %>% distinct()

pdf("Sites_north.pdf", width = 1.3, height = 0.95)

ggplot(north_sites) + geom_histogram(aes(Year), fill = "#d5b43c", col ="white", size = 0.2, binwidth = 1) + 
  
  ylab("# of trawls") +
  scale_x_continuous(breaks = seq(1995, 2020, 10)) + 
  theme(axis.text = element_text(size = 6),
        legend.position = "none",
        axis.title = element_text(size = 6),
        axis.title.x = element_text(vjust= 6),
        axis.title.y = element_text(vjust= -4.5),
        axis.text.x = element_text(vjust = 3),
        axis.text.y = element_text(hjust = 1.5),
        axis.ticks = element_line(size = 0.2),
        axis.ticks.length = unit(0.3, "mm"),
        
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = - 8,  # Bottom margin
                             l = - 7)) # Left margin

dev.off()

### ALPHA DIVERSITY MAIN REGION ####


all_rich = main_region_data %>% 
  dplyr::select(Site, Year, valid_name, swept_area, Bot_depth, Longitude, Latitude, Total_abund) %>% 
  distinct() %>% group_by(Year, Site, swept_area, Bot_depth, Longitude, Latitude) %>% 
  summarise(Abund_trawl = sum(Total_abund),
            #Richness = 100 * n()/Abund_trawl,
            Richness = n(),
            ID = cur_group_id()) %>% group_by(Year) %>% mutate(annual_trawls = n())


boxplot(swept_area ~ Year, data = all_rich)

model_all <- gam(Richness ~ s(Year, k = 7) + log(swept_area) + s(Latitude, k = 4), data = all_rich, family = poisson)

summary(model_all)
par(mfrow = c(2,3)); plot(model_all)
par(mfrow = c(2,2)); gam.check(model_all);abline(1,1,col = "red")

plot_model(model_all, type = "pred", terms = c("Year"))
plot_model(model_all, type = "pred", terms = c("swept_area"))
plot_model(model_all, type = "pred", terms = c("Abund_trawl"))

data = plot_model(model_all, type = "pred", terms = c("Year"))
data$data
data$data$conf.low 
data$data$conf.high
data$data$predicted
cor.test(predict(model_all), all_rich$Richness) 
data.frame(data$data)


100 * (data$data$conf.low[27]  - data$data$conf.high[1]) / data$data$conf.high[1]  
100 * (data$data$predicted[27] - data$data$predicted[1]) / data$data$predicted[1]
100 * (data$data$conf.high[27] - data$data$conf.low[1]) /  data$data$conf.low[1]  


# SBT

geom_p_main = main_region_data %>% dplyr::select(Longitude, Latitude) %>% distinct()
coordinates(geom_p_main) = geom_p_main[,c(1:2)]
crs(geom_p_main) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
buffer_main = raster::buffer(geom_p_main, width = 100000)
plot(buffer_main)

SBT <- raster::stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SBT/SBT_mean_area.tif")
SBT <- crop(SBT, buffer_main)
SBT <- raster::as.data.frame(SBT, xy = T)
names(SBT)[3:29] = c(1993:2019)
SBT = gather(SBT, key = "Year", value = "SBT", 3:29)
SBT$Year = as.numeric(SBT$Year)
SBT = SBT %>% group_by(Year, x,y) %>% summarise(SBT = mean(SBT)) # Annual mean per site



se <- function(x) { sd(x, na.rm = T) / sqrt(length(x))}


all_rich_SBT = all_rich %>% group_by(Year) %>% summarise(mean_rich = mean(Richness),
                                                         se_rich = se(Richness))

SBT_all = SBT %>% group_by(Year) %>% summarise(SBT_mean = mean(SBT,na.rm = T), se = se(SBT))

a = ggplot() + 
  geom_line(data = all_rich_SBT, aes(x = Year, y = mean_rich), group = 1, col = "black", size = 1.2) + 
  geom_ribbon(data = all_rich_SBT, aes(x = Year, y = mean_rich, ymax = mean_rich + 2*se_rich, ymin = mean_rich - 2*se_rich), alpha = 0.1) + 
  geom_line(data = SBT_all, aes(x = Year, y = SBT_mean * 4 +3 ), group = 1, col = "brown3", size = 1.2) +
  geom_ribbon(data = SBT_all, aes(x = Year, y = SBT_mean * 4 +3 ,
                                  ymax = (SBT_mean * 4 + 3) + 2* (2* se),
                                  ymin = (SBT_mean * 4 + 3) - 2* (2* se)), group = 1, alpha = 0.1) +
  geom_line(data = as.data.frame(data$data), aes(x = x, y = predicted), col = "darkgrey") + 
  geom_ribbon(data = as.data.frame(data$data), aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  scale_y_continuous(name = expression("Richness"),
                     breaks = c(6,8,10,12,14,16),limits = c(6,17),
                     sec.axis = sec_axis(~./4 - 3/4, name = "")) +
  
  scale_x_continuous(breaks = seq(1995, 2020, 5)) + 
  xlab("Year") + ggtitle("") +
  theme_bw() +
  theme(axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 9),
        axis.title.y = element_blank(),
        panel.grid.minor = element_line(color = "darkgrey"), panel.grid.major = element_line(color = "darkgrey"))

pdf("Alpha_all_15_11_2022.pdf", width = 2.5, height = 2.5)
a
dev.off()

SBT_all$Year = as.double(SBT_all$Year)
SBT_rich_all = left_join(SBT_all, all_rich_SBT)
SBT_rich_all %>% print(n = 27)
cor.test(SBT_rich_all$SBT_mean, SBT_rich_all$mean_rich)  #Correlated 0.59

### ALPHA DIVERSITY NORTH SEA ####


north_rich = north_data %>% dplyr::select(Site, Year, valid_name, swept_area, Gear, Bot_depth, Latitude, Longitude, Month) %>% #filter(Gear == "3271") %>%
  distinct() %>% group_by(Month, Latitude, Longitude, Year, Site, swept_area, Gear, Bot_depth) %>% 
  summarise(Richness = n()) %>% mutate(Gear = as.factor(Gear), Month = as.factor(Month)) %>% group_by(Year) %>% mutate(annual_trawls = n())


boxplot(swept_area ~ Year, data = north_rich)

model_north <- gam(Richness ~ s(Year, k = 7) + s(Latitude, k = 4) +
                     log(swept_area), data = north_rich, family = poisson)

summary(model_north)
par(mfrow = c(2,3)); plot(model_north)
par(mfrow = c(2,2)); gam.check(model_north);abline(1,1,col = "red")

plot_model(model_north, type = "pred", terms = c("Year"))
plot_model(model_north, type = "pred", terms = c("swept_area"))

data = plot_model(model_north, type = "pred", terms = c("Year"))

data$data
data$data$conf.low #42% increase overnorth
data$data$conf.high


100 * (data$data$conf.low[22]  - data$data$conf.high[1]) / data$data$conf.high[1]  
100 * (data$data$predicted[22] - data$data$predicted[1]) / data$data$predicted[1]
100 * (data$data$conf.high[22] - data$data$conf.low[1]) /  data$data$conf.low[1]  



data$data$predicted
cor.test(predict(model_north), north_rich$Richness) 
data.frame(data$data)



# SBT
geom_p_north = north_data %>% ungroup() %>% dplyr::select(Longitude, Latitude) %>% distinct()
coordinates(geom_p_north) = geom_p_north[,c(1:2)]
crs(geom_p_north) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
buffer_north = raster::buffer(geom_p_north, width = 100000)
plot(buffer_north)


SBT <- raster::stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SBT/SBT_mean_area.tif")
SBT <- crop(SBT, buffer_north)
SBT <- raster::as.data.frame(SBT, xy = T)
names(SBT)[3:29] = c(1993:2019)
SBT = gather(SBT, key = "Year", value = "SBT", 3:29)
SBT$Year = as.numeric(SBT$Year)
se <- function(x) { sd(x, na.rm = T) / sqrt(length(x))}


north_rich_SBT = north_rich %>% group_by(Year) %>% summarise(mean_rich = mean(Richness),
                                                             se_rich = se(Richness))

SBT_north = SBT %>% group_by(Year) %>% summarise(SBT_mean = mean(SBT,na.rm = T), se = se(SBT))


a = ggplot() + 
  geom_line(data = filter(north_rich_SBT, Year < 2003), aes(x = Year, y = mean_rich), group = 1, col = "black", size = 1.2) + 
  geom_ribbon(data = filter(north_rich_SBT, Year < 2003), aes(x = Year, y = mean_rich, ymax = mean_rich + 2*se_rich, ymin = mean_rich - 2*se_rich), alpha = 0.1) + 
  
  geom_line(data = filter(north_rich_SBT, Year > 2003), aes(x = Year, y = mean_rich), group = 1, col = "black", size = 1.2) + 
  geom_ribbon(data = filter(north_rich_SBT, Year > 2003), aes(x = Year, y = mean_rich, ymax = mean_rich + 2*se_rich, ymin = mean_rich - 2*se_rich), alpha = 0.1) +   
  
  geom_line(data = SBT_north, aes(x = Year, y = SBT_mean * 4 - 22), group = 1, col = "brown3", size = 1.2) +
  geom_ribbon(data = SBT_north, aes(x = Year, y = SBT_mean * 4 - 22,
                                    ymax = (SBT_mean * 4 + - 22) + 4* (2* se),
                                    ymin = (SBT_mean * 4 + - 22) - 4* (2* se)), group = 1, alpha = 0.1) +
  geom_line(data = as.data.frame(data$data), aes(x = x, y = predicted), col = "darkgrey") + 
  geom_ribbon(data = as.data.frame(data$data), aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  scale_y_continuous(name = expression("Richness"),
                     breaks = c(6,8,10,12,14,16,18,20),limits = c(4,19),
                     sec.axis = sec_axis(~./4 + 22/4, name = "")) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), limits = c(1994, 2020)) + 
  xlab("Year") + ggtitle("") +
  theme_bw() +
  theme(axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 9),
        axis.title.y = element_blank(),
        panel.grid.minor = element_line(color = "darkgrey"), panel.grid.major = element_line(color = "darkgrey"))

pdf("Alpha_north.pdf", width = 2.5, height = 2.5)
a
dev.off()

SBT_north$Year = as.double(SBT_north$Year)
SBT_rich_north = right_join(SBT_north, north_rich_SBT)

cor.test(SBT_rich_north$SBT_mean, SBT_rich_north$mean_rich)  #Correlated 0.23, not significant

### ALPHA DIVERSITY SVALBARD ####


svalb_rich = svalb_data %>% dplyr::select(Site, Year, valid_name, swept_area, Gear, Bot_depth, Latitude, Longitude, Month) %>% #filter(Gear == "3271") %>%
  distinct() %>% group_by(Month, Latitude, Longitude, Year, Site, swept_area, Gear, Bot_depth) %>% 
  summarise(Richness = n()) %>% mutate(Gear = as.factor(Gear), Month = as.factor(Month)) %>% group_by(Year) %>% mutate(annual_trawls = n())


boxplot(swept_area ~ Year, data = svalb_rich)

model_svalb <- gam(Richness ~ s(Year, k = 7),
                  # +s(Latitude, k = 4) + log(swept_area), 
                     data = svalb_rich, family = poisson)

summary(model_svalb)
par(mfrow = c(2,3)); plot(model_svalb)
par(mfrow = c(2,2)); gam.check(model_svalb);abline(1,1,col = "red")

plot_model(model_svalb, type = "pred", terms = c("Year"))
plot_model(model_svalb, type = "pred", terms = c("swept_area"))

data = plot_model(model_svalb, type = "pred", terms = c("Year"))

data$data
data$data$conf.low 
data$data$conf.high 
data$data$predicted
cor.test(predict(model_svalb), svalb_rich$Richness) 
data.frame(data$data)


100 * (data$data$conf.low[22]  - data$data$conf.high[1]) / data$data$conf.high[1]  
100 * (data$data$predicted[22] - data$data$predicted[1]) / data$data$predicted[1]
100 * (data$data$conf.high[22] - data$data$conf.low[1]) /  data$data$conf.low[1]  


# SBT
geom_p_svalb = svalb_data %>% dplyr::select(Longitude, Latitude) %>% distinct()
coordinates(geom_p_svalb) = geom_p_svalb[,c(1:2)]
crs(geom_p_svalb) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")

buffer_svalb = raster::buffer(geom_p_svalb, width = 100000)
plot(buffer_svalb)


SBT <- raster::stack("C:/Users/06061016/Documents/Treball/Costello/IMR1980-2017/Envars/SBT/SBT_mean_area.tif")
SBT = crop(SBT, buffer_svalb)


SBT <- raster::as.data.frame(SBT, xy = T)
names(SBT)[3:29] = c(1993:2019)
SBT = gather(SBT, key = "Year", value = "SBT", 3:29)
SBT$Year = as.numeric(SBT$Year)
se <- function(x) { sd(x, na.rm = T) / sqrt(length(x))}

as.data.frame(data$data)

svalb_rich_SBT = svalb_rich %>% group_by(Year) %>% summarise(mean_rich = mean(Richness),
                                                             se_rich = se(Richness))

SBT_svalb = SBT %>% group_by(Year) %>% summarise(SBT_mean = mean(SBT,na.rm = T), se = se(SBT))


filter(SBT_svalb, Year > 1994, Year < 2019)

a = ggplot() + 
  geom_line(data = filter(svalb_rich_SBT, Year < 1999), aes(x = Year, y = mean_rich), group = 1, col = "black", size = 1.2) +
  geom_ribbon(data = filter(svalb_rich_SBT, Year < 1999), aes(x = Year, y = mean_rich, ymax = mean_rich + 2*se_rich, ymin = mean_rich - 2*se_rich), alpha = 0.1) + 
  
  geom_line(data = filter(svalb_rich_SBT, Year > 1999 & Year < 2014), aes(x = Year, y = mean_rich), group = 1, col = "black", size = 1.2) + 
  geom_ribbon(data =filter(svalb_rich_SBT, Year > 1999 & Year < 2014), aes(x = Year, y = mean_rich, ymax = mean_rich + 2*se_rich, ymin = mean_rich - 2*se_rich), alpha = 0.1) + 
  
  geom_line(data = filter(svalb_rich_SBT, Year > 2015), aes(x = Year, y = mean_rich), group = 1, col = "black", size = 1.2) + 
  geom_ribbon(data = filter(svalb_rich_SBT, Year > 2015), aes(x = Year, y = mean_rich, ymax = mean_rich + 2*se_rich, ymin = mean_rich - 2*se_rich), alpha = 0.1) + 
  
  geom_point(data = filter(svalb_rich_SBT, Year  == 2015), aes(x = Year, y = mean_rich), group = 1, col = "black", size = 1.2) + 
  
  
  geom_line(data = SBT_svalb, aes(x = Year, y = SBT_mean * 4 + 5), group = 1, col = "brown3", size = 1.2) +
  geom_ribbon(data = SBT_svalb, aes(x = Year, y = SBT_mean * 4 + 3,
                                    ymax = (SBT_mean * 4 + 5) + 2* (2* se),
                                    ymin = (SBT_mean * 4 + 5) - 2* (2* se)), group = 1, alpha = 0.1) +
  geom_line(data = as.data.frame(data$data), aes(x = x, y = predicted), col = "darkgrey") + 
  geom_ribbon(data = as.data.frame(data$data), aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  scale_y_continuous(name = expression("Richness"),
                     breaks = c(6,8,10,12,14,16),limits = c(4,16),
                     sec.axis = sec_axis(~./4 - 5/4, name = "")) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), limits = c(1994, 2020)) + 
  xlab("Year") + ggtitle("") +
  theme_bw() +
  theme(axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 9),
        axis.title.y = element_blank(),
        panel.grid.minor = element_line(color = "darkgrey"), panel.grid.major = element_line(color = "darkgrey"))

pdf("Alpha_svalb.pdf", width = 2.5, height = 2.5)
a
dev.off()

SBT_svalb$Year = as.double(SBT_svalb$Year)
SBT_rich_svalb = right_join(SBT_svalb, svalb_rich_SBT)
cor.test(SBT_rich_svalb$SBT_mean, SBT_rich_svalb$mean_rich)  #No correlation



