data = read_csv("data_filtered.csv")
orig_data = data %>%  mutate(Site = paste(Longitude, Latitude, Year, Month, Bot_depth, swept_area))

main_region_data = orig_data %>% filter(Region == "Norwegian-Barents Sea")
north_data = orig_data %>% filter(Region == "North Sea")
sva_data = orig_data %>% filter(Region == "Svalbard")



### GAMMA DIVERSITY MAIN REGION ####

effort_main = main_region_data %>% dplyr::select(Site, swept_area, Year) %>% distinct() %>% group_by(Year) %>% 
  summarise(mean_eff = mean(swept_area),
            total_eff = sum(swept_area))


df_alpha_years = list()

sac_curves_random <- function(data, Years) {for (i in Years) {
  
  df_alpha_i <- data %>% filter(Year == i) 
  table_i    <- xtabs(Total_abund ~ Site + valid_name,df_alpha_i)
  SAC_i      <- specaccum(table_i, method = "exact", permutations = 1)
  data_df_i <- data.frame(Sites=SAC_i$sites, Richness=SAC_i$richness, SD=SAC_i$sd,points = rownames(table_i), Year = i)
  
  df_alpha_years[[i-(min(Years)-1)]] = data_df_i
  print(paste("Year", i, "to 2020"))
} 
  do.call(rbind, df_alpha_years)
}


all_SAC_l = list() 

for (i in 1:200){
  
  all_SAC_l[[i]] = sac_curves_random(data = main_region_data, Years = unique(main_region_data$Year))
  all_SAC_l[[i]]$it = i
  print(paste("This is iteration", i))
}

all_sac = do.call(rbind,all_SAC_l)
all_SAC = all_sac %>% group_by(Sites, Year) %>% summarise(SD = sd(Richness),Richness = mean(Richness))

all_SAC %>% dplyr::select(Year, Sites) %>% distinct() %>% group_by(Year) %>% count() %>% arrange(n)

#all_SAC = filter(all_SAC, Year > 1997)
labels_complet_all = all_SAC %>% filter(Year %in% c(seq(1995,2020, by = 5))) %>%group_by(Year) %>% summarise(Sites = max(Sites), Richness = max(Richness))

points_all = all_SAC %>% filter(Sites == 361)
points_all_max = all_SAC %>% group_by(Year) %>% filter(Sites == max(Sites))

cc <- scales::seq_gradient_pal("royalblue4", "firebrick3")(seq(0,1,length.out=27)) 
color <- data.frame(Col = cc, Year = c(1994:2020))
all_SAC = merge(all_SAC, color)

gamma_a = ggplot() +
  geom_line(data = all_SAC,  aes(x = Sites, y = Richness, color = as.factor(Year)), size = 1) + 
  ggtitle("")  +
  geom_vline(xintercept = 361, linetype = "dashed")+
  scale_y_continuous(breaks = seq(20,110, by = 10)) +
  #geom_text(data = labels_complet_all, aes(x = Sites + 20, y = Richness, label = Year), size = 3) + 
  scale_colour_manual(values =  unique(all_SAC$Col)) + 
  theme_bw() + theme(axis.text = element_text(size = 8), 
                     axis.title = element_text(size = 9),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     legend.position="none") + ggtitle("")

#geom_text(data = labels_complet_north, aes(x = Sites + 10, y = Richness, label = Year), size = 5) + theme(legend.position="none")

points_all = merge(points_all, effort_main)
gamma_model_all = gam(Richness ~ s(Year, k = 4), data = points_all, family = gaussian)
gamma_model_all_max= gam(Richness ~ s(Year, k = 4), data = points_all_max, family = gaussian)
summary(lm(Richness ~ Year, data = points_all)) #0.5
summary(gamma_model_all) #80.3
summary(gamma_model_all_max) #80.3

gamma_pred = plot_model(gamma_model_all, type = "pred", terms = "Year")
gamma_pred = plot_model(gamma_model_all, type = "pred", terms = "mean_eff")

gamma_pred$data

gamma_pred_max = plot_model(gamma_model_all_max, type = "pred", terms = "Year", show.data = T)
gamma_pred_max$data


gamma_f = ggplot(points_all) +  
  
  geom_line(data = data.frame(gamma_pred$data), aes(x = x, y = predicted), col ="#8d5a99") +
  geom_ribbon(data = data.frame(gamma_pred$data), aes(x = x, y = predicted, ymax = conf.high, ymin = conf.low), fill ="#8d5a99", alpha = 0.1)+
  #geom_smooth(method = "lm", data = points_all,aes(x = Year, y = Richness), color = "#8d5a99") + 
  geom_point(aes(x = Year, y = Richness), size = 0.6)  +
  scale_x_continuous(limits = c(1994, 2020), breaks = seq(1994, 2020, by = 7)) + 
  
  theme_bw() + theme(axis.text = element_text(size = 8), 
                     axis.title = element_text(size = 9),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()) + ggtitle("")


pdf("Gamma_diversity_main_region.pdf",height = 2.5, width = 5)

cowplot::plot_grid(gamma_a,gamma_f)

dev.off()





## GAMMA DIVERSITY NORTH SEA #####

effort_north = north_data %>% dplyr::select(Site, swept_area, Year) %>% distinct() %>% group_by(Year) %>% 
  summarise(mean_eff = mean(swept_area),
            total_eff = sum(swept_area))


north_SAC_l = list() 

for (i in 1:200){
  
  north_SAC_l[[i]] = sac_curves_random(north_data, unique(north_data$Year))
  north_SAC_l[[i]]$it = i
  print(paste("This is iteration", i))
}

north_sac = do.call(rbind,north_SAC_l)
north_SAC = north_sac %>% group_by(Sites, Year) %>% summarise(SD = sd(Richness),Richness = mean(Richness))
north_SAC %>% dplyr::select(Year, Sites) %>% distinct() %>% group_by(Year) %>% count() %>% arrange(n)

#north_SAC = filter(north_SAC, Year > 1997)
labels_complet_north = north_SAC %>% filter(Year %in% c(seq(1995,2020, by = 5))) %>%group_by(Year) %>% summarise(Sites = max(Sites), Richness = max(Richness))

points_north = north_SAC %>% filter(Sites == 37)

cc <- scales::seq_gradient_pal("royalblue4", "firebrick3")(seq(0,1,length.out=27)) 
color <- data.frame(Col = cc, Year = c(1994:2020))
north_SAC = merge(north_SAC, color)


gamma_a_north = ggplot() +
  geom_line(data = north_SAC,  aes(x = Sites, y = Richness, color = as.factor(Year)), size = 1) + 
  ggtitle("")  +
  geom_vline(xintercept = 37, linetype = "dashed")+
  scale_y_continuous(breaks = seq(20,80, by = 10)) +
  #geom_text(data = labels_complet_north, aes(x = Sites + 20, y = Richness, label = Year), size = 3) + 
  scale_colour_manual(values =  unique(north_SAC$Col)) +theme_bw() + theme(axis.text = element_text(size = 8),
                                                                           plot.title = element_text(size = 9),
                                                                           axis.title.y = element_blank(),
                                                                           axis.title.x = element_blank()) + theme(legend.position="none")

#geom_text(data = labels_complet_north, aes(x = Sites + 10, y = Richness, label = Year), size = 5) + theme(legend.position="none")

points_north = merge(points_north, effort_north)
gamma_model_north = gam(Richness ~ s(Year, k = 5) + s(total_eff),  data = points_north, family = gaussian)

gamma_model_north = gam(Richness ~ s(Year, k = 5),  data = points_north, family = gaussian)

summary(lm(Richness ~ Year, data = points_north)) #0.87
summary(gamma_model_north)

plot_model(gamma_model_north, type = "pred", terms = "Year")
plot_model(gamma_model_north, type = "pred", terms = "mean_eff")

gamma_pred = plot_model(gamma_model_north, type = "pred", terms = "Year")
gamma_pred$data


100 * (gamma_pred$data$conf.low[22] - gamma_pred$data$conf.high[1]) / gamma_pred$data$conf.high[1]  
100 * (gamma_pred$data$predicted[22] - gamma_pred$data$predicted[1]) / gamma_pred$data$predicted[1]
100 * (gamma_pred$data$conf.high[22] - gamma_pred$data$conf.low[1]) / gamma_pred$data$conf.low[1]  

gamma_f_north = ggplot(points_north) +  
  
  geom_line(data = data.frame(gamma_pred$data), aes(x = x, y = predicted), col ="#8d5a99") +
  geom_ribbon(data = data.frame(gamma_pred$data), aes(x = x, y = predicted, ymax = conf.high, ymin = conf.low), fill ="#8d5a99", alpha = 0.1)+
  #geom_smooth(method = "lm", data = points_north,aes(x = Year, y = Richness), color = "#8d5a99") + 
  geom_point(aes(x = Year, y = Richness), size = 0.6)  +
  scale_x_continuous(limits = c(1994, 2020), breaks = seq(1994, 2020, by = 7)) + 
  
  theme_bw() + theme(axis.text = element_text(size = 8), 
                     axis.title = element_text(size = 9),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()) + ggtitle("")



pdf("Gamma_diversity_north.pdf",height = 2.5, width = 5)

grid.arrange(gamma_a_north, gamma_f_north, ncol = 2)

dev.off()



## GAMMA DIVERSITY SVALBARD ####

effort_svalb = svalb_data %>% dplyr::select(Site, swept_area, Year) %>% distinct() %>% group_by(Year) %>% 
  summarise(mean_eff = mean(swept_area),
            total_eff = sum(swept_area))



svalb_SAC_l = list() 

for (i in 1:200){
  
  svalb_SAC_l[[i]] = sac_curves_random(svalb_data, unique(svalb_data$Year))
  svalb_SAC_l[[i]]$it = i
  print(paste("This is iteration", i))
}

svalb_sac = do.call(rbind,svalb_SAC_l)
svalb_SAC = svalb_sac %>% group_by(Sites, Year) %>% summarise(SD = sd(Richness),Richness = mean(Richness))
svalb_SAC %>% dplyr::select(Year, Sites) %>% distinct() %>% group_by(Year) %>% count() %>% arrange(n)
#svalb_SAC = filter(svalb_SAC, Year > 1997)
labels_complet_svalb = svalb_SAC %>% filter(Year %in% c(seq(1995,2020, by = 5))) %>%group_by(Year) %>% summarise(Sites = max(Sites), Richness = max(Richness))

points_svalb = svalb_SAC %>% filter(Sites == 30)

cc <- scales::seq_gradient_pal("royalblue4", "firebrick3")(seq(0,1,length.out=27)) 
color <- data.frame(Col = cc, Year = c(1994:2020))
svalb_SAC = merge(svalb_SAC, color)


gamma_a_svalb = ggplot() +
  geom_line(data = svalb_SAC,  aes(x = Sites, y = Richness, color = as.factor(Year)), size = 1) + 
  ggtitle("")  +
  geom_vline(xintercept = 30, linetype = "dashed")+
  scale_y_continuous(breaks = seq(0,70, by = 10)) +
  #geom_text(data = labels_complet_svalb, aes(x = Sites + 20, y = Richness, label = Year), size = 3) + 
  scale_colour_manual(values =  unique(svalb_SAC$Col)) +theme_bw() + theme(axis.text = element_text(size = 8),
                                                                           plot.title = element_text(size = 9),
                                                                           axis.title.y = element_blank(),
                                                                           axis.title.x = element_blank()) + theme(legend.position="none")

#geom_text(data = labels_complet_north, aes(x = Sites + 10, y = Richness, label = Year), size = 5) + theme(legend.position="none")
points_svalb = merge(points_svalb, effort_svalb)
gamma_model_svalb = gam(Richness ~ s(Year), data = points_svalb, family = gaussian)
gamma_model_svalb = gam(Richness ~ s(Year), data = points_svalb, family = gaussian)

summary(lm(Richness ~ Year, data = points_svalb)) #0.69
summary(gamma_model_svalb) # 67

plot_model(gamma_model_svalb, type = "pred", terms = "Year")

gamma_pred = plot_model(gamma_model_svalb, type = "pred", terms = "Year")
gamma_pred$data

gamma_f_svalb = ggplot(points_svalb) +  
  
  geom_line(data = data.frame(gamma_pred$data), aes(x = x, y = predicted), col ="#8d5a99") +
  geom_ribbon(data = data.frame(gamma_pred$data), aes(x = x, y = predicted, ymax = conf.high, ymin = conf.low), fill ="#8d5a99", alpha = 0.1)+
  #geom_smooth(method = "lm", data = points_svalb,aes(x = Year, y = Richness), color = "#8d5a99") + 
  geom_point(aes(x = Year, y = Richness), size = 0.6)  +
  scale_x_continuous(limits = c(1994, 2020), breaks = seq(1994, 2020, by = 7)) + 
  
  theme_bw() + theme(axis.text = element_text(size = 8), 
                     axis.title = element_text(size = 9),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()) + ggtitle("")



pdf("Gamma_diversity_svalb.pdf",height = 2.5, width = 5)

grid.arrange(gamma_a_svalb, gamma_f_svalb, ncol = 2)

dev.off()

library(SpadeR)
main_region_data$PA = 1
try_main = main_region_data %>% select(valid_name, Site, PA, Year)

chao_1_main = list()

for (i in 1:26){
  try_mat = xtabs(PA ~ valid_name + Site, data = filter(try_main, Year == i + 1993))
  chao_main = data.frame(ChaoSpecies(try_mat, datatype = "incidence_raw")$Species_table)
  chao_main$indicator = rownames(chao_main)
  chao_main$Year = i + 1993
  rownames(chao_main) = NULL
  chao_1_main[[i]] = chao_main
  
  print(i)
}

chao_main = do.call(rbind, chao_1_main)

colnames(chao_main) = c("Estimate", "se", "lower", "higher", "indicator", "Year")





svalb_data$PA = 1
try_svalb = svalb_data %>% select(valid_name, Site, PA, Year)

chao_1_svalb = list()

for (i in 1:3){
  try_mat = xtabs(PA ~ valid_name + Site, data = filter(try_svalb, Year == i + 1995))
  chao_svalb = data.frame(ChaoSpecies(try_mat, datatype = "incidence_raw")$Species_table)
  chao_svalb$indicator = rownames(chao_svalb)
  chao_svalb$Year = i + 1995
  rownames(chao_svalb) = NULL
  chao_1_svalb[[i]] = chao_svalb
  
  print(i)
}

chao_2_svalb = list()

for (i in c(5,7:15)){
  try_mat = xtabs(PA ~ valid_name + Site, data = filter(try_svalb, Year == i + 1995))
  chao_svalb = data.frame(ChaoSpecies(try_mat, datatype = "incidence_raw")$Species_table)
  chao_svalb$indicator = rownames(chao_svalb)
  chao_svalb$Year = i + 1995
  rownames(chao_svalb) = NULL
  chao_2_svalb[[i]] = chao_svalb
  
  print(i)
}

chao_3_svalb = list()

for (i in c(17:18,20,22:25)){
  try_mat = xtabs(PA ~ valid_name + Site, data = filter(try_svalb, Year == i + 1995))
  chao_svalb = data.frame(ChaoSpecies(try_mat, datatype = "incidence_raw")$Species_table)
  chao_svalb$indicator = rownames(chao_svalb)
  chao_svalb$Year = i + 1995
  rownames(chao_svalb) = NULL
  chao_3_svalb[[i]] = chao_svalb
  
  print(i)
}

chao_f_svalb = do.call(rbind, chao_1_svalb)
chao_s_svalb = do.call(rbind, chao_2_svalb)
chao_se_svalb = do.call(rbind, chao_3_svalb)

chao_svalb = rbind(chao_f_svalb, chao_s_svalb, chao_se_svalb)
colnames(chao_svalb) = c("Estimate", "se", "lower", "higher", "indicator", "Year")
range(chao_svalb$Year)



north_data$PA = 1
try_north = north_data %>% select(valid_name, Site, PA, Year)

chao_1_north = list()

for (i in c(1:5, 7:9, 11:23)){
  try_mat = xtabs(PA ~ valid_name + Site, data = filter(try_north, Year == i + 1997))
  chao_north = data.frame(ChaoSpecies(try_mat, datatype = "incidence_raw")$Species_table)
  chao_north$indicator = rownames(chao_north)
  chao_north$Year = i + 1997
  rownames(chao_north) = NULL
  chao_1_north[[i]] = chao_north
  
  print(i)
}

chao_1_north[[1]]

chao_north = do.call(rbind, chao_1_north)
colnames(chao_north) = c("Estimate", "se", "lower", "higher", "indicator", "Year")
range(chao_north$Year)

chao_main$Region = "Norwegian-Barents Sea"
chao_svalb$Region = "Svalbard"
chao_north$Region = "North Sea"

chao_all = rbind(chao_main, chao_svalb, chao_north)

pdf("chao_indices.pdf", width = 8, height = 3)
ggplot(chao_all, aes(x = Year, y = Estimate))+ 
  geom_line(aes(color = indicator)) +
  #geom_ribbon(aes(x = Year,ymax = higher, ymin = lower, fill = indicator), alpha = 0.1) + 
  guides(color = guide_legend(title="Richness index"))+
  facet_wrap(.~ Region) +
  theme(axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 10))

dev.off()
