### DATA ####
data = read_csv("data_filtered.csv")
orig_data = data %>%  mutate(Site = paste(Longitude, Latitude, Year, Month, Bot_depth, swept_area))

main_region_data = orig_data %>% filter(Region == "Norwegian-Barents Sea")
north_data = orig_data %>% filter(Region == "North Sea")
sva_data = orig_data %>% filter(Region == "Svalbard")





### TURNOVER MAIN REGION ####
library(betapart)

a = main_region_data %>% dplyr::select(Year, Site) %>% distinct() %>% group_by(Year) %>% count() %>% arrange(n)
plot(a)
main_region_data$Total_abund = 1
range(main_region_data$Year)
results_total_main_region_data = list()
iterations = 200

for (k in 1:iterations) {
  results = list()
  for (i in c(1994:2020)) {
    data = main_region_data %>% filter(Year == i) %>% mutate(Total_abund = 1)
    
    sites = unique(data$Site)
    sites_sel = sample(sites, 361)
    effort = data %>% filter(Site %in% sites_sel) %>% dplyr::select(Year, Site, swept_area) %>% distinct() %>% group_by(Year) %>% summarise(effort = mean(swept_area))
    data_mat = data %>% filter(Site %in% sites_sel) %>% dplyr::select(Total_abund, Site, valid_name) %>% distinct()
    mat_data = xtabs(Total_abund ~ Site + valid_name, data_mat)
    results[[i]] = data.frame(#Beta_PW_S     = beta.div.comp(mat_data, coef = "S", quant = F)$part[1],
      Beta_PW_J     = beta.div.comp(mat_data, coef = "J", quant = F)$part[1],
      #Turnover_PW_S = beta.div.comp(mat_data, coef = "S", quant = F)$part[4],
      Turnover_PW_J = beta.div.comp(mat_data, coef = "J", quant = F)$part[4],
      #beta_raw = beta.div(mat_data,method = "hellinger",samp = F)$beta[2],
      
      #Turnover_S = beta.multi(mat_data, index.family = "sorensen")$beta.SIM,
      #Beta_S = beta.multi(mat_data, index.family = "sorensen")$beta.SOR,
      #Turnover_J = beta.multi(mat_data, index.family = "jaccard")$beta.JTU,
      #Beta_J = beta.multi(mat_data, index.family = "jaccard")$beta.JAC,
      effort = effort,
      Year = i, 
      rep = k)
    print(i)
  }
  results_total_main_region_data[[k]] = do.call(rbind,results)
  print(paste("Iteration cicle", k))
}


total_results_main_region = do.call(rbind, results_total_main_region_data)
total_results_main_region = total_results_main_region %>% 
  mutate(#Turnover_S = Turnover_S/Beta_S,
  #Turnover_J = Turnover_J/Beta_J,
  #Beta_S = Beta_S,
  #Beta_J = Beta_J,
  #Turnover_PW_S = Turnover_PW_S,
  Turnover = Turnover_PW_J,
  #Beta_PW_S = Beta_PW_S,
  Beta = Beta_PW_J)
#Beta_r = beta_raw)

b = total_results_main_region %>% group_by(Year) %>% summarise(beta = mean(Beta),
                                                               turn = mean(Turnover),
                                                               eff = mean(effort.effort))
c = a %>% group_by(Year) %>% summarise(count = n())
d = merge(a,b)

summary(lm(beta ~ Year, data = d))
summary(gam(beta ~ s(Year) + s(eff, k = 3), data = d))
summary(gam(turn ~ s(Year, k = 5) + s(eff, k = 4), data = d))
summary(gam(turn ~ s(Year), data = d))

plot_turn = gam(turn ~ s(Year), data = d)
plot_beta = gam(beta ~ s(Year),data = d)

summary(plot_turn)
summary(plot_beta)

plot_t = plot_model(plot_turn, terms = "Year", type  = "pred",  show.data = T)
plot_b = plot_model(plot_beta, terms = "Year", type  = "pred",  show.data = T)



100 * (plot_t$data$conf.low[27] -  plot_t$data$conf.high[1]) / plot_t$data$conf.high[1]  
100 * (plot_t$data$predicted[27] - plot_t$data$predicted[1]) / plot_t$data$predicted[1]
100 * (plot_t$data$conf.high[27] - plot_t$data$conf.low[1])  / plot_t$data$conf.low[1]  

100 * (plot_b$data$conf.low[27] -  plot_b$data$conf.high[1]) / plot_b$data$conf.high[1]  
100 * (plot_b$data$predicted[27] - plot_b$data$predicted[1]) / plot_b$data$predicted[1]
100 * (plot_b$data$conf.high[27] - plot_b$data$conf.low[1]) /  plot_b$data$conf.low[1]  




turn_a = ggplot(d) +
  geom_point(aes(x = Year, y = beta), size = 0.7, pch = 17,  color = "#804275ff")+
  geom_line(data = data.frame(plot_b$data), aes(x = x, y = predicted), color = "#804275ff", size = 0.7) +
  geom_ribbon(data = data.frame(plot_b$data),aes(x = x, y = predicted, ymax = conf.high, ymin = conf.low),fill = "#804275ff", alpha = 0.1) +
  
  geom_point(aes(x = Year, y = turn - 0.25), size = 0.7, color = "#a8262aff")+
  geom_line(data = data.frame(plot_t$data), aes(x = x, y = predicted - 0.25), color = "#a8262aff", size = 0.7) +
  geom_ribbon(data = data.frame(plot_t$data),aes(x = x, y = predicted - 0.25, ymax = conf.high - 0.25, ymin = conf.low - 0.25), fill = "#a8262aff", alpha = 0.1) + 
  scale_y_continuous(name = expression("Beta diversity"),
                     sec.axis = sec_axis(~. + 0.25, name = "Turnover"))  +
  theme_bw() +
  theme(axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 9),
        panel.grid.minor = element_line(color = "darkgrey"), panel.grid.major = element_line(color = "darkgrey"))



pdf("Turnover_all.pdf", width = 3.5, height = 2.5)
turn_a
dev.off()


## TURNOVER SVALBARD ####
a = svalb_data %>% dplyr::select(Year, Site) %>% distinct() %>% group_by(Year) %>% count() %>% arrange(n)
plot(a)
svalb$Total_abund = 1
range(svalb_data$Year)
sort(unique(svalb_data$Year))
results_total_svalb_data = list()
iterations = 200

for (k in 1:iterations) {
  results = list()
  for (i in sort(unique(svalb_data$Year))) {
    data = svalb_data %>% filter(Year == i) %>% mutate(Total_abund = 1)
    
    sites = unique(data$Site)
    sites_sel = sample(sites, 30)
    data_mat = data %>% filter(Site %in% sites_sel) %>% dplyr::select(Total_abund, Site, valid_name) %>% distinct()
    mat_data = xtabs(Total_abund ~ Site + valid_name, data_mat)
    effort = data %>% filter(Site %in% sites_sel) %>% dplyr::select(Year, Site, swept_area) %>% distinct() %>% group_by(Year) %>% summarise(effort = mean(swept_area))
    results[[i]] = data.frame(Beta_PW_S     = beta.div.comp(mat_data, coef = "S", quant = F)$part[1],
                              Beta_PW_J     = beta.div.comp(mat_data, coef = "J", quant = F)$part[1],
                              Turnover_PW_S = beta.div.comp(mat_data, coef = "S", quant = F)$part[4],
                              Turnover_PW_J = beta.div.comp(mat_data, coef = "J", quant = F)$part[4],
                              #
                              #Turnover_S = beta.multi(mat_data, index.family = "sorensen")$beta.SIM,
                              #Beta_S = beta.multi(mat_data, index.family = "sorensen")$beta.SOR,
                              #Turnover_J = beta.multi(mat_data, index.family = "jaccard")$beta.JTU,
                              #Beta_J = beta.multi(mat_data, index.family = "jaccard")$beta.JAC,
                              effort = effort,
                              Year = i, 
                              rep = k)
    print(i)
  }
  results_total_svalb_data[[k]] = do.call(rbind,results)
  print(paste("Iteration cicle", k))
}


total_results_svalb = do.call(rbind, results_total_svalb_data)
total_results_svalb = total_results_svalb %>% mutate(Turnover_PW_S = Turnover_PW_S,
                                                     #Turnover_S = Turnover_S/Beta_S,
                                                     #Turnover_J = Turnover_J/Beta_J,
                                                     #Beta_S = Beta_S,
                                                     #Beta_J = Beta_J
                                                     Turnover_PW_J = Turnover_PW_J,
                                                     Beta_PW_S = Beta_PW_S,
                                                     Beta_PW_J = Beta_PW_J)

b = total_results_svalb %>% group_by(Year) %>% summarise(beta = mean(Beta_PW_J),
                                                         turn = mean(Turnover_PW_J),
                                                         effort = mean(effort.effort))
c = a %>% group_by(Year) %>% summarise(count = n())
d = merge(a,b)

summary(lm(beta ~ Year, data = d))
summary(lm(turn ~ Year, data = d))

beta = gam(beta ~ s(Year, k = 4), data = d)
turn = gam(turn ~ s(Year, k = 4), data = d)

summary(beta)
summary(turn)

turn_svalb = plot_model(turn, type = "pred", terms = "Year")
beta_svalb = plot_model(beta, type = "pred", terms = "Year")

a$data$predicted
b$data$predicted

100 * (a$data$conf.low[22] - a$data$conf.high[1]) / a$data$conf.high[1]  
100 * (a$data$predicted[22] - a$data$predicted[1]) / a$data$predicted[1]
100 * (a$data$conf.high[22] - a$data$conf.low[1]) / a$data$conf.low[1]  

100 * (b$data$conf.low[22] - b$data$conf.high[1]) / b$data$conf.high[1]  
100 * (b$data$predicted[22] - b$data$predicted[1]) / b$data$predicted[1]
100 * (b$data$conf.high[22] - b$data$conf.low[1]) / b$data$conf.low[1]  

turn_a = ggplot(d) +
  geom_point(aes(x = Year, y = beta), size = 0.7, pch = 17,  color = "#804275ff")+
  geom_line(data = data.frame(beta_svalb$data), aes(x = x, y = predicted), color = "#804275ff", size = 0.7) +
  geom_ribbon(data = data.frame(beta_svalb$data),aes(x = x, y = predicted, ymax = conf.high, ymin = conf.low),fill = "#804275ff", alpha = 0.1) +
  
  geom_point(aes(x = Year, y = turn - 0.25), size = 0.7, color = "#a8262aff")+
  geom_line(data = data.frame(turn_svalb$data), aes(x = x, y = predicted - 0.25), color = "#a8262aff", size = 0.7) +
  geom_ribbon(data = data.frame(turn_svalb$data),aes(x = x, y = predicted - 0.25, ymax = conf.high - 0.25, ymin = conf.low - 0.25), fill = "#a8262aff", alpha = 0.1) + 
  scale_y_continuous(name = expression("Beta diversity"),
                     sec.axis = sec_axis(~. + 0.25, name = "Turnover"))  +
  theme_bw() +
  theme(axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 9),
        panel.grid.minor = element_line(color = "darkgrey"), panel.grid.major = element_line(color = "darkgrey"))


pdf("Turnover_svalb.pdf", width = 3.5, height = 2.5)
turn_a
dev.off()

## TURNOVER NORTH ####


a = north_data %>% dplyr::select(Year, Site) %>% distinct() %>% group_by(Year) %>% count() %>% arrange(n)
plot(a)
a
north$Total_abund = 1
range(north_data$Year)
sort(unique(north_data$Year))
results_total_north_data = list()
iterations = 200

for (k in 1:iterations) {
  results = list()
  for (i in sort(unique(north_data$Year))) {
    data = north_data %>% filter(Year == i) %>% mutate(Total_abund = 1)
    
    sites = unique(data$Site)
    sites_sel = sample(sites, 37)
    data_mat = data %>% filter(Site %in% sites_sel) %>% dplyr::select(Total_abund, Site, valid_name) %>% distinct()
    mat_data = xtabs(Total_abund ~ Site + valid_name, data_mat)
    effort = data %>% filter(Site %in% sites_sel) %>% dplyr::select(Year, Site, swept_area) %>% distinct() %>% group_by(Year) %>% summarise(effort = mean(swept_area))
    results[[i]] = data.frame(Beta_PW_S     = beta.div.comp(mat_data, coef = "S", quant = F)$part[1],
                              Beta_PW_J     = beta.div.comp(mat_data, coef = "J", quant = F)$part[1],
                              Turnover_PW_S = beta.div.comp(mat_data, coef = "S", quant = F)$part[4],
                              Turnover_PW_J = beta.div.comp(mat_data, coef = "J", quant = F)$part[4],
                              
                              #Turnover_S = beta.multi(mat_data, index.family = "sorensen")$beta.SIM,
                              #Beta_S = beta.multi(mat_data, index.family = "sorensen")$beta.SOR,
                              #Turnover_J = beta.multi(mat_data, index.family = "jaccard")$beta.JTU,
                              #Beta_J = beta.multi(mat_data, index.family = "jaccard")$beta.JAC,
                              effort = effort,
                              Year = i, 
                              rep = k)
    print(i)
  }
  results_total_north_data[[k]] = do.call(rbind,results)
  print(paste("Iteration cicle", k))
}


total_results_north = do.call(rbind, results_total_north_data)
total_results_north = total_results_north %>% mutate(#Turnover_S = Turnover_S/Beta_S,
  #Turnover_J = Turnover_J/Beta_J,
  #Beta_S = Beta_S,
  #Beta_J = Beta_J,
  #Turnover_PW_S = Turnover_PW_S,
  Turnover_PW_J = Turnover_PW_J,
  #Beta_PW_S = Beta_PW_S,
  Beta_PW_J = Beta_PW_J)

b = total_results_north %>% group_by(Year) %>% summarise(beta = mean(Beta_PW_J),
                                                         turn = mean(Turnover_PW_J),
                                                         effort = mean(effort.effort))

c = a %>% group_by(Year) %>% summarise(count = n())
d = merge(a,b)

beta_plot = gam(beta ~ s(Year, k = 5), data = d)
turn_plot = gam(turn ~ s(Year, k = 5), data = d)

summary(beta_plot)
summary(turn_plot)

beta_north = plot_model(beta_plot, type = "pred", terms = "Year")
turn_north = plot_model(turn_plot, type = "pred", terms = "Year")

beta_north$data 
turn_north$data 



100 * (beta_north$data$conf.low[22] -  beta_north$data$conf.high[1]) / beta_north$data$conf.high[1]  
100 * (beta_north$data$predicted[22] - beta_north$data$predicted[1]) / beta_north$data$predicted[1]
100 * (beta_north$data$conf.high[22] - beta_north$data$conf.low[1]) /  beta_north$data$conf.low[1]  

100 * (turn_north$data$conf.low[22]  - turn_north$data$conf.high[1]) / turn_north$data$conf.high[1]  
100 * (turn_north$data$predicted[22] - turn_north$data$predicted[1]) / turn_north$data$predicted[1]
100 * (turn_north$data$conf.high[22] - turn_north$data$conf.low[1]) /  turn_north$data$conf.low[1]  


turn_a = ggplot(d) +
  geom_point(aes(x = Year, y = beta), size = 0.7, pch = 17,  color = "#804275ff")+
  geom_line(data = data.frame(beta_north$data), aes(x = x, y = predicted), color = "#804275ff", size = 0.7) +
  geom_ribbon(data = data.frame(beta_north$data),aes(x = x, y = predicted, ymax = conf.high, ymin = conf.low),fill = "#804275ff", alpha = 0.1) +
  
  geom_point(aes(x = Year, y = turn - 0.25), size = 0.7, color = "#a8262aff")+
  geom_line(data = data.frame(turn_north$data), aes(x = x, y = predicted - 0.25), color = "#a8262aff", size = 0.7) +
  geom_ribbon(data = data.frame(turn_north$data),aes(x = x, y = predicted - 0.25, ymax = conf.high - 0.25, ymin = conf.low - 0.25), fill = "#a8262aff", alpha = 0.1) + 
  scale_y_continuous(name = expression("Beta diversity"),
                     sec.axis = sec_axis(~. + 0.25, name = "Turnover"))  +
  theme_bw() +
  theme(axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 9),
        panel.grid.minor = element_line(color = "darkgrey"), panel.grid.major = element_line(color = "darkgrey"))


pdf("Turnover_north.pdf", width = 3.5, height = 2.5)
turn_a
dev.off()

