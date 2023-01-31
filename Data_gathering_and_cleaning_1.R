library(tidyverse)
library(rgdal)
library(worms)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")

setwd("C://Users//06061016//Documents//Treball//Costello//IMR1980-2017//Data")

# Data gathering ----------------------------------------------------------
documents <- list.files(pattern = "btraal.*csv")
All <- lapply(documents,function(i){read.csv(i, sep = ";")})

lapply(All,function(i){dim(i)})
All <- lapply(All, function(i){mutate(i, dist = as.double(dist),
                                      Min = as.double(Min),
                                      lengde = as.double(lengde),
                                      kjonn = as.character(kjonn))})
data <- bind_rows(All)

data <- dplyr::rename(data,
               Year            = aar, 
               Month           = mnd,
               Longitude       = lengde, 
               Latitude        = bredde,
               Gear            = redskap, 
               Start_time      = starttid,
               End_time        = stopptid,
               Duration        = taueT, 
               Bot_depth       = bunndyp,
               Opening         = opening,
               Distance        = dist,
               Quality_gear    = tilstand,
               Quality_haul    = kvalitet,
               Species_code    = akode, 
               Haul_separation = delnr,
               Common_name     = art,
               Latin_name      = latin, 
               Measurement     = maal_fangst,
               Total_weight    = fangstKvant,
               Total_abund     = fangstAnt, 
               Measurement_raw = maal_lprov,
               Measure_method  = lengdemaal, 
               Subsamp_weight  = lengdeProveKv,
               Subsamp_abund   = lengdeProveAnt, 
               Sex             = kjonn,
               Min_length      = Min,
               Max_length      = Max,
               Sum_abund       = Sum)
head(data)
unique(data$Year)

# I eliminate repeated values
data %>% duplicated() %>% table() # There are 195 repeated entrances. 
data <- data %>% distinct() # From 649483, we keep 649288 records

# Columns with "-1". They make sense in measurement columns, "Measurement_raw", and "Measurement". Not in the rest.
# However, I do not eliminate them because they may be useful for presence absence analysis. 
colSums(data == -1, na.rm = T)

# Those columns are the ones referring to Length of subsample
colnames(data)[29:69]


#  I check the difference between all different lengths, the column of total abundance and the sum of abundance. 
a = data.frame(sumoriginal = data$Total_abund, sum = rowSums(data[,c(29:69)], na.rm = T), sum_abund = data$Sum_abund, sep = data$Haul_separation)
a = a %>% mutate(difference = ((sumoriginal- sum)/sumoriginal)*100, 
                 dif = (sum_abund == sum))
a

#a %>% filter(dif == F)
#data %>% filter(Haul_separation > 3)
#data[which(data$Total_abund>400000),]
#data[648243,]

####### Nomenclature corrections ------- ######
## Drop empty & duplicated values
data = data %>% mutate(Latin_name = gsub("            ","UNSPECIFIED",Latin_name)) 
unique(data$Latin_name)

# I eliminate spaces from the species names right side
data$Latin_name = str_trim(data$Latin_name, side = "right")

# I discard those entrances with no name, and those with names that are not of our interest. Myxomicota are fungus, and stein are stones. 
# These undetermined and not interesting entrances are all together are 3640 entrances. 
data <- data %>% filter(Latin_name != "" & Common_name != "", 
                        Latin_name != "INDETERMINATUS",
                        Latin_name != "STEIN",
                        Latin_name != "MYXOMYCOTA") # 645648 are kept. 

data$Latin_name[data$Latin_name == "EUSELACHII"] = "Elasmobranchii"

# Fix common names without proper scientific name -------------------------------------------------------

# Assign corrected names from a manually built db, but firts i correct the mayus 
corrected_names <- read_csv("corrected_names.csv")

# Now I assign the corrected names

sum(data$Latin_name == "SEBASTES MARINUS",na.rm = T) ## Should be 0, since the proper name is Sebastes norvegicus

for(row in 1:nrow(data)){
  if(data$Latin_name[row] %in% corrected_names$Name){
    data$Latin_name[row] = corrected_names$Corrected_name[data$Latin_name[row] == corrected_names$Name]
  }
}
sum(data$Latin_name == "SEBASTES MARINUS",na.rm = T) ## Should be 0, since the proper name is Sebastes norvegicus

# I make the first letter uppercase and the rest lowercase

data$Latin_name  = paste(substr(data$Latin_name,1,1),tolower(substr(data$Latin_name,2,100)), sep = "")
data$Common_name = paste(substr(data$Common_name,1,1),tolower(substr(data$Common_name,2,100)), sep = "")

# The following ones I detected that their latin name was on the common name. I correct them and add them at the Latin name column

data$Latin_name[which(data$Common_name == "Langfinnet s")] = "Careproctus reinhardti"
data$Latin_name[which(data$Common_name == "Kortfinnet s")] = "Careproctus micropus"
data$Latin_name[which(data$Common_name == "Hymenaster p")] = "Hymenaster pellucidus"
data$Latin_name[which(data$Common_name == "Boreomysis a")] = "Boreomysis arctica"
data$Latin_name[which(data$Common_name == "Hymenodora g")] = "Hymenodora glacialis"
data$Latin_name[which(data$Common_name == "Yoldiella in")] = "Yoldiella intermedia"
data$Latin_name[which(data$Common_name == "Nemertesia a")] = "Nemertesia antennina"
data$Latin_name[which(data$Common_name == "Lytocarpia m")] = "Lytocarpia myriophyllum"
data$Latin_name[which(data$Common_name == "Nemertesia r")] = "Nemertesia ramosa"
data$Latin_name[which(data$Common_name == "Golfingia (g")] = "Golfingia (golfingia) vulgaris"
data$Latin_name[which(data$Common_name == "Rossia palpe")] = "Rossia palpebrosa"
data$Latin_name[which(data$Common_name == "Illex coinde")] = "Illex coindetii"
data$Latin_name[which(data$Common_name == "Alloteuthis ")] = "Alloteuthis"
data$Latin_name[which(data$Common_name == "Loligo      ")] = "Loligo"
data$Latin_name[which(data$Common_name == "?lebrosme   ")] = "Zoarcidae"
data$Latin_name[which(data$Common_name == "?lebrosmefam")] = "Zoarcidae"

# The following ones had, in the common_name column, their Taxonomic Serial Number (TSN), and had no latin name. I add the Aphia ID instead. 
data$Common_name[which(data$Common_name == "73887")] = 138855
data$Common_name[which(data$Common_name == "95573")] = 110708
data$Common_name[data$Latin_name == "Alosa agone"] = 416357
data$Latin_name[data$Latin_name == "Alosa agone"] = "Unspecified" # Alosa agone for some reason is not accepted by worms package, I need to add it manually

sum(is.na(data$Latin_name))
sum(is.na(data$Common_name))
# WorMS reference --------------------------------------------------------
#options(max.print=2000)
#head(data)
#
#names = sort(unique(data$Latin_name))
#names = names[-which(names == "Unspecified")]
#names # 1074 
#
#ids <- as.numeric(unique(data$Common_name[which(data$Latin_name == "Unspecified")]))
#worms_ref_ids <- wormsbyid(ids)
#worms_ref_names <- wormsbymatchnames(names)
#
#
#worms_ref_ids <- worms_ref_ids %>% select(AphiaID, scientificname, valid_name, valid_AphiaID, kingdom, phylum, class, order, family, genus) 
#worms_ref_names <- worms_ref_names %>% select(AphiaID, scientificname, valid_name, valid_AphiaID, kingdom, phylum, class, order, family, genus) 
#
#head(worms_ref_ids)
#head(worms_ref_names)
#
#
#write.csv(worms_ref_ids,"worms_ref_ids.csv")
#write.csv(worms_ref_names,"worms_ref_names.csv")

#Complete the tax info of data from worms databases --------------------------------------------------------
worms_ref_names = read_csv("worms_ref_names.csv")[,-1]
worms_ref_ids   = read_csv("worms_ref_ids.csv")[,-1]
no_worms_ref    = read_csv("no_worms_ref.csv")
head(worms_ref_ids)

worms_ref_names
ids = data %>% filter(Latin_name == "Unspecified") 

no_ids = data %>% filter(Latin_name != "Unspecified",
                         Latin_name != "Golfingia (golfingia) vulgaris",
                         Latin_name !="Porania (porania) pulvillus") 

no_worms = data %>% filter(Latin_name == "Golfingia (golfingia) vulgaris" |
                           Latin_name == "Porania (porania) pulvillus")


ids_complete <- merge(ids, worms_ref_ids, by.x = "Common_name", by.y = "AphiaID", all = TRUE)
ids_complete = ids_complete %>% filter(is.na(Year) == FALSE)#For some reason two extra rows filled with NAs are added to the df

no_worms_complete <- merge(no_worms, no_worms_ref, by.x = "Latin_name", by.y = "scientificname", all = TRUE)

no_ids_complete <- merge(no_ids, worms_ref_names, by.x = "Latin_name", by.y = "scientificname", all = TRUE)
no_ids_complete <- no_ids_complete %>% filter(is.na(Year) == FALSE)
no_ids_complete <- no_ids_complete[-which(duplicated(no_ids_complete)),]

total_completed_data <- bind_rows(ids_complete,no_ids_complete, no_worms_complete)

sum(is.na(total_completed_data$Year))
sum(is.na(total_completed_data$valid_name))
sum(is.na(total_completed_data$Total_abund))

total_completed_data %>% select(Year, Longitude, Latitude, Bot_depth, Distance) %>% distinct() %>% arrange(Year, Longitude, Latitude, Bot_depth, Distance) %>% 
  group_by(Year, Longitude, Latitude, Bot_depth, Distance) %>%  summarise(count = n())


wrong = total_completed_data %>% select(Year, Longitude, Latitude, Bot_depth, Distance, Gear, Start_time, End_time) %>% distinct() %>% arrange(Year, Longitude, Latitude, Bot_depth, Distance) %>% 
  group_by(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time) %>%  summarise(count = n()) %>% filter(count > 1)

wrong2 = total_completed_data %>% 
  filter(Year %in% wrong$Year & Longitude %in% wrong$Longitude & Latitude %in% wrong$Latitude & Bot_depth %in% wrong$Bot_depth & Gear %in% wrong$Gear & Start_time %in% wrong$Start_time & End_time %in% wrong$End_time) %>% 
  arrange(Year, Longitude, Latitude, Bot_depth, Distance) %>% 
  group_by(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time, Distance) %>%  summarise(count = n()) %>% arrange(Year, Longitude, Latitude, Bot_depth)  %>% filter(count == 1)



wrong2 = wrong2 %>% mutate(id = paste(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time, Distance))

total_completed_data = total_completed_data %>% mutate(id = paste(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time, Distance))

total_completed_data = total_completed_data %>% mutate(Distance = ifelse(total_completed_data$id %in% wrong2$id, Distance/10, Distance))

wrong3 = total_completed_data %>% select(Year, Longitude, Latitude, Bot_depth, Distance, Gear, Start_time, End_time) %>% distinct() %>% arrange(Year, Longitude, Latitude, Bot_depth, Distance) %>% 
  group_by(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time) %>%  summarise(count = n()) %>% filter(count > 1) ## These are the rows that although repeated, both good and bad had 1 entrance and both have been divided by 10, which is a mistake. 


wrong3 = wrong3%>% mutate(id = paste(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time))
wrong2 = wrong2 %>% mutate(id = paste(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time))

good_wrongs = wrong2[!wrong2$id %in% wrong3$id,] # These are the rows in which the smallest Distance has more than one row. 

# Now I need to select rows from wrong2, but not from wrong3 ! 
total_completed_data <- bind_rows(ids_complete,no_ids_complete, no_worms_complete)

wrong2 = total_completed_data %>% 
  filter(Year %in% wrong$Year & Longitude %in% wrong$Longitude & Latitude %in% wrong$Latitude & Bot_depth %in% wrong$Bot_depth & Gear %in% wrong$Gear & Start_time %in% wrong$Start_time & End_time %in% wrong$End_time) %>% 
  arrange(Year, Longitude, Latitude, Bot_depth, Distance) %>% 
  group_by(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time, Distance) %>%  summarise(count = n()) %>% arrange(Year, Longitude, Latitude, Bot_depth)  %>% filter(count == 1)



wrong2 = wrong2 %>% mutate(id = paste(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time, Distance))
wrong2_good = wrong2 %>% mutate(id_2 = paste(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time)) %>% filter(id_2 %in% good_wrongs$id)


total_completed_data = total_completed_data %>% mutate(id = paste(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time, Distance))
total_completed_data = total_completed_data %>% mutate(Distance = ifelse(id %in% wrong2_good$id , Distance/10,
                                                                         ifelse(id %in% wrong3$id, Distance, Distance)))

## Up to here, I have corrected all the duplicated rows which had more than 1 row in the smallest Distance. Now, there are more rows left that also need to be corrected


still_wrong = total_completed_data %>% select(Year, Longitude, Latitude, Bot_depth, Distance, Gear, Start_time, End_time) %>% distinct() %>% arrange(Year, Longitude, Latitude, Bot_depth, Distance) %>% 
  group_by(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time) %>%  summarise(count = n()) %>% filter(count > 1)

wrong2 = total_completed_data %>% 
  filter(Year %in% still_wrong$Year & Longitude %in% still_wrong$Longitude & Latitude %in% still_wrong$Latitude & Bot_depth %in% still_wrong$Bot_depth & Gear %in% still_wrong$Gear & Start_time %in% still_wrong$Start_time & End_time %in% still_wrong$End_time) %>% 
  arrange(Year, Longitude, Latitude, Bot_depth, Distance) %>% 
  group_by(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time, Distance) %>%  summarise(count = n()) %>% arrange(Year, Longitude, Latitude, Bot_depth)  %>% filter(count == 1)

# As the total number of rows here are 278, I could loop through all of them, check if the previous row is ten times smaller and select the rows that fulfill this condition
resu = list()
for (i in 2:nrow(wrong2)) {
  if(wrong2[i-1,8] == wrong2[i,8]/10){
    resu[[i]] = wrong2[i,] 
  }
}
selected_rows = do.call(rbind, resu) ## These rows in the main data frame need to hace the Distance corrected. 

selected_rows = selected_rows %>% mutate(id = paste(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time, Distance))


total_completed_data = total_completed_data %>% mutate(Distance = ifelse(id %in% selected_rows$id , Distance/10,Distance))



## I check if I still have the problem somewhere

total_completed_data %>% select(Year, Longitude, Latitude, Bot_depth, Distance, Gear, Start_time, End_time) %>% distinct() %>% arrange(Year, Longitude, Latitude, Bot_depth, Distance) %>% 
  group_by(Year, Longitude, Latitude, Bot_depth, Gear, Start_time, End_time) %>%  summarise(count = n()) %>% filter(count > 1)

## I don't!! 

write.csv(total_completed_data, "total_completed_data.csv")

