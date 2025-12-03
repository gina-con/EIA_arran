rm(list=ls())
aidata <- read.csv('Data/kicksampling_data.csv', header = TRUE)
library(dplyr)
library(ggplot2)
library(ggVennDiagram)

#south
#----
#subsetting 
sparentid_kicksampling <- c('SD4FI_A', 'SU4FI_A','SD4FI_B','SU4FI_B')

aidata_kicksampling_south <- aidata %>% 
  filter(eventID %in% sparentid_kicksampling)
#analysis 
unique_species_kicksampling_south <- aidata_kicksampling_south %>% 
  distinct(Commonname, .keep_all = TRUE)

Commonname_kicksampling_south <- c(unique_species_kicksampling_south$Commonname)
order_kicksampling_south <- c(unique_species_kicksampling_south$Order)
family_kicksampling_south <- c(unique_species_kicksampling_south$scientificName)
site_id_south <- c(unique_species_kicksampling_south$eventID)
south_kicksampling_dataframe <- data.frame(Site = "South", "Site ID" = site_id_south, Order = order_kicksampling_south, Family = family_kicksampling_south, "Common Name" = Commonname_kicksampling_south,
                                           check.rows=TRUE)

south_kicksampling_dataframe

#-

#north

#north
#----
#subsetting
nparentid_kicksampling <- c('ND4FI_A', 'NU3FI_A','ND4FI_B','NU3FI_B')

aidata_kicksampling_north <- aidata %>% 
  filter(eventID %in% nparentid_kicksampling)

#analysis
unique_species_kicksampling_north <- aidata_kicksampling_north %>% 
  distinct(Commonname, .keep_all = TRUE)

Commonname_kicksampling_north <- c(unique_species_kicksampling_north$Commonname)
order_kicksampling_north <- c(unique_species_kicksampling_north$Order)
family_kicksampling_north <- c(unique_species_kicksampling_north$scientificName)
site_id_north <- c(unique_species_kicksampling_north$eventID)
north_kicksampling_dataframe <- data.frame(Site = "North", "Site ID" = site_id_north, Order = order_kicksampling_north, Family = family_kicksampling_north, "Common Name" = Commonname_kicksampling_north,
                                           check.rows=TRUE)

unique_south_aquatic <- unique(south_kicksampling_dataframe$Order)
unique_north_aquatic <- unique(north_kicksampling_dataframe$Order)

#venn diagram to compare shared and unique order
vennlist <- list('South'=c(unique_south_aquatic), 'North'=c(unique_north_aquatic))
ggVennDiagram(vennlist, label_alpha = 0, label="count")+
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
  theme(legend.position = "none")+ 
  ggtitle("Number of Unique Orders of Aquatic Invertebrates")+
  coord_flip()


#making tables
library(gt)

gt(south_kicksampling_dataframe)
gt(north_kicksampling_dataframe)

#for combined data set
aquatic_unique_south <- data.frame(Taxa = "Aquatic Invertebrates", 
                                       Site = "South",
                                       Order = unique_south_aquatic)
aquatic_unique_north <- data.frame(Taxa = "Aquatic Invertebrates", 
                                       Site = "North",
                                       Order = unique_north_aquatic)
aquatic_unique_combined <- (rbind(aquatic_unique_south, aquatic_unique_north))
write.csv(aquatic_unique_combined, "C:\\Users\\gcon0\\Documents\\university\\Professional skills\\assessment\\data_analysis\\Figures\\csv_for_master\\aquatic_comb.csv", row.names=FALSE)

###----
#retrieving site IDs for every time each common name was recorded (south)
worm_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in% "Worm")
blackfly_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in% "Blackfly_larvae")
stonefly_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in%"Stonefly_larvae")
waterbeetle_larvae_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in% "Waterbeetle_larvae ")
waterbeetle_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in% "Waterbeetle ")
caddisfly_larvae_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in%"Caddisfly_larvae")
mayfly_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in%"Mayfly_larvae")
diving_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in%"Diving_beetle")
lesser_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in%"Lesser Water Boatman")
damsel_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in%"Damselfly_larvae")
dragonfly_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in%"Dragonfly_larvae")
greater_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in% "Greater Water Boatman")
caddisfly_case_south <- aidata_kicksampling_south %>% 
  filter(Commonname %in%"Caddisfly_case")

EID_worm_south <- unique(worm_south$eventID)
EID_blackfly_south <- unique(blackfly_south$eventID)
EID_stonefly_south <- unique(stonefly_south$eventID)
EID_waterbeetle_south <- unique(waterbeetle_south$eventID)
EID_waterbeetlelarvae_south <- unique(waterbeetle_larvae_south$eventID)
EID_caddisfly_south <- unique(caddisfly_larvae_south$eventID)
EID_mayfly_south <- unique(mayfly_south$eventID)
EID_diving_south <- unique(diving_south$eventID)
EID_lesser_south <- unique(lesser_south$eventID)
EID_damsel_south <- unique(damsel_south$eventID)
EID_dragonfly_south <- unique(dragonfly_south$eventID)
EID_greater_south <- unique(greater_south$eventID)
EID_caddiscase_south <- unique(caddisfly_case_south$eventID)

EID_blackfly_south
EID_mayfly_south
EID_worm_south 
EID_stonefly_south
EID_waterbeetle_south
EID_waterbeetlelarvae_south
EID_caddisfly_south
EID_diving_south 
EID_lesser_south
EID_damsel_south
EID_dragonfly_south
EID_greater_south
EID_caddiscase_south

#retrieving site IDs for every time each common name was recorded (north)
worm_north <- aidata_kicksampling_north %>% 
  filter(Commonname %in% "Worm")
stonefly_north <- aidata_kicksampling_north %>% 
  filter(Commonname %in%"Stonefly_larvae")
waterbeetle_larvae_north <- aidata_kicksampling_north %>% 
  filter(Commonname %in% "Waterbeetle_larvae")
waterbeetle_north <- aidata_kicksampling_north %>% 
  filter(Commonname %in% "Waterbeetle ")
caddisfly_larvae_north <- aidata_kicksampling_north %>% 
  filter(Commonname %in%"Caddisfly_larvae")
mayfly_north <- aidata_kicksampling_north %>% 
  filter(Commonname %in%"Mayfly_larvae")
dragonfly_north <- aidata_kicksampling_north %>% 
  filter(Commonname %in%"Dragonfly_larvae")
cased_caddisfly_larvae_north <- aidata_kicksampling_north %>% 
  filter(Commonname %in%"Cased_caddisfly_larvae")
caseless_caddisfly_larvae_north <- aidata_kicksampling_north %>% 
  filter(Commonname %in%"Caseless_caddisfly_larvae")

EID_worm_north <- unique(worm_north$eventID)
EID_stonefly_north <- unique(stonefly_north$eventID)
EID_waterbeetle_north <- unique(waterbeetle_north$eventID)
EID_waterbeetlelarvae_north <- unique(waterbeetle_larvae_north$eventID)
EID_caddisfly_north <- unique(caddisfly_larvae_north$eventID)
EID_mayfly_north <- unique(mayfly_north$eventID)
EID_dragonfly_north <- unique(dragonfly_north$eventID)
EID_cased_caddisfly_north <- unique(cased_caddisfly_larvae_north$eventID)
EID_caseless_caddisfly_north <- unique(caseless_caddisfly_larvae_north$eventID)

EID_mayfly_north
EID_dragonfly_north
EID_stonefly_north
EID_worm_north
EID_cased_caddisfly_north
EID_waterbeetle_north
EID_caseless_caddisfly_north
EID_caddisfly_north
