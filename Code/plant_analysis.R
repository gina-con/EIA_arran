rm(list=ls())
plantdata <- read.csv('Data/plant_data.csv', header = TRUE)
library(dplyr)
library(ggplot2)
library(ggVennDiagram)
library(gt)

#vectors with ids for each altitude of each site 
north_high_ID <- c("N1P_A", "N1P_B", "N1P_C", "N1P_D", "N1P_E", "N1P_F", "N1P_G", "N1P_H", "N1P_I", "N1P_J")

south_high_ID <- c("S1P_A", "S1P_B", "S1P_C", "S1P_D", "S1P_E", "S1P_F", "S1P_G", "S1P_H", "S1P_I", "S1P_J")

north_mid_ID <- c("N2P_A", "N2P_B", "N2P_C", "N2P_D", "N2P_E", "N2P_F", "N2P_G", "N2P_H", "N2P_I", "N2P_J")

south_mid_ID <- c("S2P_A", "S2P_B", "S2P_C", "S2P_D", "S2P_E", "S2P_F", "S2P_G", "S2P_H", "S2P_I", "S2P_J")

north_low_ID <- c("N3P_A", "N3P_B", "N3P_C", "N3P_D", "N3P_E", "N3P_F", "N3P_G", "N3P_H", "N3P_I", "N3P_J")

south_low_ID <- c("S3P_A", "S3P_B", "S3P_C", "S3P_D", "S3P_E", "S3P_F", "S3P_G", "S3P_H", "S3P_I", "S3P_J")

#subsetting by altitude and site 
plantdata_north_high <- plantdata %>% 
  filter(eventID %in% north_high_ID)

plantdata_south_high <- plantdata %>% 
  filter(eventID %in% south_high_ID)

plantdata_north_mid <- plantdata %>% 
  filter(eventID %in% north_mid_ID)

plantdata_south_mid <- plantdata %>% 
  filter(eventID %in% south_mid_ID)

plantdata_north_low <- plantdata %>% 
  filter(eventID %in% north_low_ID)

plantdata_south_low <- plantdata %>% 
  filter(eventID %in% south_low_ID)

#retrieving unique species and creating dataframes
unique_species_plant_north_high <- plantdata_north_high%>% 
  distinct(scientificName, .keep_all = TRUE)

#high
Commonname_plant_north_high <- c(unique_species_plant_north_high$Commonname)
species_plant_north_high <- c(unique_species_plant_north_high$scientificName)
north_plant_dataframe_high <- data.frame(Altitude = "High", Species = species_plant_north_high, "Common Name" = Commonname_plant_north_high, 
                                  check.rows=TRUE)

unique_species_plant_south_high <- plantdata_south_high%>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_plant_south_high <- c(unique_species_plant_south_high$Commonname)
species_plant_south_high <- c(unique_species_plant_south_high$scientificName)
south_plant_dataframe_high <- data.frame(Altitude = "High", Species = species_plant_south_high, "Common Name" = Commonname_plant_south_high, 
                                  check.rows=TRUE)

#mid 
unique_species_plant_north_mid <- plantdata_north_mid%>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_plant_north_mid <- c(unique_species_plant_north_mid$Commonname)
species_plant_north_mid <- c(unique_species_plant_north_mid$scientificName)
north_plant_dataframe_mid <- data.frame(Altitude = "Mid", Species = species_plant_north_mid, "Common Name" = Commonname_plant_north_mid, 
                                  check.rows=TRUE)

unique_species_plant_south_mid <- plantdata_south_mid%>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_plant_south_mid <- c(unique_species_plant_south_mid$Commonname)
species_plant_south_mid <- c(unique_species_plant_south_mid$scientificName)
south_plant_dataframe_mid <- data.frame(Altitude = "Mid", Species = species_plant_south_mid, "Common Name" = Commonname_plant_south_mid, 
                                  check.rows=TRUE)

#low
unique_species_plant_north_low <- plantdata_north_low%>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_plant_north_low <- c(unique_species_plant_north_low$Commonname)
species_plant_north_low <- c(unique_species_plant_north_low$scientificName)
north_plant_dataframe_low <- data.frame(Altitude = "Low", Species = species_plant_north_low, "Common Name" = Commonname_plant_north_low, 
                                  check.rows=TRUE)

unique_species_plant_south_low <- plantdata_south_low%>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_plant_south_low <- c(unique_species_plant_south_low$Commonname)
species_plant_south_low <- c(unique_species_plant_south_low$scientificName)
south_plant_dataframe_low <- data.frame(Altitude = "Low", Species = species_plant_south_low, "Common Name" = Commonname_plant_south_low, 
                                  check.rows=TRUE)

#combining into south and north
south_plants <- rbind(south_plant_dataframe_high, south_plant_dataframe_mid, south_plant_dataframe_low)
south_plants$Species <- trimws(south_plants$Species)
north_plants <- rbind(north_plant_dataframe_high, north_plant_dataframe_mid, north_plant_dataframe_low)
north_plants$Species <- trimws(north_plants$Species)

#creating venn diagram with unique species list for each site 
unique_south_plants <- unique(south_plants$Species)
unique_north_plants <- unique(north_plants$Species)

vennlist_plant <- list('South'=c(unique_south_plants), 'North'=c(unique_north_plants))
plant_venn <- ggVennDiagram(vennlist_plant, label_alpha = 0, label="count")+
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
  theme(legend.position = "none")+ 
  ggtitle("Number of Unique Plant Species Identified")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  coord_flip()

plant_venn

#combining North and South unique species and saving into csv for combined figure species richness figure
south_plants$Site <- "South"
north_plants$Site <- "North"

south_plants_unique <- south_plants %>% 
  distinct(Species, .keep_all = TRUE)

north_plants_unique <- north_plants %>% 
  distinct(Species, .keep_all = TRUE)

combined_plants <- rbind(south_plants_unique, north_plants_unique)

plants_combined <- data.frame(Taxa = "Plants", Site = combined_plants$Site, Species = combined_plants$Species,
                                   check.rows=TRUE)

write.csv(plants_combined, "C:\\Users\\gcon0\\Documents\\university\\Professional skills\\assessment\\data_analysis\\Data\\for_combined_figure\\plants_combined.csv", row.names=FALSE)

#outputting table for appendix 
gt(combined_plants)

#identifying any protected plants/plants on Scottish Biodiversity List identified at each site
protected_north_plants <- north_plants_unique %>%
  filter(Species %in% protected_plants_species)

protected_south_plants <- south_plants_unique %>% 
  filter(Species %in% protected_plants_species)

protected_north_plants
#none?
protected_south_plants
#none?

SBL_plants_north <- north_plants_unique %>%
  filter(Species %in% SBL_plant_species_list)

SBL_plants_south <- south_plants_unique %>% 
  filter(Species %in% SBL_plant_species_list)

SBL_plants_north
#none?
SBL_plants_south
#none?

gt(combined_plants)

