rm(list=ls())
batdata <- read.csv('Data/bat_data.csv', header = TRUE)
library(dplyr)
library(ggplot2)
library(ggVennDiagram)
library(gt)

#south 
#subsetting by eventID for South Site AudioMoths

sparentid_bat <- c("S3CR_A", "S2CR_A", "S2CR_B", "S3CR_B", "S3CR_C")
batdata_south <- batdata %>% 
  filter(eventID %in% sparentid_bat)


#north
#subsetting by eventID for North Site AudioMoths

nparentid_bat <- c("N3CR_A", "N2CR_B", "N2CR_A", "N3CR_B", "N1CR_C", "N1CR_B")
batdata_north <- batdata %>% 
  filter(eventID %in% nparentid_bat)

#analysis south - retrieving unique species
unique_species_bat_south <- batdata_south%>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_bat_south <- c(unique_species_bat_south$Commonname)
species_bat_south <- c(unique_species_bat_south$scientificName)
south_bat_dataframe <- data.frame(Site = "South", Species = species_bat_south, "Common Name" = Commonname_bat_south, 
                                            check.rows=TRUE)
south_bat_df_for_fig <- data.frame(Taxa = "Mammal - Bats", Site = "South", Species = species_bat_south,
                                  check.rows=TRUE)
#some are not unique, maybe due to spelling differences?
#row 5, 6 removed

south_bat_dataframe <- south_bat_dataframe[-c(5,6),]
south_bat_df_for_fig <- south_bat_df_for_fig[-c(5,6),]
south_bat_dataframe

#analysis north - retrieving unique species
unique_species_bat_north <- batdata_north%>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_bat_north <- c(unique_species_bat_north$Commonname)
species_bat_north <- c(unique_species_bat_north$scientificName)
north_bat_dataframe <- data.frame(Site = "North", Species = species_bat_north, "Common Name" = Commonname_bat_north, 
                                  check.rows=TRUE)
north_bat_df_for_fig <- data.frame(Taxa = "Mammal - Bats", Site = "North", Species = species_bat_north,
                                   check.rows=TRUE)
north_bat_dataframe
#2 and 3 some are not unique due to spelling difference, removing 
north_bat_dataframe <- north_bat_dataframe[-3,]
north_bat_df_for_fig <- north_bat_dataframe[-3,]

north_bat_dataframe
#venn diagram to compare
unique_north <- north_bat_dataframe$Species
unique_south <- south_bat_dataframe$Species
unique_bat <- rbind(north_bat_dataframe, south_bat_dataframe)

vennlist_bat <- list('South'=c(unique_south), 'North'=c(unique_north))
bats_venn <- ggVennDiagram(vennlist_bat, label_alpha = 0, label="count")+
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
  theme(legend.position = "none")+ 
  ggtitle("Number of Unique Bat Species Identified")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  coord_flip()


gt(north_bat_dataframe)
gt(south_bat_dataframe)

#combining for master figure 

bats_combined <- rbind(south_bat_df_for_fig, north_bat_df_for_fig)
bats_combined
write.csv(bats_combined, "C:\\Users\\gcon0\\Documents\\university\\Professional skills\\assessment\\data_analysis\\Data\\for_combined_figure\\bats_combined.csv", row.names=FALSE)

