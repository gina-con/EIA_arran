rm(list=ls())
mamdata <- read.csv('Data/mammal_data.csv', header = TRUE)
library(dplyr)
library(ggplot2)

basis <- c("Humanobservation")

#south incidental
#subsetting South from eventID
sparentid_mammal <- c('S3MR','S1MR','S2MR')

mamdata_mammal_south <- mamdata %>% 
  filter(basisOfRecord %in% basis)%>%
  filter(eventID %in% sparentid_mammal)

#analysis - finding unique/distinct species
unique_species_mammal_south <- mamdata_mammal_south %>% 
  distinct(scientificName, .keep_all = TRUE)

mammal_species_south <- unique_species_mammal_south$scientificName
mammal_commonname_south <- unique_species_mammal_south$Commonname
south_mammal_df <- data.frame(Site = "South", Species = mammal_species_south, "Common Name"= mammal_commonname_south)

#north incidental 
#subsetting North from eventID
nparentid_mammal <- c('N3MR','N2MR','N1MR')

mamdata_mammal_north <- mamdata %>% 
  filter(basisOfRecord %in% basis)%>%
  filter(eventID %in% nparentid_mammal)

#analysis
unique_species_mammal_north <- mamdata_mammal_north %>% 
  distinct(scientificName, .keep_all = TRUE)

mammal_species_north <- unique_species_mammal_north$scientificName
mammal_commonname_north <- unique_species_mammal_north$Commonname
north_mammal_df <- data.frame(Site = "North", Species = mammal_species_north, "Common Name"= mammal_commonname_north)

#combining South and North data frames for master figure R code
combined_mammals <- rbind(north_mammal_df, south_mammal_df)

mammal_unique_df <- data.frame(Taxa = "Mammals", 
                               Site = combined_mammals$Site,
                               Species = combined_mammals$Species)
mammal_unique_df
write.csv(mammal_unique_df, "C:\\Users\\gcon0\\Documents\\university\\Professional skills\\assessment\\data_analysis\\Data\\for_combined_figure\\mammals_combined.csv", row.names=FALSE)

#creating a venn diagram 
#creating lists for venn list for venn diagram
species_south <- south_mammal_df$Species
species_north <- north_mammal_df$Species

vennlist <- list('South'=c(species_south), 'North'=c(species_north))
mammals_venn <- ggVennDiagram(vennlist, label_alpha = 0, label="count")+
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
  theme(legend.position = "none")+ 
  ggtitle("Number of Unique Species of Mammal")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  coord_flip()

mammals_venn

#creating coordinate table for appendix 
#subsetting table to return badger and red squirrel rows
badgercoords <- mamdata %>%
  filter(Commonname %in% "Badger")

redsquirrelcoords <- mamdata %>%
  filter(Commonname %in% "Red_Squirrel")

#binding above rows together
mamcoords <- rbind(badgercoords, redsquirrelcoords)

#creating simpler table 
mamcoords_commonname <- mamcoords$Commonname
mamcoords_basis <- mamcoords$basisOfRecord
mamcoords_indcount <- mamcoords$individualCount
mamcoords_site <- mamcoords$eventID
mamcoords_latitude <- mamcoords$Latitude.N
mamcoords_longitude <- mamcoords$Longitude.W

mamcoords_table <- data.frame("Common Name"=mamcoords_commonname, 
                              "Basis of Record"=mamcoords_basis,
                              "Number of Individuals"=mamcoords_indcount,
                              "Site"=mamcoords_site,
                              "Latitude"=mamcoords_latitude,
                              "Longitude"=mamcoords_longitude)

gt(mamcoords_table)

