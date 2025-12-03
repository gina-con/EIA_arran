rm(list=ls())
birddata <- read.csv('Data/bird_data.csv', header = TRUE)
library(dplyr)
library(ggplot2)


#south walking transect
#subsetting by eventID for associated observations
sparentid_bird <- c('S3B', 'S1B', 'S2B')

birddata_bird_transect_south <- birddata %>% 
  filter(eventID %in% sparentid_bird)

#analysis - returning unique/distinct species for South, transect
unique_species_bird_transect_south <- birddata_bird_transect_south %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_bird_transect_south <- c(unique_species_bird_transect_south$Commonname)
species_bird_transect_south <- c(unique_species_bird_transect_south$scientificName)
south_bird_transect_dataframe <- data.frame(Site = "South", Survey = "Walking Transect", Species = species_bird_transect_south, "Common Name" = Commonname_bird_transect_south, 
                                      check.rows=TRUE)

#north walking transect
#subsetting by eventID for associated observations
#subsetting
nparentid_bird <- c('N1B', 'N3B', 'N2B')

birddata_bird_transect_north <- birddata %>% 
  filter(eventID %in% nparentid_bird)

#analysis
unique_species_bird_transect_north <- birddata_bird_transect_north %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_bird_transect_north <- c(unique_species_bird_transect_north$Commonname)
species_bird_transect_north <- c(unique_species_bird_transect_north$scientificName)
north_bird_transect_dataframe <- data.frame(Site = "North", Survey = "Walking Transect", Species = species_bird_transect_north, "Common Name" = Commonname_bird_transect_north, 
                                            check.rows=TRUE)

#south out of bounds - subsetting birds sighted out of bounds of the transect
#subsetting by eventID for associated observations
#subsetting
soobparentid_bird <- c('S2B_OoB')

birddata_bird_outofbounds_south <- birddata %>% 
  filter(eventID %in% soobparentid_bird)
#analysis - unique/distinct species
unique_species_bird_outofboounds_south <- birddata_bird_outofbounds_south %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_bird_outofbounds_south <- c(unique_species_bird_outofboounds_south$Commonname)
species_bird_outofbounds_south <- c(unique_species_bird_outofboounds_south$scientificName)
south_bird_outofbounds_dataframe <- data.frame(Site = "South", Survey = "Walking Transect - Out of Bounds", Species = species_bird_outofbounds_south, "Common Name" = Commonname_bird_outofbounds_south, 
                                            check.rows=TRUE)

#north out of bounds 
#subsetting by eventID for associated observations
#subsetting
noobparentid_bird <- c('N1B_OoB', 'N3B_OoB')

birddata_bird_outofbounds_north <- birddata %>% 
  filter(eventID %in% noobparentid_bird)

#analysis
unique_species_bird_outofboounds_north <- birddata_bird_outofbounds_north %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_bird_outofbounds_north <- c(unique_species_bird_outofboounds_north$Commonname)
species_bird_outofbounds_north <- c(unique_species_bird_outofboounds_north$scientificName)
north_bird_outofbounds_dataframe <- data.frame(Site = "North", Survey = "Walking Transect - Out of Bounds", Species = species_bird_outofbounds_north, "Common Name" = Commonname_bird_outofbounds_north, 
                                               check.rows=TRUE)

#south incidental 
#subsetting birds sighted/heard incidentally
sincparentid_bird <- c('SIB_OoB')

birddata_bird_incidental_south <- birddata %>% 
  filter(eventID %in% sincparentid_bird)

#analysis
unique_species_bird_incidental_south <- birddata_bird_incidental_south %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_bird_incidental_south <- c(unique_species_bird_incidental_south$Commonname)
species_bird_incidental_south <- c(unique_species_bird_incidental_south$scientificName)
south_bird_incidental_dataframe <- data.frame(Site = "South", Survey = "Incidental Sighting", Species = species_bird_incidental_south, "Common Name" = Commonname_bird_incidental_south, 
                                               check.rows=TRUE)

#north incidental 
#subsetting 
nincparentid_bird <- c('NB_I')

birddata_bird_incidental_north <- birddata %>% 
  filter(eventID %in% nincparentid_bird)

#analysis
unique_species_bird_incidental_north <- birddata_bird_incidental_north %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_bird_incidental_north <- c(unique_species_bird_incidental_north$Commonname)
species_bird_incidental_north <- c(unique_species_bird_incidental_north$scientificName)
north_bird_incidental_dataframe <- data.frame(Site = "North", Survey = "Incidental Sighting", Species = species_bird_incidental_north, "Common Name" = Commonname_bird_incidental_north, 
                                              check.rows=TRUE)

#combining dataframes to create big organised bird dataframe

south_birds <- rbind(south_bird_transect_dataframe, south_bird_outofbounds_dataframe, south_bird_incidental_dataframe)
north_birds <- rbind(north_bird_transect_dataframe, north_bird_outofbounds_dataframe, north_bird_incidental_dataframe)

birds <- rbind(south_birds, north_birds)

#creating dataframe with unique species and site 
unique_south_birds <- unique(south_birds$Species)
unique_south_birds
#11 is empty
unique_south_birds <- unique_south_birds[-11]

unique_south_birds_df <- south_birds %>%
  distinct(Species, .keep_all = TRUE)
unique_south_birds_df <- unique_south_birds_df[-11,]
unique_south_birds_df
gt(unique_south_birds_df)

unique_north_birds <- unique(north_birds$Species)
unique_north_birds
#12 is unidentified
unique_north_birds <- unique_north_birds[-12]

unique_north_birds_df <- north_birds %>%
  distinct(Species, .keep_all = TRUE)
unique_north_birds_df <- unique_north_birds_df[-12,]
gt(unique_north_birds_df)

#creating dataframe using unique species name for each site from above for combined species richness graph
south_bird_dataframe <- data.frame(Taxa = "Birds", Site = "South", Species = unique_south_birds,
                                  check.rows=TRUE)
north_bird_dataframe <- data.frame(Taxa = "Birds", Site = "North", Species = unique_north_birds,
                                   check.rows=TRUE)

birds_combined <- rbind(south_bird_dataframe, north_bird_dataframe)
write.csv(birds_combined, "C:\\Users\\gcon0\\Documents\\university\\Professional skills\\assessment\\data_analysis\\Data\\for_combined_figure\\birds_combined.csv", row.names=FALSE)

#venn diagram to compare sites unique and shared species
vennlist <- list('South'=c(unique_south_birds), 'North'=c(unique_north_birds))
birds_venn <- ggVennDiagram(vennlist, label_alpha = 0, label="count")+
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
  theme(legend.position = "none")+ 
  ggtitle("Number of Unique Bird Species Identified")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  coord_flip()

#identifying any protected birds/birds on Scottish Biodiversity List identified at each site
protected_north <- north_birds %>%
  filter(Species %in% protected_birds_species)

protected_south <- south_birds %>% 
  filter(Species %in% protected_birds_species)

protected_north
protected_south

#scottish biodiversity list
SBL_bird_north <- north_birds %>%
  filter(Species %in% SBL_bird_species_list)

SBL_bird_south <- south_birds %>% 
  filter(Species %in% SBL_bird_species_list)
SBL_bird_north
SBL_bird_south

#dataframes for both sites, containing abundance data (no removal of duplicate species entries as above)
south_abundance_df_birds <- rbind(birddata_bird_transect_south, birddata_bird_outofbounds_south, birddata_bird_incidental_south)
south_abundance_df_birds$site <- "South"
#remove any spaces from end of name to ensure unique
south_abundance_df_birds$scientificName <- trimws(south_abundance_df_birds$scientificName)
#removing nest
south_abundance_df_birds <- south_abundance_df_birds[-16,]

north_abundance_df_birds <- rbind(birddata_bird_transect_north, birddata_bird_outofbounds_north, birddata_bird_incidental_north)
north_abundance_df_birds$site <- "North"
#remove any spaces from end of name
north_abundance_df_birds$scientificName <- trimws(north_abundance_df_birds$scientificName)
#removing unidentified 
north_abundance_df_birds <- north_abundance_df_birds[-20,]

combined_abundance_df_birds <- rbind(south_abundance_df_birds, north_abundance_df_birds)

#south abundance plot
south_abundance_plot_birds <- ggplot(data = south_abundance_df_birds, aes(x=individualCount, y=Commonname, fill = scientificName))+
  geom_col()+
  theme_classic()+
  theme(legend.position="none")+
  xlab("Abundance")+
  ylab("Species Name")+
  ggtitle("Bird Species Abundance on the South Site")+
  scale_x_continuous(breaks=seq(0,15,by=2))+
  theme(plot.title = element_text(size = 10))

#north abundance plot
north_abundance_plot_birds <- ggplot(data = north_abundance_df_birds, aes(x=individualCount, y=Commonname, fill = scientificName))+
  geom_col()+
  theme_classic()+
  theme(legend.position="none")+
  xlab("Abundance")+
  ylab("Species Name")+
  ggtitle("Bird Species Abundance on the North Site")+
  scale_x_continuous(breaks=seq(0,15,by=2))+
  theme(plot.title = element_text(size = 10))

#combined abundance plot
combined_abundance_plot_birds <- ggplot(data = combined_abundance_df_birds, aes(x=individualCount, y=Commonname, fill = site))+
  geom_col(position="dodge")+
  theme_classic()+
  xlab("Abundance")+
  ylab("Species Name")

#total number of birds identifed per site
totalsampled_plot_birds <- ggplot(data = combined_abundance_df_birds, aes(x=site, y=individualCount, fill = site))+
  geom_col()+
  theme_classic()+
  xlab("Site")+
  ylab("Number of Birds Sampled")+
  labs(fill = "Site")+
  ggtitle("Total Number of Birds Sampled")+
  scale_y_continuous(breaks=seq(0,60,by=5))+
  theme(plot.title = element_text(size = 10))

#combining into one figure
cowplot::plot_grid(north_abundance_plot_birds, south_abundance_plot_birds, totalsampled_plot_birds, ncol = 3, labels=c("A)", "B)", "C)"))
