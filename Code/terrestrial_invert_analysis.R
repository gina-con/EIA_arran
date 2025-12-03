rm(list=ls())
tidata <- read.csv('Data/sweep_net_pitfall_data.csv')
modata <- read.csv('Data/mothtrap_data.csv')
library(dplyr)
library(ggplot2)
library(cowplot)

#pitfalls
#----

#subsetting pitfall trap observations (south)
#assigning eventIDs associated with pitfall traps on the South Site
sparentid_pitfall <- c('S3TIP_E','S3TIP_I','S3TIP_J', 'S2TIP_C')

tidata_pitfall_south <- tidata %>% 
  filter(eventID %in% sparentid_pitfall)

#finding unique species recorded in south pitfalls
unique_species_pitfall_south <- tidata_pitfall_south %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_pitfall_south <- c(unique_species_pitfall_south$Commonname)
species_pitfall_south <- c(unique_species_pitfall_south$scientificName)
ID_pitfall_south <- c(unique_species_pitfall_south$eventID)
order_pitfall_south <- c(unique_species_pitfall_south$Order)
south_pitfall_dataframe <- data.frame(Site = "South", Survey = "Pitfall Trap", "Location ID" = ID_pitfall_south, Order = order_pitfall_south, Species = species_pitfall_south, "Common Name" = Commonname_pitfall_south, 
                                            check.rows=TRUE)

#north
#----
#subsetting pitfalls (north)
#assigning eventIDs associated with pitfall traps on the North Site
nparentid_pitfall <- c('N3TIP_B','N3TIP_C','N3TIP_D', 'N2TIP_J')

tidata_pitfall_north <- tidata %>% 
  filter(eventID %in% nparentid_pitfall)

#analysing pitfalls (north)
unique_species_pitfall_north <- tidata_pitfall_north %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_pitfall_north <- c(unique_species_pitfall_north$Commonname)
species_pitfall_north <- c(unique_species_pitfall_north$scientificName)
ID_pitfall_north <- c(unique_species_pitfall_north$eventID)
order_pitfall_north <- c(unique_species_pitfall_north$Order)
north_pitfall_dataframe <- data.frame(Site = "North", Survey = "Pitfall Trap", "Location ID"= ID_pitfall_north, Order = order_pitfall_north, Species = species_pitfall_north, "Common Name" = Commonname_pitfall_north, 
                                       check.rows=TRUE)
#

#netting
#----
#subsetting trap netting (south)
#assigning eventIDs associated with sweep netting on the South Site

sparentid_net <- c('S3TIN_A', 'S3TIN_B', 'S3TIN_C', 'S3TIN_D', 'S3TIN_E', 'S3TIN_F', 'S3TIN_G', 'S3TIN_H', 'S3TIN_I','S3TIN_J',
                   'S2TIN_A', 'S2TIN_B', 'S2TIN_C', 'S2TIN_D', 'S2TIN_E', 'S2TIN_F', 'S2TIN_G', 'S2TIN_H', 'S2TIN_I','S2TIN_J',
                   'S1TIN_A', 'S1TIN_B', 'S1TIN_C', 'S1TIN_D', 'S1TIN_E', 'S1TIN_F', 'S1TIN_G', 'S1TIN_H')

tidata_net_south <- tidata %>% 
  filter(eventID %in% sparentid_net)

#retrieving unique species identified from net sweeping (south)
unique_species_net_south <- tidata_net_south %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_net_south <- c(unique_species_net_south$Commonname)
species_net_south <- c(unique_species_net_south$scientificName)
ID_net_south <- c(unique_species_net_south$eventID)
order_net_south <- c(unique_species_net_south$Order)
south_net_dataframe <- data.frame(Site = "South", Survey = "Sweep Netting", "Location ID" = ID_net_south, Order = order_net_south, Species = species_net_south, "Common Name" = Commonname_net_south, 
                                       check.rows=TRUE)
#----
#subsetting trap netting (north)
#assigning eventIDs associated with sweep netting on the North Site
nparentid_net <- c('N3TIN_A', 'N3TIN_B', 'N3TIN_C', 'N3TIN_D', 'N3TIN_E', 'N3TIN_F', 'N3TIN_G', 'N3TIN_H', 'N3TIN_I','N3TIN_J',
                   'N2TIN_A', 'N2TIN_B', 'N2TIN_C', 'N2TIN_D', 'N2TIN_E', 'N2TIN_F', 'N2TIN_G', 'N2TIN_H', 'N2TIN_I','N2TIN_J',
                   'N1TIN_A', 'N1TIN_B', 'N1TIN_C', 'N1TIN_D', 'N1TIN_E', 'N1TIN_F', 'N1TIN_G', 'N1TIN_H')

tidata_net_north<- tidata %>% 
  filter(eventID %in% nparentid_net)

#retrieving unique species from net sweeping (north)
unique_species_net_north <- tidata_net_north %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_net_north <- c(unique_species_net_north$Commonname)
species_net_north <- c(unique_species_net_north$scientificName)
ID_net_north <- c(unique_species_net_north$eventID)
order_net_north <- c(unique_species_net_north$Order)
north_net_dataframe <- data.frame(Site = "North", Survey = "Sweep Netting", "Location ID" = ID_net_north, Order = order_net_north, Species = species_net_north, "Common Name" = Commonname_net_north, 
                                   check.rows=TRUE)
#

#moth trapping
#----
#subsetting data
#assigning eventIDs associated with moth trapping on the South Site
sparentid_moth <- c('S3TIT_A', 'S3TIT_E', 'S3TIT_J')

modata_moth_south <- modata %>% 
  filter(eventID %in% sparentid_moth)

#retrieving unique species from moth trapping on South Site
unique_species_modata_moth_south <- modata_moth_south %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_moth_south <- c(unique_species_modata_moth_south$Commonname)
species_moth_south <- c(unique_species_modata_moth_south$scientificName)
ID_moth_south <- c(unique_species_modata_moth_south$eventID)
order_moth_south <- c(unique_species_modata_moth_south$Order)
south_moth_dataframe <- data.frame(Site = "South", Survey = "Moth Trap", "Location ID" = ID_moth_south, Order = order_moth_south, Species = species_moth_south, "Common Name" = Commonname_moth_south, 
                                   check.rows=TRUE)
#----
#subsetting data
#assigning eventIDs associated with moth trapping on the North Site
nparentid_moth <- c('N3TIT_A', 'N3TIT_E', 'N3TIT_J')

modata_moth_north <- modata %>% 
  filter(eventID %in% nparentid_moth)

#retrieving unique species from moth trapping on North Site
unique_species_modata_moth_north <- modata_moth_north %>% 
  distinct(scientificName, .keep_all = TRUE)

Commonname_moth_north <- c(unique_species_modata_moth_north$Commonname)
species_moth_north <- c(unique_species_modata_moth_north$scientificName)
ID_moth_north <- c(unique_species_modata_moth_north$eventID)
order_moth_north <- c(unique_species_modata_moth_north$Order)
north_moth_dataframe <- data.frame(Site = "North", Survey = "Moth Trap", "Location ID" = ID_moth_north, Order = order_moth_north, Species = species_moth_north, "Common Name" = Commonname_moth_north, 
                                   check.rows=TRUE)

#combining all the datasets 
north_terr <- rbind(north_moth_dataframe, north_net_dataframe, north_pitfall_dataframe)
south_terr <- rbind(south_moth_dataframe, south_net_dataframe, south_pitfall_dataframe)
comb_terr <- rbind(north_terr, south_terr)

unique_north_terr <- unique(north_terr$Species)
unique_north_terr
#23 and 27 are the same, spelt differently, removing
unique_north_terr <- unique_north_terr[-23]
north_terr <- north_terr[-23,]
unique_south_terr <- unique(south_terr$Species)
unique_south_terr
#16 is a replicate, removing
unique_south_terr <- unique_south_terr[-16]
south_terr <- south_terr[-16,]

#creating dataframe containing unique species and the sites for master species richness graph
terrestrial_unique_south <- data.frame(Taxa = "Terrestrial Invertebrates", 
                                       Site = "South",
                                       Species = unique_south_terr)
terrestrial_unique_north <- data.frame(Taxa = "Terrestrial Invertebrates", 
                                       Site = "North",
                                       Species = unique_north_terr)
terrestrial_unique_combined <- (rbind(terrestrial_unique_south, terrestrial_unique_north))
terrestrial_unique_combined

#storing in csv for use in master figure script
write.csv(terrestrial_unique_combined, "C:\\Users\\gcon0\\Documents\\university\\Professional skills\\assessment\\data_analysis\\Data\\for_combined_figure\\terrinv_combined.csv", row.names=FALSE)

#figures
#making venn diagram to compare species in north and south 
library(ggVennDiagram)
library(ggplot2)

#creating list with unique species for each site
vennlist_terr <- list('South'=c(unique_south_terr), 'North'=c(unique_north_terr))
terr_venn <- ggVennDiagram(vennlist_terr, label_alpha = 0, label="count")+
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
  theme(legend.position = "none")+ 
  ggtitle("Number of Unique Species of Terrestrial Invertebrates", )+
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  coord_flip()
  
terr_venn

#creating tables 
library(gt)

gt(north_terr)
gt(south_terr)

#--
#comparing to Scottish Protected Species from list vector created in 
#protected_species analysis.R of terrestrial invertebrates listed on the 
#Protected Species list provided by NatureScot.
priority_south <- south_terr %>% 
  filter(Species %in% priority_inverts)
priority_north <- north_terr %>% 
  filter(Species %in% priority_inverts)

#comparing to Scottish Biodiversity List from list vector created in 
#protected_species analysis.R of terrestrial invertebrates listed on the Scottish Biodiversity List  
SBL_terr_north <- terrestrial_unique_north %>%
  filter(Species %in% SBL_terr_species_list)

SBL_terr_south <- terrestrial_unique_south %>% 
  filter(Species %in% SBL_terr_species_list)

SBL_terr_north
SBL_terr_south

#---
#creating abundance graphs 
#binding together dataframes subsetted by sampling method 
south_abundance_df <- rbind(tidata_pitfall_south, tidata_net_south, modata_moth_south)
south_abundance_df$site <- "South"
#remove any spaces from end of name
south_abundance_df$scientificName <- trimws(south_abundance_df$scientificName)

#repeating for north
north_abundance_df <- rbind(tidata_pitfall_north, tidata_net_north, modata_moth_north)
north_abundance_df$site <- "North"
#remove any spaces from end of name
north_abundance_df$scientificName <- trimws(north_abundance_df$scientificName)

combined_abundance_df <- rbind(south_abundance_df, north_abundance_df)

#south abundance plot
south_abundance_plot <- ggplot(data = south_abundance_df, aes(x=individualCount, y=Commonname, fill = Commonname))+
  geom_col()+
  theme_classic()+
  theme(legend.position="none")+
  xlab("Abundance")+
  ylab("Species Name")+
  ggtitle("South Site Terrestrial Invertebrate Species Abundance")+
  scale_x_continuous(breaks=seq(0,200,by=20))+
  theme(plot.title = element_text(size = 7))

#north abundance plot
north_abundance_plot <- ggplot(data = north_abundance_df, aes(x=individualCount, y=Commonname, fill = Commonname))+
  geom_col()+
  theme_classic()+
  theme(legend.position="none")+
  xlab("Abundance")+
  ylab("Species Name")+
  ggtitle("North Site Terrestrial Invertebrate Species Abundance")+
  scale_x_continuous(breaks=seq(0,200,by=5))+
  theme(plot.title = element_text(size = 7))

#combined abundance by species
ggplot(data = combined_abundance_df, aes(x=individualCount, y=scientificName, fill = site))+
  geom_col(position="dodge")+
  theme_classic()+
  xlab("Abundance")+
  ylab("Species Name")

#combined abundance total
combined_abundance <- ggplot(data = combined_abundance_df, aes(x=site, y=individualCount, fill = site))+
  geom_col()+
  theme_classic()+
  xlab("Site")+
  ylab("Number of Invertebrates Sampled")+
  labs(fill = "Site")+
  ggtitle("Total Number of Terrestrial Invertebrates Sampled")+
  theme(plot.title = element_text(size = 8))

#creating one big figure from abundance graphs  
cowplot::plot_grid(north_abundance_plot, south_abundance_plot, combined_abundance, ncol = 3, labels=c("A)", "B)", "C)"))
