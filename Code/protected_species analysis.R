#PROTECTED SPECIES IN SCOTLAND ANALYSIS (NatureScot)
#data taken directly from NatureScot and can be found: https://www.nature.scot/doc/table-all-scotlands-protected-species
#Published in 2022 

install.packages("gt")
library(dplyr)
library(tidyverse)
library(gt)

#reading in data
protected_species_df <- read.csv('Data/protected_species.csv', header=TRUE)
#retrieving all the names of all the different taxon sampled
unique(protected_species_df$Taxon)


#subsetting data into taxon dataframes
protected_birds <- protected_species_df %>% 
  filter(Taxon %in% 'Bird')

protected_reptiles <- protected_species_df %>%
  filter(Taxon %in% "Reptile")

protected_mammal <- protected_species_df %>%
  filter(Taxon %in% "Mammal")

protected_moss <- protected_species_df %>%
  filter(Taxon %in% "Moss")

protected_lichen <- protected_species_df %>%
  filter(Taxon %in% "Lichen")

protected_vplant <- protected_species_df %>% 
  filter(Taxon %in% "Vascular Plant")

protected_fish <- protected_species_df %>%
  filter(Taxon %in% "Fish")

protected_crustacean <- protected_species_df %>%
  filter(Taxon %in% "Crustacean")

protected_butterfly <- protected_species_df %>%
  filter(Taxon %in% "Butterfly")

protected_amphibian <- protected_species_df %>%
  filter(Taxon %in% "Amphibian")

protected_mollusc <- protected_species_df %>%
  filter(Taxon %in% "Mollusc")

protected_stonewort <- protected_species_df %>%
  filter(Taxon %in% "Stonewort")

protected_liverwort <- protected_species_df %>%
  filter(Taxon %in% "Liverwort")

protected_annelid_worm <- protected_species_df %>%
  filter(Taxon %in% "Annelid worm")

protected_moth <- protected_species_df %>%
  filter(Taxon %in% "Moth")

protected_fungi <- protected_species_df %>%
  filter(Taxon %in% "Fungi")

#binding into bigger groups
protected_butterfly_moth <- rbind(protected_butterfly, protected_moth)

protected_non_animal <- rbind(protected_moss, protected_lichen, protected_fungi, protected_vplant, protected_liverwort, protected_stonewort)

protected_mollusc_crustacean <- rbind(protected_mollusc, protected_crustacean)

protected_reptile_amphibian <- rbind(protected_reptiles, protected_amphibian)

protected_butterfly_moth
protected_vplant

#creating vector with the names and species of protected bird species known in Scotland
protected_birds_commonname <- unique(protected_birds$Common.name)
protected_birds_species <- unique(protected_birds$Current.taxon.name)

#creating vector with names and species of protected plant species
#replacing space with _ to match plant excel format
protected_non_animal$Current.taxon.name <- sub(" ", "_", protected_non_animal$Current.taxon.name)
#creating list for comparison analysis in plant_analysis.R 
protected_plants_species <- unique(protected_non_animal$Current.taxon.name)

#----
#BUTTERFLY CONSERVATION ANALYSIS 

#butterfly conservation priority species in Scotland: https://butterfly-conservation.org/in-your-area/scottish-office/scottish-priority-species
#Priority species at time of study, October 2025.
#creating dataframes for butterfly, micro and macro moths of priority species.

butterfly_commonname <- c("Chequered Skipper", "Large Heath", "Pearl-bordered Fritillary", "Marsh Fritillary", "Northern Brown Argus", "Small Blue")
macro_moth_commonname <- c("White-barred Clearwing", "Large Red-belted Clearwing", "Transparent Burnet", "Slender Scotch Burnet", "New Forest Burnet", "Kentish Glory", "Barred Tooth-striped", "Dark Bordered Beauty", "Small Dark Yellow Underwing", "Portland Moth", "Silvery Arches", "Yellow-ringed Carpet", "Heath Rivulet")
micro_moth_commonname <- c("White-spotted Sable", "Affric Twitcher", "Tiree Twist", "Silver Shade", "N/A", "N/A", "Currant Shoot Borer", "N/A", "Highland Nymph")

butterfly_species <- c("Carterocephalus_palaemon", "Coenonympha_tullia", "Boloria_euphrosyne", "Euphydryas_aurinia", "Aricia_artaxerxces", "Cupido_minimus")
macro_moth_species <- c("Synanthedon_spheciformis", "Synanthedon_culiciformis", "Zygaena_purpuralis", "Zygaena_loti", "Zygaena_viciae", "Endromis_versicolora", "Trichopteryx_polycommata", "Epione_vespertaria", "Coranarta_cordigera", "Actebia_praecox", "Polia_hepatica", "Entephria_flavicinctata", "Perizoma_minorata")
micro_moth_species <- c("Anania_funebris", "Choreutis_diana", "Periclepsis_cinctana", "Eana_argentana", "Depressaria_olerella", "Anacampis_temerella", "Lampronia_capitella", "Lampronia_pubicornis", "Callisto_cofeella")

butterfly_df <- data.frame(Taxon="Butterfly", Species=butterfly_species, 'Common Name'=butterfly_commonname)
macro_moth_df <- data.frame(Taxon="Macro-moth", Species=macro_moth_species, 'Common Name'=macro_moth_commonname)
micro_moth_df <- data.frame(Taxon="Micro-moth", Species=micro_moth_species, 'Common Name'=micro_moth_commonname)

#combining into one big dataframe
butterfly_moth_priority_df <- rbind(butterfly_df, macro_moth_df, micro_moth_df)

#creating table with priority species 
butterfly_moth_priority_df %>%
  gt()%>%
  tab_header(title=md("Scottish Priority Butterfly and Moth Species"), subtitle = md("As identified by the Butterfly Conservation"))


#----
##NBN ANALYSIS

#reading in nbn atlas data for North Site, containing RSPB priority species, species on the Scottish Biodiversity List, and Invasive Species 
NBN_rspb_north <- read.csv('Data/NBN_atlas_north/north_site_rspb.csv', header=TRUE)
NBN_biodiversity_list_north <- read.csv('Data/NBN_atlas_north/north_site_biodiversity_list.csv', header=TRUE)
NBN_invasive_north <- read.csv('Data/NBN_atlas_north/north_site_invasive.csv', header=TRUE)

#creating dataframe with RSPB Priority Species List 
NBN_north_rspb_vernacular <- data.frame(NBN_rspb_north$Vernacular.Name, NBN_rspb_north$Class)
NBN_north_rspb_vernacular$Status <- "RSPB Priority Species"
colnames(NBN_north_rspb_vernacular) <- c("Species", "Class", "Status")
NBN_north_rspb_vernacular

#creating dataframe with species listed on the Scottish Biodiversity List
NBN_north_biodiversity_list_vernacular <- data.frame(NBN_biodiversity_list_north$Vernacular.Name, NBN_biodiversity_list_north$Class)
NBN_north_biodiversity_list_vernacular$Status <- "Scottish Biodiversity List" 
colnames(NBN_north_biodiversity_list_vernacular) <- c("Species", "Class", "Status")
NBN_north_biodiversity_list_vernacular

#creating dataframe with species listed as Invasive
NBN_north_invasive_vernacular <- data.frame(NBN_invasive_north$Vernacular.Name, NBN_invasive_north$Class, NBN_invasive_north$Invasive)
colnames(NBN_north_invasive_vernacular) <- c("Species", "Class", "Status")
NBN_north_invasive_vernacular

NBN_north <- rbind(NBN_north_biodiversity_list_vernacular, NBN_north_invasive_vernacular, NBN_north_rspb_vernacular)
NBN_north$Site <- "North"


#repeating with nbn atlas data for South Site 
NBN_rspb_south <- read.csv('Data/NBN_atlas_south/south_site_rspb.csv', header=TRUE)
#no records for biodiversity list on NBN
NBN_invasive_south <- read.csv('Data/NBN_atlas_south/south_site_invasive.csv', header=TRUE)

#creating dataframe with RSPB Priority Species List 
NBN_south_rspb_vernacular <- data.frame(NBN_rspb_south$Vernacular.Name, NBN_rspb_south$Class)
NBN_south_rspb_vernacular$Status <- "RSPB Priority Species"
colnames(NBN_south_rspb_vernacular) <- c("Species", "Class", "Status")
NBN_south_rspb_vernacular

#creating dataframe with species listed on the Scottish Biodiversity List
#NBN Atlas occurrence download at https://nbnatlas.org accessed on [21st November 2025].
#no records on nbn

#creating dataframe with species listed as Invasive
NBN_south_invasive_vernacular <- data.frame(NBN_invasive_south$Vernacular.Name, NBN_invasive_south$Class, NBN_invasive_south$Invasive)
colnames(NBN_south_invasive_vernacular) <- c("Species", "Class", "Status")
NBN_south_invasive_vernacular

NBN_south <- rbind(NBN_south_invasive_vernacular, NBN_south_rspb_vernacular)
NBN_south$Site <- "South"

#binding north and south into one NBN dataframe
NBN <- rbind(NBN_north, NBN_south)

#outputting table with combined data
gt(NBN)

#---
#SCOTTISH BIODIVERSITY LIST
#relevant taxons from Scottish Biodiversity List, filtered by 'Avoid negative impacts' = Yes
#reading in data
scottish_biodiversity_list <- read.csv('Data/biodiversity_list_avoid.csv', header=TRUE)

#subsetting into a bird species dataframe
SBL_bird <- scottish_biodiversity_list %>%
  filter(Taxon.group == "bird")

#creating bird species name list for comparison analysis in bird_analysis.R 
SBL_bird_species_list <- unique(SBL_bird$Scientific.Name)

#doing the same for plants for plant_analysis.R
SBL_plant_vascular <- scottish_biodiversity_list %>%
  filter(Main.group == "Vascular plants")

SBL_plant_nonvasc <- scottish_biodiversity_list %>%
  filter(Main.group == "Non vascular plants")

#combining non-vascular and vascular together
SBL_plants <- rbind(SBL_plant_vascular, SBL_plant_nonvasc)
#adding _ between spaces to match the format of the excel data for plants
SBL_plants$Scientific.Name <- sub(" ", "_", SBL_plants$Scientific.Name)
#creating list for comparison analysis in plant_analysis.R 
SBL_plant_species_list <- unique(SBL_plants$Scientific.Name)

#doing the same for terrestrial invertebrates 
SBL_terr <- scottish_biodiversity_list %>%
  filter(Main.group == "Terrestrial invertebrates")
#adding _ to match format of excel for terrestrial invertebrates
SBL_terr$Scientific.Name <- sub(" ", "_", SBL_terr$Scientific.Name)
#creating list for comparison analysis terrestrial_invert_analysis.R
SBL_terr_species_list <- unique(SBL_terr$Scientific.Name)
