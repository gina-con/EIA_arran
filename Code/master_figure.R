rm(list=ls())
library(dplyr)
library(ggplot2)
library(cowplot)

#master comparison figure of species richness
#loading in data
bats_combined <- read.csv("Data/for_combined_figure/bats_combined.csv")
terrinv_combined <- read.csv("Data/for_combined_figure/terrinv_combined.csv")
birds_combined <- read.csv("Data/for_combined_figure/birds_combined.csv")
mammals_combined <- read.csv("Data/for_combined_figure/mammals_combined.csv")
plants_combined <- read.csv("Data/for_combined_figure/plants_combined.csv")

#combining all taxa datasets in order to create combined species richness figure
masterdf <- rbind(terrinv_combined, birds_combined, mammals_combined, bats_combined, plants_combined)
library(ggplot2)

#creating bar chart showing species richness of each taxa on each site
ggplot(masterdf, aes(x=Taxa, fill=Site))+
  geom_bar(position="dodge")+
  theme_classic()+ 
  labs(x="Taxa surveyed", y="Species Richness", fill="Site")
  

#combined venn diagrams to make one venn diagram figure containing venns for each taxa and combined taxa venn

#creating dataframes containing all unique species on each site
unique_master_south <- masterdf%>% 
  filter(Site == "South")
unique_master_south_list <- unique_master_south$Species
unique_master_north <- masterdf%>%
  filter(Site == "North")
unique_master_north_list <- unique_master_north$Species

#creating a venn diagram demonstrating the total number of unique and shared species on each site
vennlist_masterdf <- list('South'=c(unique_master_south_list), 'North'=c(unique_master_north_list))
total_venn <- ggVennDiagram(vennlist_masterdf, label_alpha = 0, label="count")+
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF")+
  theme(legend.position = "none")+ 
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  ggtitle("Number of Unique Species Identified")+
  coord_flip()

#creating one big figure with all of the venn diagrams in, using venn diagrams
#saved to a vector from each taxon analysis code.
cowplot::plot_grid(total_venn, birds_venn, bats_venn, mammals_venn, terr_venn, plant_venn, nrow=2, 
                   rel_widths=c(2,2,2,2), labels=c("A", "B", "C", "D", "E", "F"))

