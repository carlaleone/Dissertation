# Dissertation Richness Script
# Carla Leone
# 5th March, 2024


## Load the dataset and packages ----
install.packages("tidyverse")
install.packages("vegan")
library(tidyverse)
library(vegan)
library(readxl)

meta_richness <- read_excel("data/meta_richness.xlsx", 
                            sheet = "big_sheet (2)")
View(meta_richness)

## Add column for habitat ---
richness <- meta_richness %>%
  mutate(habitat = case_when(
    site %in% c("port_dinallaen", "ardmore", "gallanach_bay") ~ "1",
    site %in% c("craignish", "skye", "kintyre") ~ "2",
    site %in% c("gansey_bay", "kyles_of_bute", "isle_of_soay", "canna") ~ "3",
    TRUE ~ NA_character_  # In case there are unmatched sites
  )) #match sites to habitat

View(richness)
richness$habitat<- as.factor(richness$habitat)

## Data frame with richness per site
richness <- richness %>%
  mutate(max_richness = case_when(
    site %in% c("port_dinallaen") ~ "12",
    site %in% c("gallanach_bay") ~ "9",
    site %in% c("ardmore") ~ "15",
    site %in% c("craignish") ~ "12",
    site %in% c("skye") ~ "13",
    site %in% c("kintyre") ~ "9",
    site %in% c("gansey_bay") ~ "8",
    site %in% c("kyles_of_bute") ~ "12",
    site %in% c("isle_of_soay") ~ "13",
    site %in% c("canna") ~ "12",
    TRUE ~ NA_character_  # In case there are unmatched sites
  )) #match sites to total richness
richness$max_richness<- as.numeric(richness$max_richness)

## Richness vs Habitat ----
boxplot(richness$max_richness~richness$habitat)
boxplot(richness$richness~richness$site)



### Assumptions for ANOVA
plot


## NMDS Total Richness----
sound_nmds <- read_excel("data/meta_richness.xlsx", 
                            sheet = "Sheet2")
View(sound_nmds)
### Delete unnecessary columns
presence_nmds<- subset(sound_nmds, select = -c(boat, water, avg_richness,max_richnes, sample_richness,high,low, high_low, samples) ) #remove unnecessary columns
presence_nmds[, -1] <- sapply(presence_nmds[, -1], function(x) as.numeric(as.character(x))) #make all values numeric
class(presence_nmds$grunt)

presence_nmds<- presence_nmds %>%
  column_to_rownames(var = "site") # make site the name of the rows

View(presence_nmds)

### Make separate for habitats
habitats <- read_excel("data/meta_richness.xlsx", 
                         sheet = "nmds_categories")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)

### Distance matrix calculation
sound_dist<- vegdist(presence_nmds, method="bray", binary= TRUE)
sound_dist

### NMDS
fishes_nmds<- metaMDS(presence_nmds, #the community data
                    distance = "bray", # Using bray-curtis distance
                    try = 100)
### Plots
ordihull(fishes_nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 

### SIMPER
habitat_simper<- simper(presence_nmds, 
                         habitats$habitat,
                         permutations = 999)
summary(habitat_simper)
