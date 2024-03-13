# Dissertation NMDS Analysis
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

## Add column for habitat ----
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



## NMDS Total Richness----
sound_nmds <- read_excel("data/meta_richness.xlsx", 
                                       sheet = "RICHNESS")
View(sound_nmds)
### Delete unnecessary columns
presence_nmds<- subset(sound_nmds, select = -c(cetacean,boat, water, avg_richness,max_richnes, sample_richness,high,low, high_low, samples) ) #remove unnecessary columns

 #make all values numeric
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
summary(fishes_nmds)
help(metaMDS)
### Plots
ordihull(fishes_nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 

### SIMPER
basic_simper<- simper(presence_nmds, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(presence_nmds, 
                         habitats$habitat,
                         permutations = 999)
summary(habitat_simper)

## NMDS Activity ---- 
# Load the data
library(readxl)
activity <- read_excel("data/meta_richness.xlsx", 
                            sheet = "activity")
activity <- activity %>%
  mutate(habitat = case_when(
    site %in% c("port_dinallaen", "ardmore", "gallanach_bay") ~ "1",
    site %in% c("craignish", "skye", "kintyre") ~ "2",
    site %in% c("gansey_bay", "kyles_of_bute", "isle_of_soay", "canna") ~ "3",
    TRUE ~ NA_character_  # In case there are unmatched sites
  ))
View(activity)

# make the sites the row names
activity_nmds<- activity %>%
column_to_rownames(var = "site")  %>%
  select(-water, -boat, -cetacean)
activity_nmds <- sapply(activity_nmds, as.numeric)
str(activity_nmds)
View(activity_nmds)



# calculating distances
distances<- vegdist(numeric_data, method="bray")
distances

# make the nmds
nmds<- metaMDS(activity_nmds, #the community data
                      distance = "bray", # Using bray-curtis distance
                      try = 100)
activity_fish_nmds

# plot the nmds
ordihull(activity_fish_nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = F #removing labels from the plygons
)

# SIMPER 
basic_simper_activity<- simper(nmds, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(presence_nmds, 
                        habitats$habitat,
                        permutations = 999)
summary(habitat_simper)

## NMDS Relative Abundance ----
# import the dataset
abundances <- read_excel("data/meta_richness.xlsx", 
                            sheet = "relative_abundance")
abundances<- subset(abundances, select = -c(boat, water, avg_richness,max_richnes, sample_richness,high,low, high_low, samples) ) #remove unnecessary columns
abundances<- abundances %>%
  column_to_rownames(var = "site")
abundances <- sapply(abundances, as.numeric)
View(abundances)

# Make the nmds
abundance_nmds<- metaMDS(abundances, #the community data
           distance = "bray",autotransform =FALSE, # Using bray-curtis distance
           try = 100)
any(!is.numeric(abundances))
any(!is.infinite(abundances))
infinite_values <- is.infinite(abundances)
infinite_values
abundances[infinite_values] <- 0 

### Plots
ordihull(abundance_nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 

### SIMPER
basic_simper<- simper(abundance_nmds, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(presence_nmds, 
                        habitats$habitat,
                        permutations = 999)
summary(habitat_simper)


## Richness vs time ----
class(meta_richness$time)
View(meta_richness)
View(richness)
richness$richness<- as.numeric(richness$richness)

#Compute average richness for each time point and habitat category
df_avg <- richness %>%
  group_by(time,habitat) %>%
  summarise(avg_richness = mean(max_richnes),
            sd_richness = sd(max_richnes),
            se_richness = sd(max_richnes) /sqrt(n()))%>%
              ungroup()
#gives the stats for each habitat at each time point. Has taken the average richness of each habitat for each time.

### Group by site
df_avg_site<- richness %>%
  group_by(time,site) %>%
  summarise(avg_richness = mean(richness),
            sd_richness = sd(richness),
            se_richness = sd(richness) /sqrt(n()))%>%
  ungroup()

View(df_avg)

### Plot by habitat
ggplot(df_avg, aes(x = time, y = avg_richness, color = habitat)) +
  geom_line() +  # Add lines
  geom_point() +  # Add points
  labs(x = "Time", y = "Average Richness", color = "Habitat Category") +  # Labels
  theme_minimal()  # Optional: change theme if desired

### plot by site
ggplot(df_avg_site, aes(x = time, y = avg_richness, color = site)) +
  geom_line() +  # Add lines
  geom_point() +  # Add points
  labs(x = "Time", y = "Average Richness", color = "Habitat Category") +  # Labels
  theme_minimal()  # Optional: change theme if desired


### Using the full dataset richness to plot and adding error bars
ggplot(richness, aes(x = time, y = richness, color = habitat)) +
  stat_summary(fun.data = mean_se, geom = "line") +  # Add lines with mean and standard error
  stat_summary(fun.data = mean_se, geom = "point") +  # Add points with mean and standard error
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +  # Add error bars with standard error
  labs(x = "Time", y = "Richness", color = "Habitat Category") +  # Labels
  theme_minimal()

## Boat Presence ----


