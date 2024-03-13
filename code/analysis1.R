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
max_richness <- read_excel("data/meta_richness.xlsx", 
                                       sheet = "RICHNESS")
View(max_richness)

### Delete unnecessary columns
max_richness<- subset(max_richness, select = -c(cetacean,boat, water, avg_richness,max_richnes, sample_richness,high,low, high_low, samples) ) #remove unnecessary columnsc
class(max_richness)
str(max_richness)

max_richness<- max_richness %>%
  column_to_rownames(var = "site") # make site the name of the rows


### Make separate for habitats
habitats <- read_excel("data/meta_richness.xlsx", 
                         sheet = "nmds_categories")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)

### Distance matrix calculation
sound_dist<- vegdist(presence_nmds, method="bray", binary= TRUE)
sound_dist

### NMDS
richness_nmds<- metaMDS(max_richness, #the community data
                    distance = "bray", # Using bray-curtis distance
                    try = 100)

### Plots
ordihull(max_richness, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 

### SIMPER
basic_simper<- simper(max_richness, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(max_richness, 
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
                      distance = "bray",
               k=2,# Using bray-curtis distance
                      try = 300)


# plot the nmds
ordihull(nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = F #removing labels from the plygons
)
plot
# SIMPER 
basic_simper_activity<- simper(activity_nmds, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(activity_nmds, 
                        habitats$habitat,
                        permutations = 999)
summary(habitat_simper, ordered = T)
stressplot(nmds)
ordiplot(nmds, type= "text")

data.scores <- as.data.frame(scores(nmds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- grp  #  add the grp variable created earlier
head(data.scores) 

## NMDS Relative Abundance ----
# import the dataset
abundances <- read_excel("data/meta_richness.xlsx", 
                            sheet = "relative_abundance")
abundances<- subset(abundances, select = -c(cetacean,boat, water, avg_richness,max_richnes, sample_richness,high,low, high_low, samples) ) #remove unnecessary columns
View(abundances)
str(abundances)
abundances$site<- as.character(abundances$site)
abundances<- abundances %>%
  column_to_rownames(var = "site")
class(abundances)
is.data.frame(abundances) #data frame
abundances <- abundances[complete.cases(abundances), ]
is.na.data.frame(abundances)
abundances <- round(abundances, 3)
# Make the nmds
abundance_nmds<- metaMDS(abundances, #the community data
           distance = "bray",k=2, # Using bray-curtis distance
           try = 300)
abundance_nmds

abundance_simper_habitat<- simper(abundances,
                                  habitats$habitat,#our community data set
                          permutations = 999) # permutations to run
summary(abundance_simper_habitat)
## keep getting error saying that it is not numeric

### Plots
plot<- ordihull(abundance_nmds, # the nmds we created
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


