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
meta_richness$habitat<- as.factor(meta_richness$habitat)

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
activity<- activity %>%
column_to_rownames(var = "site")

activity<- activity %>% 
select(-water, -boat, -cetacean)
View(activity)

# calculating distances
distances<- vegdist(activity, method="bray")
distances

# make the nmds
activity_nmds<- metaMDS(activity, #the community data
                      distance = "bray",
               k=2,# Using bray-curtis distance
                      try = 300)

habitats <- read_excel("data/meta_richness.xlsx", 
                      sheet = "nmds_categories")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)
habitats<- c(1,3,3,1,1,3,3,2,2,2)

# plotting with ggplot
data.scores <- as.data.frame(scores(activity_nmds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$habitat <- habitats  #  add the grp variable created earlier
head(data.scores) 

species.scores <- as.data.frame(scores(activity_nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores) 

grp.1 <- data.scores[data.scores$habitat == "1", ][chull(data.scores[data.scores$habitat == 
                                                                   "1", c("sites.NMDS1", "sites.NMDS2")]), ]  # hull values for grp A

grp.2 <- data.scores[data.scores$habitat == "2", ][chull(data.scores[data.scores$habitat == 
                                                                   "2", c("sites.NMDS1", "sites.NMDS2")]), ]  # hull values for grp B

grp.3 <- data.scores[data.scores$habitat == "3", ][chull(data.scores[data.scores$habitat == 
                                                                       "3", c("sites.NMDS1", "sites.NMDS2")]), ]  # hull values for grp B

hull.data <- rbind(grp.1, grp.2, grp.3)  #combine grp.a and grp.b
hull.data

library(tidyverse)
Colours <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
names(Colours) <- c("BW", "LI", "SOSR", "WB", "WOSR", "WW")
Colours_darker <- darken(Colours, 0.4)
names(Colours_darker) <- c("BW", "LI", "SOSR", "WB", "WOSR", "WW")

(  activity_NMDS_plot <- ggplot() +
    geom_polygon(data=hull.data,aes(x=sites.NMDS1,y=sites.NMDS2,fill=habitat,group=habitat),alpha=0.30) + # add the convex hulls
    #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
    geom_point(data=data.scores,aes(x=sites.NMDS1,y=sites.NMDS2,colour=habitat),size=3) + # add the point markers
    #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
    scale_colour_manual(values=Colours) +
    scale_fill_manual(values=Colours) +
    # scale_x_continuous(limits = c(-1.4, 3), breaks = c(-1,0,1,2,3)) +
    # scale_y_continuous(limits = c(-1.4, 1.2), breaks = c(-1,-0.5,0, 0.5 ,1)) +
    scale_x_continuous(limits = c(-0.7, 1.6), breaks = c(-0.5,0,0.5,1,1.5)) +
    scale_y_continuous(limits = c(-0.7, 0.5), breaks = c(-0.5,-0.25,0,0.25,0.5)) +
    #coord_equal() 
    ggtitle("Invertebrates") +
    theme(plot.title = element_text(hjust = 0.5)))

# SIMPER 
basic_simper_activity<- simper(activity_nmds, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(activity, 
                        habitats$habitat,
                        permutations = 999)
summary(habitat_simper, ordered = T)
stressplot(nmds)
ordiplot(nmds, type= "text")


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


## NMDS low richness ----
nmds_low_activity_matrix<- read_excel("data/meta_richness.xlsx", 
                                      sheet = "low_presence")
nmds_low_richness_matrix<- subset(nmds_low_richness_matrix, select = -c(simpsons,habitat, max_richness) ) #remove unnecessary columnsc

nmds_low_richness_matrix<- nmds_low_richness_matrix %>%
  column_to_rownames(var = "site")
View(nmds_low_richness_matrix)

##nmds
nmds_low_richness<-  metaMDS(nmds_low_richness_matrix, #the community data
                                            distance = "bray",
                             autotransform = F,
                             # Using bray-curtis distance
                                            try = 300)

nmds_low_richness
plot(nmds_low_richness)
stressplot(nmds_low_richness)
ordiplot(nmds_low_richness, type= "text")


## adding habitat
habitats <- read_excel("data/meta_richness.xlsx", 
                       sheet = "habitats")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)

#simper
basic_simper<- simper(nmds_low_richness_matrix, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(nmds_low_richness_matrix, 
                        habitats$habitat,
                        permutations = 999)
summary(habitat_simper)

## NMDS low abundance ----
nmds_low_abundance_matrix<- read_excel("data/meta_richness.xlsx", 
                                      sheet = "low_relative_abundance")
View(nmds_low_abundance_matrix)

nmds_low_abundance_matrix<- nmds_low_abundance_matrix %>%
  column_to_rownames(var = "site")
View(nmds_low_abundance_matrix)

##nmds
nmds_low_abundance<-  metaMDS(nmds_low_abundance_matrix, #the community data
                             distance = "bray",
                             autotransform = F,
                             # Using bray-curtis distance
                             try = 300)

nmds_low_abundance
plot(nmds_low_abundance)
stressplot(nmds_low_abundance)
ordiplot(nmds_low_abundance, type= "text")


## adding habitat
habitats <- read_excel("data/meta_richness.xlsx", 
                       sheet = "habitats")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)

#simper
basic_simper<- simper(nmds_low_abundance_matrix, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.
# none have significant p values, but highest ratio is the thump

habitat_simper<- simper(nmds_low_abundance_matrix, 
                        habitats$habitat,
                        permutations = 999)
summary(habitat_simper)
# difference between 1-2 = knock
# difference between 1-3 = croak and knock
# difference between 2-3 = low long grunt

##Plot
ordihull(nmds_low_abundance_matrix, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 
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


