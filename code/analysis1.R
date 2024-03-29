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
                            sheet = "big_sheet (3)")
View(meta_richness)
meta_richness$habitat<- as.factor(meta_richness$habitat)

## NMDS Full Presence----
max_richness <- read_excel("data/phonic_richness.xlsx", 
                                       sheet = "full_presence (2)")
View(max_richness)
max_richness<- subset(max_richness, select = -c(knock))

max_richness<- max_richness %>%
  column_to_rownames(var = "site") # make site the name of the rows


### Make separate for habitats
habitats <- read_excel("data/meta_richness.xlsx", 
                         sheet = "habitats")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)


### Distance matrix calculation
sound_dist<- vegdist(max_richness, method="jaccard", binary= TRUE)
sound_dist

### NMDS
richness_nmds<- metaMDS(max_richness, #the community data
                    distance = "jaccard", # Using bray-curtis distance
                    try = 300)
richness_nmds
#stress = 0.099
#distance = jaccard
plot(richness_nmds)
stressplot(richness_nmds)

### Plots
ordihull(richness_nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the polygons
         label = FALSE #removing labels from the polygons
) 

### permanova
#permanova
dist_full_richness<- vegdist(max_richness, method="jaccard")
perm_full_richness <- adonis2(dist_full_richness ~ habitat, data = habitats)

summary(perm_full_richness)
perm_full_richness
#significant p = 0.017
?pairwise.adonis()
?adonis2()
pairwise.adonis(dist_full_richness,habitats$habitat)

### SIMPER
basic_simper<- simper(max_richness,
                      distance= "jaccard",#our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(max_richness, 
                         habitats$habitat,
                        distance= "jaccard",
                         permutations = 999)
?simper()
summary(habitat_simper)
habitat_simper

### permatfull on pres/abs
x1<- permatfull(max_richness, strata = habitats$habitat)
summary(x1)
plot(x1)


## NMDS Full Relative Abundance ----
max_relative_abundance <- read_excel("data/phonic_richness.xlsx", 
                           sheet = "full_relative_abundance (2)")
View(max_relative_abundance)
max_relative_abundance<- subset(max_relative_abundance, select = -c(knock))

max_relative_abundance<- max_relative_abundance %>%
  column_to_rownames(var = "site") # make site the name of the rows


## Make separate for habitats
habitats <- read_excel("data/meta_richness.xlsx", 
                       sheet = "habitats")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)
habitats<- subset(habitats, select = -c(boats,wind, animals))
habitats<- habitats %>%
  column_to_rownames(var = "site")

### NMDS
relative_abundance_nmds<- metaMDS(dist_full_relative_abundance, #the community data
                        distance = "bray",# Using bray-curtis distance
                        try = 300)
relative_abundance_nmds
#stress = 0.0826
#distance = bray
plot(relative_abundance_nmds)
stressplot(relative_abundance_nmds)

### Plots

ordihull(relative_abundance_nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the polygons
         label = FALSE #removing labels from the polygons
) 

### permanova
install.packages("pairwise.adonis")

dist_full_relative_abundance<- vegdist(max_relative_abundance, method="bray")
perm_full_relative_abundance <- adonis2(dist_full_relative_abundance ~ habitat, data = habitats)
pairwise1<- pairwise.adonis2(dist_full_relative_abundance ~ habitat, 
                 data =habitats, permutations = 999)
perm_full_relative_abundance
#not significant p = 0.182

#significant 

### SIMPER
basic_simper<- simper(max_relative_abundance,
                      distance= "bray",#our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(max_relative_abundance, 
                        habitats$habitat,
                        distance= "bray",
                        permutations = 999)
summary(habitat_simper)



## NMDS Full Occurrence ---- 
# Load the data
library(readxl)
full_occurrence <- read_excel("data/phonic_richness.xlsx", 
                            sheet = "full_occurrence (2)")
View(full_occurrence)
full_occurrence<- subset(full_occurrence, select = -c(knock))


# make the sites the row names
full_occurrence<- full_occurrence %>%
column_to_rownames(var = "site")

# calculating distances
full_occurence_distance<- vegdist(full_occurrence, method="bray")
full_occurence_distance

# make the nmds
full_occurrence_nmds<- metaMDS(full_occurrence, #the community data
                      distance = "bray",
               k=2,# Using bray-curtis distance
                      try = 300)

full_occurrence_nmds
#stress = 0.0798
# method of distance is bray curtis



### permanova
#permanova
dist_full_occurrence<- vegdist(full_occurrence, method="bray")
perm_full_occurrence <- adonis2(full_occurrence ~ habitat, data = habitats)

perm_full_occurrence
# significant with a p value =. 0.009, f = 3.4561, df habitat =3, df resid =7, df total =9
plot(full_occurrence_nmds)

ordihull(full_occurrence_nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 


### SIMPER
basic_simper_full_occurence<- simper(full_occurrence,
                      distance= "bray",#our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(max_relative_abundance, 
                        habitats$habitat,
                        distance= "bray",
                        permutations = 999)
summary(habitat_simper)

# plotting with ggplot ----
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


## NMDS Low Relative Abundance ----
# import the dataset
low_relative_abundance <- read_excel("data/phonic_richness.xlsx", 
                            sheet = "low_relative_abundance (2)")

low_relative_abundance<- subset(low_relative_abundance, select = -c(13) ) #remove unnecessary columns
low_relative_abundance<- low_relative_abundance %>%
  column_to_rownames(var = "site")
View(low_relative_abundance)
# Make the nmds
low_abundance_nmds<- metaMDS(low_relative_abundance, #the community data
           distance = "bray",
           k=2, # Using bray-curtis distance
           try = 300)
low_abundance_nmds
# stress = 0.066
plot(low_abundance_nmds)
abundance_simper_habitat<- simper(abundances,
                                  habitats$habitat,#our community data set
                          permutations = 999) # permutations to run
summary(abundance_simper_habitat)
### permanova
dist_low_abundance<- vegdist(low_relative_abundance, method="bray")
perm_low_abundance <- adonis2(dist_low_abundance ~ habitat, data = habitats)

perm_low_abundance
#not significant , p = 0.083


### Plots
ordihull(low_abundance_nmds, # the nmds we created
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


## NMDS Low presence ----
low_presence<- read_excel("data/phonic_richness.xlsx", 
                                      sheet = "low_presence (2)")
View(low_presence)
low_presence<- subset(low_presence, select = -c(habitat) ) #remove unnecessary columnsc

low_presence<- low_presence %>%
  column_to_rownames(var = "site")
View(low_presence)

##nmds
nmds_low_presence<-  metaMDS(low_presence, #the community data
                                            distance = "jaccard",
                             autotransform = F,
                             # Using bray-curtis distance
                                            try = 300)

nmds_low_presence
# stress = 0.077
plot(nmds_low_presence)
stressplot(nmds_low_presence)
ordiplot(nmds_low_presence, type= "text")

View(habitats)
habitats<- subset(habitats, select= -c(boats,wind, animals ))

#permanova
dist_low_presence<- vegdist(low_presence, method="jaccard")
perm_low_presence <- adonis2(dist_low_presence ~ habitat, data = habitats)

summary(perm_low_presence)
perm_low_presence
# not significant, p = 0.525

#simper
basic_simper<- simper(low_presence, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(nmds_low_richness_matrix, 
                        habitats$habitat,
                        permutations = 999)
summary(habitat_simper)
#plot
plot.new()
ordihull(nmds_low_presence, # NMDS results object
         groups = habitats$habitat, # Grouping variable
         draw = "polygon", # Draw polygons
         col = 1:3, # Specify colors for polygons
         label = FALSE # Remove labels from polygons
)

## NMDS low Occurrence ----
low_occurrence<- read_excel("data/phonic_richness.xlsx", 
                                      sheet = "low_activity (2)")
View(low_occurrence)
low_occurrence<- subset(low_occurrence, select = -c(habitat))
low_occurrence<- low_occurrence %>%
  column_to_rownames(var = "site")

##nmds
nmds_low_occurrence<-  metaMDS(low_occurrence, #the community data
                             distance = "bray",
                             autotransform = F,
                             # Using bray-curtis distance
                             try = 300)

nmds_low_occurrence
#stress = 0.067

stressplot(nmds_low_abundance)
ordiplot(nmds_low_occurrence, type= "text")
?ordiplot
plot(nmds_low_occurrence)
View(habitats)

ordihull(nmds_low_occurrence, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 


## adding habitat
habitats <- read_excel("data/meta_richness.xlsx", 
                       sheet = "habitats")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)

# Try PERMANOVA
dist_low_occurrence<- vegdist(low_occurrence, method="bray")
perm_low_occurrence <- adonis2(low_occurrence ~ habitat, data = habitats)

summary(perm_low_occurrence)
perm_low_occurrence
# is significant: 0.033 = p

#simper
basic_simper<- simper(nmds_low_abundance_matrix, #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.
# none have significant p values, but highest ratio is the thump

habitat_simper<- simper(dist_low_occurrence, 
                        habitats$habitat,
                        permutations = 999)
summary(habitat_simper)
w# difference between 1-2 = knock
# difference between 1-3 = croak and knock
# difference between 2-3 = low long grunt

##Plot
ordihull(nmds_low_abundance_matrix, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 

## low occurrence nmds----
nmds_low_activity_matrix<- read_excel("data/meta_richness.xlsx", 
                                       sheet = "low_activity")
View(nmds_low_activity_matrix)
nmds_low_activity_matrix<- subset(nmds_low_activity_matrix, select = -c(simpsons,habitat, max_richness) ) #remove unnecessary columnsc


nmds_low_activity_matrix<- nmds_low_activity_matrix %>%
  column_to_rownames(var = "site")
View(nmds_low_abundance_matrix)

##nmds
nmds_low_activity<-  metaMDS(nmds_low_activity_matrix, #the community data
                              distance = "bray",
                              autotransform = T,
                              # Using bray-curtis distance
                              try = 300)

nmds_low_activity
plot(nmds_low_abundance)
stressplot(nmds_low_activity)
ordiplot(nmds_low_activity, type= "text")

ordihull(nmds_low_abundance_matrix, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 


## adding habitat
habitats <- read_excel("data/meta_richness.xlsx", 
                       sheet = "habitats")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)

# Try PERMANOVA
dist_low_activity<- vegdist(nmds_low_activity_matrix, method="bray")
perm_low_activity <- adonis2(dist_low_activity ~ habitat, data = habitats)

summary(perm_low_abundance)
perm_low_activity
#significant


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
ordihull(nmds_low_activity_matrix, # the nmds we created
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


