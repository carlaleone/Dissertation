# Dissertation NMDS Analysis
# Carla Leone
# 5th March, 2024

### Load the dataset and packages ----
install.packages("tidyverse")
install.packages("vegan")
library(tidyverse)
library(vegan)
library(readxl)

meta_richness <- read_excel("data/meta_richness.xlsx", 
                            sheet = "big_sheet (3)")
View(meta_richness)
meta_richness$habitat<- as.factor(meta_richness$habitat)

### NMDS broadband Presence/Absence----
max_richness <- read_excel("data/phonic_richness.xlsx", 
                                       sheet = "full_presence (2)")
View(max_richness)
max_richness<- subset(max_richness, select = -c(knock))
## Make separate for habitats
habitats <- read_excel("data/meta_richness.xlsx", 
                       sheet = "habitats")
habitats$habitat<- as.factor(habitats$habitat) # make habitat a factor
habitats<- subset(habitats, select= c(site, habitat)) #reduce the habitat data frame 
#creating max_richness 2 with habitat in the data frame to make plotting easier
max_richness_2<- merge(habitats, max_richness, by = c("site"))
max_richness_2<- max_richness_2 %>%
  column_to_rownames(var = "site")
max_richness_2$habitat<- as.factor(max_richness_2$habitat)
#once max_richness two has been created, can make site the row names in max richness 1
max_richness<- max_richness %>%
  column_to_rownames(var = "site") # make site the name of the rows

### Distance matrix calculation
max_richness_dist<- vegdist(max_richness, method="bray", binary =T)
max_richness_dist
perm_max_richness<- adonis2(max_richness_dist ~ habitat, data = habitats)
perm_max_richness

nmds_richness<- metaMDS(max_richness_dist, #the community data
                        try = 300)
nmds_richness
#distance = binary bray

### NMDS Broadband Presence/Absence plot ----

richness_nmds<- metaMDS(max_richness_2[,-c(habitat)], #the community data
                    distance = "bray",# Using bray-curtis distance
                    k =2 , # two dimensions
                    try = 300) #300 tries
richness_nmds
goodness(richness_nmds)
scores(richness_nmds)
View(max_richness_2)
nrow(max_richness_2$habitat)
#stress = 0.099
#distance = jaccard
plot(richness_nmds)
stressplot(richness_nmds)

group = as.character(max_richness_2$habitat)
unique(max_richness_2$habitat)
colors = group
colors[colors=="1"] <- "Green"
colors[colors==2] <- "Purple"
colors[colors==3] <- "Orange"

ordiplot(richness_nmds, type = "n", cex.axis = 1.5, cex.lab=1.5)

for(i in unique(group)) {
 ordihull(richness_nmds$point[grep(i, group),], draw="polygon",
       groups = group[group == i],col = colors[grep(i,group)],label=F) } 
 
   orditorp(richness_nmds, display = "species", label=F, col = "Grey50", air = 0.01)
  
   fit <- adonis2(max_richness_2[, -c(1)] ~ habitat, max_richness_2, 
                 permutations = 999, method = "jaccard")
  fit
  
##needs to a be highlighted and run together
  
## Now to try in ggplot
  # ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
  Colours <- c( "#56B4E9", "#009E73", "#F0E442")
  names(Colours) <- c("1", "2", "3")
  Colours_darker <- darken(Colours, 0.4)
  names(Colours_darker) <- c("1", "2", "3")
  
  data.scores <- as.data.frame(scores(richness_nmds)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
  data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
  data.scores$grp <- max_richness_2$habitat  #  add the grp variable created earlier
  View(data.scores)
  
  species.scores <- as.data.frame(scores(richness_nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
  species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
  head(species.scores) 
  # insect_cols_mansort$Corrected_name
  
  hull.data <- data.frame()
  for(i in 1:length(unique(max_richness_2$habitat))){
    temp <- data.scores[data.scores$grp == unique(max_richness_2$habitat)[i], ][chull(data.scores[data.scores$grp == 
                                                                                                          unique(max_richness_2$habitat)[i], c("NMDS1", "NMDS2")]), ]
    hull.data <- rbind(hull.data, temp)
  }
  
  (max_richness_nmds_plot <- ggplot() +
      geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30) +# add the convex hulls
      labs(fill = "Habitat Category") +
      geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size= 2, alpha=0.5) +  # add the species labels
      geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=grp),size=3) + # add the point markers
      #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=3,vjust=0) +  # add the site labels
      scale_colour_manual(values=Colours) +
      scale_fill_manual(values=Colours) +
      # scale_x_continuous(limits = c(-1.4, 3), breaks = c(-1,0,1,2,3)) +
      # scale_y_continuous(limits = c(-1.4, 1.2), breaks = c(-1,-0.5,0, 0.5 ,1)) +
      scale_x_continuous(limits = c(-0.7, 1.6), breaks = c(-0.5,0,0.5,1,1.5)) +
      scale_y_continuous(limits = c(-0.7, 0.5), breaks = c(-0.5,-0.25,0,0.25,0.5)) +
      #coord_equal() +
      theme_classic() +
      ggtitle("Broadband Presence/Absence") +
      theme(plot.title = element_text(hjust = 0.5))+
      guides(colour = FALSE))
  
  

###Table of results
# Extract NMDS coordinates
nmds_coordinates <- scores(richness_nmds)
nrow(nmds_coordinates)

# Convert coordinates to a data frame
nmds_df <- as.data.frame(nmds_coordinates)


# Add sample names (row names) to the data frame
nmds_df$site <- rownames(nmds_df)

# Optionally, you can rename the columns for clarity
colnames(nmds_df) <- c("NMDS1", "NMDS2", "Sample")

# Print or manipulate the resulting data frame as needed
print(nmds_df)

### Plots
ordihull(richness_nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the polygons
         label = FALSE #removing labels from the polygons
) 

### permanova
#permanova
dist_full_richness<- vegdist(max_richness, method="jaccard", binary = TRUE)
perm_full_richness <- adonis2(dist_full_richness ~ habitat, data = habitats)

summary(perm_full_richness)
perm_full_richness
#significant p = 0.017

# dispersion
?betadisper()
permdisp_max_richness<- betadisper(dist_full_richness, habitat)
plot(permdisp_max_richness)
permdisp_max_richness
?permutest.betadisper
anova(permdisp_max_richness)
pmod <- permutest(permdisp_max_richness, permutations = 99, pairwise = TRUE)
pmod
# p value is 0.97 which indicates that the dispersion is not significantly different between the groups. 

eNMDS### SIMPER
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


#----
#----
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



#----
#----
## NMDS Full Occurrence ---- 
# Load the data
library(readxl)
full_abundance <- read_excel("data/phonic_richness.xlsx", 
                            sheet = "full_occurrence (2)")
View(full_abundance)
full_abundance<- subset(full_abundance, select = -c(knock))


# make the sites the row names
full_abundance<- full_abundance %>%
column_to_rownames(var = "site")
str(full_abundance)

full_abundance<- full_abundance[, 1:16] <- full_abundance[, 1:16] * 13
View(full_abundance)

# calculating distances and permanova
full_abundance_distance<- vegdist(full_abundance, method="bray")
perm_full_abundance<- adonis2(full_abundance_distance ~ habitat, data = habitats)
perm_full_abundance

# make the nmds
full_abundance_nmds<- metaMDS(full_abundance_distance, #the community data
                      distance = "bray",
               k=2,# Using bray-curtis distance
                      try = 300)

full_abundance_nmds
#stress = 0.079
# method of distance is bray curtis dissimilarity

plot(full_abundance_nmds)
ordihull(full_occurrence_nmds, # the nmds we created
         groups= habitats$habitat, #calling the groups from the mpa data frame we made
         draw = "polygon", # drawing polygons
         col = 1:3, # shading the plygons
         label = FALSE #removing labels from the plygons
) 


### SIMPER = contribution to dissimilarities ...between groups
basic_simper_full_abundance<- simper(full_abundance,
                      distance= "bray",#our community data set
                      permutations = 999) # permutations to run

summary(basic_simper_full_abundance, ordered = T) #summary is the total contrast.
# Extract dissimilarity values and contributions
dissimilarity <- basic_simper_full_abundance$diss
contributions <- basic_simper_full_abundance$contrib

# Combine dissimilarity and contributions into a data frame
simper_output <- data.frame(Dissimilarity = dissimilarity, Contributions = contributions)
View(simper_output)
basic_simper_full_abundance
write.table(basic_simper_full_abundance)
write.table(basic_simper_full_abundance, file = "simper_output_table.txt", sep = "\t", quote = FALSE)
?simper
habitat_simper_full_abundance<- simper(full_abundance, 
                        habitats$habitat,
                        distance= "bray",
                        permutations = 999)
summary(habitat_simper_full_abundance)

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
str(low_occurrence)
low_abundance<- low_occurrence[, 1:12] <- low_occurrence[, 1:12] * 13

##nmds
nmds_low_occurrence<-  metaMDS(low_abundance, #the community data
                             distance = "bray",
                             autotransform = T,
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

nmds_low_occurrence$points

# Calculate convex hulls
chulls <- as.data.frame(tapply(1:nrow(nmds_low_occurrence$points), habitats$habitat, function(i) {
  hull_pts <- chull(nmds_low_occurrence$points[i, 1], nmds_low_occurrence$points[i, 2])
  cbind(nmds_low_occurrence$points[i[hull_pts], ], Group = unique(habitats$habitat[i]))
}))

# Plot NMDS points and convex hulls
ggplot() +
geom_point (data = nmds_low_occurrence$points, aes(x = MDS1, y = MDS2)) +
  geom_polygon(data = chulls, aes(group = habitat), fill = NA, color = "black") +
  labs(title = "NMDS Plot with Convex Hulls", x = "MDS1", y = "MDS2")

## adding habitat
habitats <- read_excel("data/meta_richness.xlsx", 
                       sheet = "habitats")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)

# Try PERMANOVA
dist_low_occurrence<- vegdist(low_occurrence, method="bray")
perm_low_occurrence <- adonis2(low_abundance ~ habitat, data = habitats)

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

## Clustering presence ----
dist_low_presence<- vegdist(low_presence, method="jaccard")
cluster <- hclust(dist_low_presence, method = "average", members = NULL)
plot(cluster)

?hclust
## Clustering Indices? ----
str(merged_habitats)
str(merged_cluster)
merged_cluster<- merged_avg %>%
  select(-c(1,2))

dist_indices<- vegdist(merged_cluster, method= "bray")
cluster_indices<- hclust(dist_indices, method= "averag", member =NULL)
plot(cluster_indices)
## Richness vs diversity ----
str(meta_richness)
linear<- meta_richness%>%
  group_by(site, habitat)%>%
  summarise(richness = mean(max_richnes),
            simpson = mean(simpson))%>%
  ungroup()
View(linear)
linear$habitat<- as.factor(linear$habitat)

ggplot(linear, aes(x = richness, y = simpson, color = habitat)) +
  geom_point() + # Add points
  geom_text(aes(label = site), vjust = -0.5) +
  geom_smooth(aes(y=predictions), method = "lm", se=F, colour = "black") +
  labs(x = "Site Maximum Number of Signal Types", y = "Simpson's Diversity (D)", color = "Habitat Complexity") +
  theme_classic()

predictions<- predict(lm_linear)
lm_linear<- lm(simpson~richness, data=linear)
plot(lm_linear)
#doesnt meet the assumptions
summary(lm_linear)
correlation<- cor(linear$richness, linear$simpson, method = "spearman")
print(correlation)
#spearmans correlation is 0.8211, pretty good
## Low Richness vs Low Diversity ----
str(merged)
linear_low<- subset(merged, select = c(1,66, 67,57))
linear_low<- linear_low %>%
  group_by(site, habitat)%>%
  summarise(richness = mean(low_max_richness),
            simpson = mean(low_simpson))%>%
  ungroup()
linear_low$habitat<- as.factor(linear_low$habitat)
View(linear_low)
lm_linear_low<- lm(simpson~richness, data=linear_low)
summary(lm_linear_low)
plot(lm_linear_low)
# doesnt fit assumptions, lots of leverage and unqual variance
correlation_low<- cor(linear_low$richness, linear_low$simpson, method = "spearman")
print(correlation_low)
#0.82
predictions_low <- predict(lm_linear_low)
#plot
ggplot(linear_low, aes(x = richness, y = simpson, color = habitat)) +
  geom_point() + # Add points
  geom_text(aes(label = site), vjust = -0.5) +
  geom_smooth(aes(y=predictions_low), method = "lm", se=F, colour = "black") +
  labs(x = "Site Maximum Number of Signal Types", y = "Simpson's Diversity (D)", color = "Habitat Complexity") +
  theme_classic()

## Plotting relative abundance ----
library(ggplot2)
# first make correct dataset. Need column for sound, relative abundance, site, and habitat
low_relative_abundance <- read_excel("data/phonic_richness.xlsx", 
                                     sheet = "low_relative_abundance (2)")
low_relative_abundance<- subset(low_relative_abundance, select = -c(...14, ...15, ...16, ...17))

# Make the data in long format
long_data <- gather(low_relative_abundance, key = "Species", value = "RelativeAbundance", -c(site, habitat))

summary_data <- long_data %>%
  group_by(Species, site, habitat) %>%
  summarise(mean_abundance = mean(RelativeAbundance),
            se_abundance = sd(RelativeAbundance) / sqrt(n())) %>%
  ungroup()

# Plot
# trying proportional fill
## colours
custom_palette <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
                    "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#8DD3C7")

(long_data_plot<- ggplot(summary_data, aes(x = site, y = mean_abundance, fill = Species)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette) + 
  labs(title = "Relative Abundance by Site",
       x = "Site",
       y = "Relative Abundance",
       fill = "Sound Category") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~habitat, scales = "free")
)