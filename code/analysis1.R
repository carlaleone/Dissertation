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
View(max_richness)
str(max_richness_2)
#once max_richness two has been created, can make site the row names in max richness 1
max_richness_2 <- max_richness_2 %>%
  mutate(site = recode(site, "ardmore" = "Ardmore", 
                       "port_dinallaen"="Port Dinllaen", 
                       "canna" = "Canna", 
                       "isle_of_soay"="Isle of Soay", 
                       "kyles_of_bute"= "Kyles of Bute",
                       "gallanach_bay"= "Gallanach Bay", 
                       "gansey_bay"= "Gansey Bay", 
                       "kintyre"= "Kintyre",
                       "skye"= "Skye", 
                       "craignish"="Loch Craignish"))

max_richness_2<- max_richness_2 %>%
  column_to_rownames(var = "site") # make site the name of the rows

### Distance matrix calculation
max_richness_dist<- vegdist(max_richness_2[,-c(habitat)], method="bray", binary =T)
max_richness_dist
perm_max_richness<- adonis2(max_richness_dist ~ habitat, data = max_richness_2)
perm_max_richness


#distance = binary bray

# ----
### NMDS Broadband Presence/Absence plot ----

richness_nmds<- metaMDS(max_richness_2[,-c(habitat)], #the community data
                    distance = "bray",
                    binary = T,
                    k =2 , 
                    try = 300) 
richness_nmds
goodness(richness_nmds)
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
                 permutations = 999, method = "bray")
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
     # geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size= 2, alpha=0.5) +  # add the species labels
      geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=grp),size=3) + # add the point markers
      geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label = site),size=5,vjust=0) +  # add the site labels
      scale_colour_manual(values=Colours) +
      scale_fill_manual(values=Colours) +
      # scale_x_continuous(limits = c(-1.4, 3), breaks = c(-1,0,1,2,3)) +
      # scale_y_continuous(limits = c(-1.4, 1.2), breaks = c(-1,-0.5,0, 0.5 ,1)) +
      scale_x_continuous(limits = c(-0.5, 0.5), breaks = c(-0.5,0,0.5)) +
      scale_y_continuous(limits = c(-0.3, 0.5), breaks = c(-0.25,0,0.25,0.5)) +
      #coord_equal() +
      theme_classic() +
      #ggtitle("Broadband Presence/Absence") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.line = element_line(size = 1.5),  # increase axis line thickness
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 16),  # increase legend text size
            legend.title = element_text(size = 18),
            legend.background = element_rect(color = "grey", size = 0.5))+
           # legend.position = "top")+
      guides(colour = FALSE)
    ) 
  
  

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


#----
#----
### NMDS Full Relative Abundance ----
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



# ----
# ----
# ----
## NMDS Broadband Abundance ---- 
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
full_abundance<- full_abundance %>%
  column_to_rownames(var = "site")
site<- rownames(full_abundance)

full_abundance$site<- site
full_abundance<- full_abundance[, 1:16] <- full_abundance[, 1:16] * 13
print(max_abundance_2)

# calculating distances and permanova
full_abundance_distance<- vegdist(max_abundance_2[,-c(habitat)], method="bray")
full_abundance_distance
perm_full_abundance<- adonis2(full_abundance_distance ~ habitat, data = max_abundance_2)
perm_full_abundance

# make the nmds
full_abundance_nmds<- metaMDS(max_abundance_2[,-c(habitat)], #the community data
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
basic_simper_full_abundance<- simper(max_abundance_2[,-c(habitat)],
                      distance= "bray",#our community data set
                      permutations = 999) # permutations to run

summary(basic_simper_full_abundance, ordered = T) #summary is the total contrast.


# ----
## NMDS Broadband Abundance Plot ----
# the nmds
full_abundance_nmds<- metaMDS(max_abundance_2[,-c(habitat)], #the community data
                              distance = "bray",
                              k=2,# Using bray-curtis distance
                              try = 300)
full_abundance_nmds
stressplot(full_abundance_nmds)

max_abundance_2<- merge(habitats, full_abundance, by = c("site"))
max_abundance_2$habitat<- as.factor(max_abundance_2$habitat)
print(max_abundance_2)
max_abundance_2 <- max_abundance_2 %>%
  mutate(site = recode(site, "ardmore" = "Ardmore", 
                       "port_dinallaen"="Port Dinllaen", 
                       "canna" = "Canna", 
                       "isle_of_soay"="Isle of Soay", 
                       "kyles_of_bute"= "Kyles of Bute",
                       "gallanach_bay"= "Gallanach Bay", 
                       "gansey_bay"= "Gansey Bay", 
                       "kintyre"= "Kintyre",
                       "skye"= "Skye", 
                       "craignish"="Loch Craignish")) # make site names presentable

max_abundance_2<- max_abundance_2 %>%
  column_to_rownames(var = "site")


group = as.character(max_abundance_2$habitat)
unique(max_abundance_2$habitat)
colors = group
colors[colors=="1"] <- "Green"
colors[colors==2] <- "Purple"
colors[colors==3] <- "Orange"

ordiplot(full_abundance_nmds, type = "n", cex.axis = 1.5, cex.lab=1.5)

for(i in unique(group)) {
  ordihull(full_abundance_nmds$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(full_abundance_nmds, display = "species", label=F, col = "Grey50", air = 0.01)

fit <- adonis2(max_abundance_2[, -c(1)] ~ habitat, max_abundance_2, 
               permutations = 999, method = "bray")
fit

##needs to a be highlighted and run together

## Now to try in ggplot
# ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
Colours <- c( "#56B4E9", "#009E73", "#F0E442")
names(Colours) <- c("1", "2", "3")
Colours_darker <- darken(Colours, 0.4)
names(Colours_darker) <- c("1", "2", "3")

data.scores <- as.data.frame(scores(full_abundance_nmds)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- max_abundance_2$habitat  #  add the grp variable created earlier
View(data.scores)

species.scores <- as.data.frame(scores(full_abundance_nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores) 
# insect_cols_mansort$Corrected_name

hull.data <- data.frame()
for(i in 1:length(unique(max_abundance_2$habitat))){
  temp <- data.scores[data.scores$grp == unique(max_abundance_2$habitat)[i], ][chull(data.scores[data.scores$grp == 
                                                                                                  unique(max_abundance_2$habitat)[i], c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(max_abundance_nmds_plot <- ggplot() +
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30) +# add the convex hulls
    labs(fill = "Habitat Category") +
   # geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size= 2, alpha=0.5) +  # add the species labels
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=grp),size=3) + # add the point markers
    geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=5,vjust=0) +  # add the site labels
    scale_colour_manual(values=Colours) +
    scale_fill_manual(values=Colours) +
    # scale_x_continuous(limits = c(-1.4, 3), breaks = c(-1,0,1,2,3)) +
    # scale_y_continuous(limits = c(-1.4, 1.2), breaks = c(-1,-0.5,0, 0.5 ,1)) +
    scale_x_continuous(limits = c(-0.7, 0.8), breaks = c(-0.5,0,0.5,1,1.5)) +
    scale_y_continuous(limits = c(-0.7, 0.5), breaks = c(-0.5,0,0.5,1,1.5)) +
    #coord_equal() +
    theme_classic() +
   # ggtitle("Broadband Sound Type Abundance") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(size = 1.5),  # increase axis line thickness
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 16),  # increase legend text size
          legend.title = element_text(size = 18),
          legend.background = element_rect(color = "grey", size = 0.5))+
    # legend.position = "top")+
    guides(colour = FALSE)
)


# ----
# ----
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


# ----
# ---- 
## NMDS Low presence ----
low_presence<- read_excel("data/phonic_richness.xlsx", 
                                      sheet = "low_presence (2)")
View(low_presence)
low_presence<- subset(low_presence, select = -c(habitat) ) #remove unnecessary columns

low_presence<- low_presence %>%
  column_to_rownames(var = "site")

low_presence_2 <- low_presence_2 %>%
  mutate(site = recode(site, "ardmore" = "Ardmore", 
                       "port_dinallaen"="Port Dinllaen", 
                       "canna" = "Canna", 
                       "isle_of_soay"="Isle of Soay", 
                       "kyles_of_bute"= "Kyles of Bute",
                       "gallanach_bay"= "Gallanach Bay", 
                       "gansey_bay"= "Gansey Bay", 
                       "kintyre"= "Kintyre",
                       "skye"= "Skye", 
                       "craignish"="Loch Craignish"))
str(low_presence)
low_presence_2<- merge(habitats, low_presence, by = c("site"))
low_presence_2<- low_presence_2 %>%
  column_to_rownames(var = "site")
low_presence_2$habitat<- as.factor(low_presence_2$habitat)
str(low_presence_2)


## Distance matrix and PERMANOVA
low_presence_dist<- vegdist(low_presence_2[,-c(habitat)], method = "bray", binary = T)
low_presence_perm<- adonis2(low_presence_dist~ habitat, data = low_presence_2)
low_presence_perm




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
## ----
## NMDS Low presence Plot ----
# the nmds
low_presence_nmds<- metaMDS(low_presence_2[,-c(1)], #the community data
                              distance = "bray",
                            binary = T,
                              k=2,# Using bray-curtis distance
                              try = 300)

low_presence_nmds
stressplot(low_presence_nmds)
#simper
low_presence_basic_simper<- simper(low_presence_2[,-c(habitat)],
                          distance = "bray",
                          binary = T,
                      permutations = 999) # permutations to run

summary(low_presence_basic_simper , ordered = TRUE) #summary is the total contrast.

habitat_simper<- simper(nmds_low_richness_matrix, 
                        habitats$habitat,
                        permutations = 999)
summary(habitat_simper)


group = as.character(low_presence_2$habitat)
unique(low_presence_2$habitat)
colors = group
colors[colors=="1"] <- "Green"
colors[colors==2] <- "Purple"
colors[colors==3] <- "Orange"

ordiplot(low_presence_nmds, type = "n", cex.axis = 1.5, cex.lab=1.5)

for(i in unique(group)) {
  ordihull(low_presence_nmds$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(low_presence_nmds, display = "species", label=F, col = "Grey50", air = 0.01)

fit <- adonis2(low_presence_2[, -c(1)] ~ habitat, low_presence_2, 
               permutations = 999, method = "bray")
fit

##needs to a be highlighted and run together

## Now to try in ggplot
# ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
Colours <- c( "#56B4E9", "#009E73", "#F0E442")
names(Colours) <- c("1", "2", "3")
Colours_darker <- darken(Colours, 0.4)
names(Colours_darker) <- c("1", "2", "3")

data.scores <- as.data.frame(scores(low_presence_nmds)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <-low_presence_2$habitat  #  add the grp variable created earlier
View(data.scores)

species.scores <- as.data.frame(scores(low_presence_nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores) 
# insect_cols_mansort$Corrected_name

hull.data <- data.frame()
for(i in 1:length(unique(low_presence_2$habitat))){
  temp <- data.scores[data.scores$grp == unique(low_presence_2$habitat)[i], ][chull(data.scores[data.scores$grp == 
                                                                                                   unique(low_presence_2$habitat)[i], c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(low_presence_nmds_plot <- ggplot() +
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30) +# add the convex hulls
    labs(fill = "Habitat Category") +
    #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size= 2, alpha=0.5) +  # add the species labels
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=grp),size=3) + # add the point markers
    geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=5,vjust=0) +  # add the site labels
    scale_colour_manual(values=Colours) +
    scale_fill_manual(values=Colours) +
    # scale_x_continuous(limits = c(-1.4, 3), breaks = c(-1,0,1,2,3)) +
    # scale_y_continuous(limits = c(-1.4, 1.2), breaks = c(-1,-0.5,0, 0.5 ,1)) +
    scale_x_continuous(limits = c(-0.5, 0.5), breaks = c(-0.5,0,0.5,1,1.5)) +
    scale_y_continuous(limits = c(-0.5, 0.5), breaks = c(-0.5,0,0.5,1,1.5)) +
    #coord_equal() +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(size = 1.5),  # increase axis line thickness
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 16),  # increase legend text size
          legend.title = element_text(size = 18),
          legend.background = element_rect(color = "grey", size = 0.5))+
    # legend.position = "top")+
    guides(colour = FALSE))


## ----
## NMDS low Abundance ----
low_abundance<- read_excel("data/phonic_richness.xlsx", 
                                      sheet = "low_activity (2)")
View(low_abundance)
low_abundance<- subset(low_abundance, select = -c(habitat))
low_abundance<- low_abundance %>%
  column_to_rownames(var = "site")
str(low_abundance)
low_abundance<- low_abundance[, 1:12] <- low_abundance[, 1:12] * 13


##nmds
nmds_low_abundance<-  metaMDS(low_abundance, #the community data
                             distance = "bray",
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

## adding habitat
habitats <- read_excel("data/meta_richness.xlsx", 
                       sheet = "habitats")
View(habitats)
habitats$habitat<- as.factor(habitats$habitat)


#simper
basic_simper_low_abundance<- simper(low_abundance_2[,-c(habitat)], #our community data set
                      permutations = 999) # permutations to run

summary(basic_simper_low_abundance , ordered = TRUE) #summary is the total contrast.
# none have significant p values, but highest ratio is the thump

habitat_simper_low_abundance<- simper(low_abundance_2[,-c(habitat)], 
                        low_abundance_2$habitat,
                        permutations = 999)
summary(habitat_simper_low_abundance)
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

## ----
### NMDS Low abundance plot ----
# the nmds
str(low_abundance_2)
low_abundance_dist<- vegdist(low_abundance_2[,-c(habitat)], method="bray")
low_abundance_perm<- adonis2(low_abundance_dist~ habitat, data = low_abundance_2)
low_abundance_perm


low_abundance_nmds<- metaMDS(low_abundance_2[,-c(habitat)], #the community data
                            distance = "bray",
                            k=2,
                            try = 300)
low_abundance_nmds
stressplot(low_abundance_nmds)

#simper
low_abund_basic_simper<- simper(low_abundance_2[,-c(habitat)],
                                distance = "bray", #our community data set
                      permutations = 999) # permutations to run

summary(low_abund_basic_simper , ordered = TRUE) #summary is the total contrast.
# none have significant p values, but highest ratio is the thump
low_abund_habitat_simper<- simper(low_abundance_2[,-c(habitat)], 
                                  distance= "bray",
                        low_abundance_2$habitat,
                        permutations = 999)
summary(low_abund_habitat_simper)

low_abundance<- read_excel("data/phonic_richness.xlsx", 
                           sheet = "low_activity (2)")
low_abundance<- subset(low_abundance, select = -c(habitat))
low_abundance<- low_abundance %>%
  column_to_rownames(var = "site")
site<- rownames(low_abundance)
low_abundance<- low_abundance[, 1:12] <- low_abundance[, 1:12] * 13
low_abundance$site<- site
str(low_abundance_2)
low_abundance_2<- merge(habitats, low_abundance, by = c("site"))
low_abundance_2 <- low_abundance_2 %>%
  mutate(site = recode(site, "ardmore" = "Ardmore", 
                       "port_dinallaen"="Port Dinllaen", 
                       "canna" = "Canna", 
                       "isle_of_soay"="Isle of Soay", 
                       "kyles_of_bute"= "Kyles of Bute",
                       "gallanach_bay"= "Gallanach Bay", 
                       "gansey_bay"= "Gansey Bay", 
                       "kintyre"= "Kintyre",
                       "skye"= "Skye", 
                       "craignish"="Loch Craignish"))
low_abundance_2<- low_abundance_2 %>%
  column_to_rownames(var = "site")
low_abundance_2$habitat<- as.factor(low_abundance_2$habitat)
print(low_abundance_2)



group = as.character(low_abundance_2$habitat)
unique(low_abundance_2$habitat)
colors = group
colors[colors=="1"] <- "Green"
colors[colors==2] <- "Purple"
colors[colors==3] <- "Orange"

ordiplot(low_abundance_nmds, type = "n", cex.axis = 1.5, cex.lab=1.5)

for(i in unique(group)) {
  ordihull(low_abundance_nmds$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(low_abundance_nmds, display = "species", label=F, col = "Grey50", air = 0.01)

fit <- adonis2(low_abundance_2[, -c(1)] ~ habitat, low_abundance_2, 
               permutations = 999, method = "bray")
fit

##needs to a be highlighted and run together

## Now to try in ggplot
# ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
Colours <- c( "#56B4E9", "#009E73", "#F0E442")
names(Colours) <- c("1", "2", "3")
Colours_darker <- darken(Colours, 0.4)
names(Colours_darker) <- c("1", "2", "3")

data.scores <- as.data.frame(scores(low_abundance_nmds)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <-low_abundance_2$habitat  #  add the grp variable created earlier
View(data.scores)

species.scores <- as.data.frame(scores(low_abundance_nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores) 
# insect_cols_mansort$Corrected_name

hull.data <- data.frame()
for(i in 1:length(unique(low_abundance_2$habitat))){
  temp <- data.scores[data.scores$grp == unique(low_abundance_2$habitat)[i], ][chull(data.scores[data.scores$grp == 
                                                                                                  unique(low_abundance_2$habitat)[i], c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(low_abundance_nmds_plot <- ggplot() +
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30) +# add the convex hulls
    labs(fill = "Habitat Category") +
   #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size= 2, alpha=0.5) +  # add the species labels
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=grp),size=3) + # add the point markers
    geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=5,vjust=0) +  # add the site labels
    scale_colour_manual(values=Colours) +
    scale_fill_manual(values=Colours) +
    # scale_x_continuous(limits = c(-1.4, 3), breaks = c(-1,0,1,2,3)) +
    # scale_y_continuous(limits = c(-1.4, 1.2), breaks = c(-1,-0.5,0, 0.5 ,1)) +
    scale_x_continuous(limits = c(-1, 1), breaks = c(-0.5,0,0.5,1,1.5)) +
    scale_y_continuous(limits = c(-0.7, 0.6), breaks = c(-0.5,0,0.5,1,1.5)) +
    #coord_equal() +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(size = 1.5),  # increase axis line thickness
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 16),  # increase legend text size
          legend.title = element_text(size = 18),
          legend.background = element_rect(color = "grey", size = 0.5))+
    # legend.position = "top")+
    guides(colour = FALSE)
)
### ----
### ----
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
str(merged3)

#Compute average richness for each time point and habitat category
df_avg <- meta3 %>%
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
merged3$habitat<- as.factor(merged3$habitat)
merged3 <- merged3 %>%
  mutate(site = recode(site, "ardmore" = "Ardmore", 
                       "port_dinallaen"="Port Dinllaen", 
                       "canna" = "Canna", 
                       "isle_of_soay"="Isle of Soay", 
                       "kyles_of_bute"= "Kyles of Bute",
                       "gallanach_bay"= "Gallanach Bay", 
                       "gansey_bay"= "Gansey Bay", 
                       "kintyre"= "Kintyre",
                       "skye"= "Skye", 
                       "craignish"="Loch Craignish"))
"#00CD66"
custom_palette<- c("#87CEFF", "#FFFF00", "#40E0D0", "#4F94CD", "#EEEE00", "#00CD66", "#8B8B00", "#54FF9F", "#CDCD00", "#008B45")
### Plot by habitat
ggplot(merged3, aes(x = time, y = richness, color = site)) +
  geom_smooth(se=F)+  
 # scale_colour_manual(values = custom_palette) + 
  labs(x = "Time", y = "Phonic Richness per 2 minute sample", color = "Site") +  
  theme_classic()+
  facet_wrap(~habitat, scales = "free") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(size = 1.5),  # increase axis line thickness
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),  # increase legend text size
        legend.title = element_text(size = 18),
        legend.background = element_rect(color = "grey", size = 0.5),
        strip.text = element_text (size= 18),
        strip.background = element_rect(color = "black", size = 1.5) 
  )

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
str(long_data)
sound_time<- merged3 %>%
  group_by(day)%>%
  reframe(low_grunt = low_grunt, 
            click= click,
            croak = croak, 
            snap = snap, 
            pinch = pinch, 
            thump_series = thump_series, 
            scream = scream, 
            gulp = gulp, 
            hoot = hoot, 
            scrape = scrape, 
            burp = burp, 
            grunt = grunt, 
            thump = thump, 
            growl = growl, 
            rattle = rattle, 
            squeak = squeak, 
            knock = knock ) %>%
  ungroup()

View(sound_time)
# Create a function to count presence for each species
count_presence <- function(x) {
  sum(x) / length(x)
}

# Group by category and apply the count_presence function to each species column
summary <- sound_time %>%
  group_by(day) %>%
  summarise(across(c("low_grunt", "click", "croak", "snap", "pinch", "thump_series", "scream", "gulp", "hoot", "scrape", "burp", "grunt", "thump", "growl", "rattle", "squeak"), .fns = count_presence))%>%
  ungroup()

# Print the summary
time_long <- pivot_longer(summary, cols = -day, names_to = "species", values_to = "occurrence")
print(time_long)
time_long <- time_long %>%
  mutate(Species = recode(species, "burp" = "Burp", 
                          "click"="Click", 
                          "croak" = "Croak", 
                          "growl"="Growl", 
                          "grunt"= "Grunt",
                          "gulp"= "Gulp", 
                          "hoot"= "Hoot", 
                          "low_grunt"= "Low Grunt",
                          "scrape"= "Scrape", 
                          "scream"="Scream",
                          "thump" = "Thump",
                          "thump_series" = "Thump Series",
                          "rattle"= "Rattle",
                          "pinch"= "Pinch",
                          "snap" = "Snap",
                          "squeak" = "Squeak"))
# Create the plot
View(time_long)
ggplot(time_long, aes(x = day, y = occurrence, fill = day)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = time_colour) +
  facet_wrap(~ Species) +
  labs(x = "Time Point", y = "Probability of Occurrence", fill = "Time Point") +
  theme_classic()
  
time_colour<- c("orange", "green", "darkblue")


time_dist<- vegdist(summary[,-c(1)], method="bray")
time_dist
summary$day<- day
perm_time<- adonis2(time_dist ~ day, data = summary)
summary(perm_time)
  
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

summary_data <- summary_data %>%
  mutate(site = recode(site, "ardmore" = "Ardmore", 
                       "port_dinallaen"="Port Dinllaen", 
                       "canna" = "Canna", 
                       "isle_of_soay"="Isle of Soay", 
                       "kyles_of_bute"= "Kyles of Bute",
                       "gallanach_bay"= "Gallanach Bay", 
                       "gansey_bay"= "Gansey Bay", 
                       "kintyre"= "Kintyre",
                       "skye"= "Skye", 
                       "craignish"="Loch Craignish"))
# make site names presentable

summary_data <- summary_data %>%
  mutate(Species = recode(Species, "burp" = "Burp", 
                       "click"="Click", 
                       "croak" = "Croak", 
                       "growl"="Growl", 
                       "grunt"= "Grunt",
                       "gulp"= "Gulp", 
                       "hoot"= "Hoot", 
                       "low_grunt"= "Low Grunt",
                       "scrape"= "Scrape", 
                       "scream"="Scream",
                       "thump" = "Thump",
                       "thump_series" = "Thump Series"))

# Plot
# trying proportional fill
## colours
custom_palette<- c("#7FFFD4", "#53868B", "#66CDAA", "#9BCD9B", "#66CD00", "#AB82FF", "#B452CD", "#8EE5EE", "#00BFFF", "#009ACD", "#00CDCD", "#1C86EE")

(long_data_plot<- ggplot(summary_data, aes(x = site, y = mean_abundance, fill = Species))+ #(Species, levels= c(
  #"Burp", "Click", "Croak", "Thump", "Thump Series", "Gulp", "Scrape", "Growl", "Grunt", "Hoot", "Low Grunt", "Scream")))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette) + 
  labs( x = "Site",
       y = "Relative Abundance",
       fill = "Sound Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16)) +
  facet_wrap(~habitat, scales = "free") + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(size = 1.5),  # increase axis line thickness
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 16),  # increase legend text size
          legend.title = element_text(size = 18),
          legend.background = element_rect(color = "grey", size = 0.5),
          strip.text = element_text (size= 18),
          strip.background = element_rect(color = "black", size = 1.5) 
          )
)

# Relative Abundances for full band 
full_relative_abundance <- read_excel("data/phonic_richness.xlsx", 
                                     sheet = "full_relative_abundance (2)")
print(full_relative_abundance)
full_relative_abundance<- subset(full_relative_abundance, select = -c(knock))
full_relative_abundance$habitat<- as.factor(c(3,3,3,2,2,2,1,1,1,3))

# Make the data in long format
long_data <- gather(full_relative_abundance, key = "Species", value = "RelativeAbundance", -c(site, habitat))

summary_data <- long_data %>%
  group_by(Species, site, habitat) %>%
  summarise(mean_abundance = mean(RelativeAbundance),
            se_abundance = sd(RelativeAbundance) / sqrt(n())) %>%
  ungroup()

summary_data <- summary_data %>%
  mutate(site = recode(site, "ardmore" = "Ardmore", 
                       "port_dinallaen"="Port Dinllaen", 
                       "canna" = "Canna", 
                       "isle_of_soay"="Isle of Soay", 
                       "kyles_of_bute"= "Kyles of Bute",
                       "gallanach_bay"= "Gallanach Bay", 
                       "gansey_bay"= "Gansey Bay", 
                       "kintyre"= "Kintyre",
                       "skye"= "Skye", 
                       "craignish"="Loch Craignish"))
# make site names presentable

print(summary_data)
summary_data <- summary_data %>%
  mutate(Species = recode(Species, "burp" = "Burp", 
                          "click"="Click", 
                          "croak" = "Croak", 
                          "growl"="Growl", 
                          "grunt"= "Grunt",
                          "gulp"= "Gulp", 
                          "hoot"= "Hoot", 
                          "low_grunt"= "Low Grunt",
                          "scrape"= "Scrape", 
                          "scream"="Scream",
                          "thump" = "Thump",
                          "thump_series" = "Thump Series",
                          "rattle"= "Rattle",
                          "pinch"= "Pinch",
                          "snap" = "Snap",
                          "squeak" = "Squeak")) %>%
  mutate(Species = factor(Species,
                          levels= c(
                            "Burp", "Click", "Croak", "Thump", "Thump Series", "Gulp", "Scrape", "Growl", "Grunt", "Hoot", "Low Grunt", "Scream", "Pinch", "Snap", "Squeak", "Rattle"
                          )))

# Plot
# trying proportional fill
## colours
custom_palette <- c("#FF6A6A", "#87CEFA", "#8EE5EE", "#ADFF2F", "#008B00", "#EE6363", "#00CD00", "#9BCD9B", "#FFAEB9", "#A2CD5A", "#009ACD", "#00B2EE", "#BA55D3", "#7B68EE", "#9400D3", "#EE00EE")


custom_palette<- c("#7FFFD4", "#53868B", "#66CDAA", "#9BCD9B", "#66CD00", "#AB82FF", "#B452CD", "#8EE5EE", "#00BFFF", "#009ACD", "#00CDCD", "#1C86EE", "#FFB6C1", "#FF6EB4", "#EE6363", "#FF82AB")



(long_data_plot<- ggplot(summary_data, aes(x = site, y = mean_abundance, fill = factor(Species, levels= c(
  "Burp", "Click", "Croak", "Thump", "Thump Series", "Gulp", "Scrape", "Growl", "Grunt", "Hoot", "Low Grunt", "Scream", "Pinch", "Snap", "Squeak", "Rattle")))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = custom_palette) + 
    labs( x = "Site",
          y = "Relative Abundance",
          fill = "Sound Type") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16)) +
    facet_wrap(~habitat, scales = "free") + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(size = 1.5),  # increase axis line thickness
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 16),  # increase legend text size
          legend.title = element_text(size = 18),
          legend.background = element_rect(color = "grey", size = 0.5),
          strip.text = element_text (size= 18),
          strip.background = element_rect(color = "black", size = 1.5) 
    )
)

