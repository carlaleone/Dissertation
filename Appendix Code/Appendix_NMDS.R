# Code for Question 2: PERMANOVA, NMDS, SIMPER
# Carla Leone
# April 2024

### Loading Packages ----
library(tidyverse)
library(vegan)
library(readxl)
#----
### Load and prepare the data----
max_richness <- read_excel("data/phonic_richness.xlsx", 
                           sheet = "full_presence (2)")
habitats <- read_excel("data/meta_richness.xlsx", 
                       sheet = "habitats")
habitats$habitat<- as.factor(habitats$habitat) # make habitat a factor
habitats<- subset(habitats, select= c(site, habitat)) #reduce the habitat data frame 
#creating max_richness 2 with habitat in the data frame to make plotting easier
max_richness_2<- merge(habitats, max_richness, by = c("site"))
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
                       "craignish"="Loch Craignish")) # make site names presentable
max_richness_2<- max_richness_2 %>%
  column_to_rownames(var = "site")
max_richness_2$habitat<- as.factor(max_richness_2$habitat)
View(max_richness)
#once max_richness two has been created, can make site the row names in max richness 1
max_richness<- max_richness %>%
  column_to_rownames(var = "site") # make site the name of the rows

#----
### Distance matrix and PERMANOVA ----
max_richness_dist<- vegdist(max_richness_2[,-c(habitat)], method="bray", binary =T)
# use binary because presence/absence data: not needed for abundance matrices
perm_max_richness<- adonis2(max_richness_dist ~ habitat, data = max_richness_2)
perm_max_richness

#----
### NMDS----
richness_nmds<- metaMDS(max_richness_2[,-c(habitat)], #the community data
                        distance = "bray",#distance matrix
                        binary = T, #binary data for presence/absence
                        k =2 ,  #specify 2 dimensions
                        try = 300)  # 300 tries

richness_nmds #results of the nmds, shows stress
goodness(richness_nmds) # vector of goodness of fit values for each site
stressplot(richness_nmds) # stressplot and shows non metric and linear r2

#----
### PERMDISP ----
dispersion
?betadisper()
permdisp_max_richness<- betadisper(dist_full_richness, habitat)
plot(permdisp_max_richness)
permdisp_max_richness
?permutest.betadisper
anova(permdisp_max_richness)
pmod <- permutest(permdisp_max_richness, permutations = 99, pairwise = TRUE)
pmod
# p value is 0.97 which indicates that the dispersion is not significantly different between the groups. 



#----
### SIMPER ----
basic_simper<- simper(max_richness_2[,-c(habitat)],
                      distance= "bray",
                      binary=T,
                      permutations = 999) # permutations to run

summary(basic_simper , ordered = TRUE) #summary is the total contrast.
# ungrouped simper, just shows sound contribution to total dissimilarities between sites.

habitat_simper<- simper(max_richness_2[,-c(habitat)], 
                        max_richness_2$habitat,
                        distance= "bray",
                        binary =T,
                        permutations = 999)
summary(habitat_simper)
#grouped simper, shows contributions to dissimilarities between habitat groups. 
#----
# NMDS Plot ----
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

