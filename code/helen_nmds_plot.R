# Helen nmds script

library(tidyverse)
library(vegan)


# NMDS ####--------------------------------------------------------------------


NMDS_rich <- metaMDS(max_richness, distance = "bray", k = 2, trymax=300) #creates our ordination. Bray-Curtis distance is chosen because it is not affected by zero values. k represents the number of dimensions we want and is used to reduce stress.

goodness(NMDS_rich)
stressplot(NMDS_rich)
plot(NMDS_rich)


# group = insects_trad_NMDS$Final_crop
# 
# # Create a vector of color values with same length as the vector of group values
# unique(insects_trad_NMDS$Final_crop)
# colors = group
# colors[colors=="WW"] <- "Red"
# colors[colors=="WB"] <- "Blue"
# colors[colors=="WOSR"] <- "Green"
# colors[colors=="LI"] <- "Purple"
# colors[colors=="SOSR"] <- "Orange"
# colors[colors=="W B"] <- "Yellow"
# 
# ordiplot(NMDS, type = "n", cex.axis = 1.5, cex.lab=1.5)
# 
# 
# for(i in unique(group)) {
#   ordihull(NMDS_invert$point[grep(i, group),], draw="polygon",
#            groups = group[group == i],col = colors[grep(i,group)],label=F) } 
# 
# orditorp(NMDS_invert, display = "species", label=F, col = "Grey50", air = 0.01)
# 
# fit <- adonis2(insects_trad_NMDS[, -c(1:3)] ~ Crop_longname, insects_trad_NMDS, 
#                permutations = 999, method = "bray")
# fit


# ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
Colours <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
names(Colours) <- c("BW", "LI", "SOSR", "WB", "WOSR", "WW")
Colours_darker <- darken(Colours, 0.4)
names(Colours_darker) <- c("BW", "LI", "SOSR", "WB", "WOSR", "WW")

data.scores <- as.data.frame(scores(NMDS_rich)$habitat)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$habitat <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- insects_trad_NMDS$Final_crop  #  add the grp variable created earlier
head(data.scores)

species.scores <- as.data.frame(scores(NMDS_rich, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores) 
# insect_cols_mansort$Corrected_name

hull.data <- data.frame()
for(i in 1:length(unique(insects_trad_NMDS$Final_crop))){
  temp <- data.scores[data.scores$grp == unique(insects_trad_NMDS$Final_crop)[i], ][chull(data.scores[data.scores$grp == 
                                                                                                        unique(insects_trad_NMDS$Final_crop)[i], c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(NMDS_plot <- ggplot() +
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=habitat,group=habitat),alpha=0.30) + # add the convex hulls
    #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=habitat),size=3) + # add the point markers
    #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
    scale_colour_manual(values=Colours) +
    scale_fill_manual(values=Colours) +
    # scale_x_continuous(limits = c(-1.4, 3), breaks = c(-1,0,1,2,3)) +
    # scale_y_continuous(limits = c(-1.4, 1.2), breaks = c(-1,-0.5,0, 0.5 ,1)) +
    scale_x_continuous(limits = c(-0.7, 1.6), breaks = c(-0.5,0,0.5,1,1.5)) +
    scale_y_continuous(limits = c(-0.7, 0.5), breaks = c(-0.5,-0.25,0,0.25,0.5)) +
    #coord_equal() +
    my_theme() +
    ggtitle("Invertebrates") +
    theme(plot.title = element_text(hjust = 0.5)))


