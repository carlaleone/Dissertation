# Univariate comparisons Dissertation
# 12/03/2024
# Carla Leone

## Install packages and load data ----
install.packages("tidyverse")
install.packages("vegan")
library(tidyverse)
library(vegan)
library(readxl)

meta_richness <- read_excel("data/meta_richness.xlsx", 
                            sheet = "big_sheet (2)")
View(meta_richness)

## Specpool to calculate richness----
View(richness)
colnames(richness)
richness_specpool<-  subset(richness, select = -c(recording,time, minute,boat, water, richness,max_richnes,max_richness, samples,invert,fish, ...33,invert_dominance, samples, habitat) )
View(richness_specpool)
richness_specpool<- richness_specpool %>%
  column_to_rownames(var = "site")
richness_specpool <- sapply(richness_specpool, as.numeric)
specpool(richness_specpool, site, smallsample=TRUE)

## Add column for habitat ----
richness <- richness %>%
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
boxplot(richness$richness~richness$habitat)

View(richness)
## ANOVA ---- 
aov_richness<- aov(log(richness)~habitat, data=richness)
summary(aov_max_richness)
plot(aov_richness)


aov_max_richness<- aov(max_richness~habitat, data=richness)
summary(aov_max_richness)
plot(aov_max_richness)

#Assumptions for anova are not met, so lets try the kruskal wallis
kw_richness<- kruskal.test(richness~habitat, data=richness)
kw_richness

kw_max_richness<- kruskal.test(max_richness~habitat, data=richness)
kw_max_richness
#insignificant

### Assumptions
help(aov)

### Assumptions for ANOVA
plot


## New Code ----
univariate <- read_excel("data/meta_richness.xlsx", 
                            sheet = "diversity")
univariate<-  subset(univariate, select = -c(cetacean) )

View(univariate)
univariate$habitat<- as.factor(univariate$habitat)

## High to low frequency ratio vs habitat ----
boxplot(univariate$high_low~univariate$habitat)
kruskal_high_low<-kruskal.test(high_low~habitat, data=univariate)
kruskal_high_low

## Diversity models ----
diversity <- read_excel("data/meta_richness.xlsx", 
                            sheet = "diversity")
View(diversity)
boxplot(diversity$simpson~diversity$habitat)
boxplot(diversity$avg_richness~diversity$habitat)

kw_avg_richness<- kruskal.test(avg_richness~habitat, data=diversity)
summary(aov_avg_richness)
kw_avg_richness
#not significant

## High to low frequency ratio over time ----
ratios <- read_excel("data/meta_richness.xlsx", 
                            sheet = "big_sheet (2)")

ratios<-  subset(ratios, select = -c(samples,...33) )
# add the habitat types
ratios <- ratios %>%
  mutate(habitat = case_when(
    site %in% c("port_dinallaen", "ardmore", "gallanach_bay") ~ "1",
    site %in% c("craignish", "skye", "kintyre") ~ "2",
    site %in% c("gansey_bay", "kyles_of_bute", "isle_of_soay", "canna") ~ "3",
    TRUE ~ NA_character_  # In case there are unmatched sites
  )) #match sites to habitat

View(ratios)

#plot the ratio over time for each habitat
df_avg_ratios <- ratios %>%
  group_by(time,habitat) %>%
  summarise(avg_ratio = mean(invert_dominance),
            sd_ratio = sd(invert_dominance),
            se_ratio = sd(invert_dominance) /sqrt(n()))%>%
  ungroup()#gives the stats for each habitat at each time point. Has taken the average richness of each habitat for each time.


View(df_avg_ratios)
df_avg_ratios[is.na(df_avg_ratios)] <- 0

### Plot by habitat
ggplot(df_avg_ratios, aes(x = time, y = avg_ratio, color = habitat)) +
  geom_line() +  # Add lines
  geom_point() +  # Add points
  labs(x = "Time", y = "Average Richness", color = "Habitat Category") +  # Labels
  theme_minimal()  # Optional: change theme if desired

### Try a model----
lm_ratios<- lm(invert_dominance~time + habitat, data = ratios)
summary(lm_ratios)
