# Univariate comparisons Dissertation
# 12/03/2024
# Carla Leone

## Install packages and load data ----
install.packages("tidyverse")
install.packages("vegan")
library(tidyverse)
library(vegan)
library(readxl)

full_richness <- read_excel("data/phonic_richness.xlsx", 
                            sheet = "big_sheet (3)")
low_richness<- read_excel("data/phonic_richness.xlsx", 
                          sheet = "new_low_sheet ")
View(low_richness)
full_richness$habitat<- as.factor(full_richness$habitat)
full_richness$max_richness<- as.numeric(full_richness$max_richnes)
### Richness by habitat ----
## Initial Boxplots 
boxplot(full_richness$richness~full_richness$habitat)
#look very similar
boxplot(full_richness$richness~full_richness$revised_habitat)
#1 has the highest, but very large error bars
boxplot(low_richness$low_richness~low_richness$habitat)
#see some differences, with 2 at the lowest, 
boxplot(low_richness$low_richness~low_richness$revised_habitat)
#with the revised habitat, 1 has an even higher richness

### Max richness comparisons----
##full band
full_max_richness <- full_richness %>%
  group_by(site,habitat) %>%
  summarise(max_richness = mean(max_richness)) %>%
  ungroup()
View(full_max_richness)

#boxplots
boxplot(full_max_richness$max_richness~full_max_richness$habitat)
kw_full_max_richness<- kruskal.test(max_richness~habitat, data=full_max_richness)
kw_full_max_richness #not sig, p=0.876


##low band
phonic_richness <- read_excel("data/phonic_richness.xlsx", 
                              +     sheet = "new_low_sheet ")
low_richness<- phonic_richness 
View(low_richness)
low_richness$max_richnes<- as.numeric(low_richness$max_richnes)
low_max_richness <- low_richness %>%
  group_by(site,habitat) %>%
  summarise(max_richness = mean(max_richnes)) %>%
  ungroup()
View(low_max_richness)

#boxplots
boxplot(low_max_richness$max_richness~low_max_richness$habitat)
kw_low_max_richness<- kruskal.test(max_richness~habitat, data= low_max_richness)
kw_low_max_richness
#not significant, but given the plots looks like 2 is far lower


### Diversity Comparisons ----
## full band
full_diversity <- full_richness %>%
  group_by(site,habitat, revised_habitat) %>%
  summarise(diversity = mean(simpson)) %>%
  ungroup()
View(full_diversity)
boxplot(full_diversity$diversity~full_diversity$habitat)
boxplot(full_diversity$diversity~full_diversity$revised_habitat)
kw_full_diversity<- kruskal.test(diversity~habitat, data=full_diversity)
kw_full_diversity
# not significant 

## low band
low_diversity <- low_richness %>%
  group_by(site,habitat, revised_habitat) %>%
  summarise(diversity = mean(simpson)) %>%
  ungroup()
View(low_diversity)
boxplot(low_diversity$diversity~low_diversity$revised_habitat)

boxplot(low_diversity$diversity~low_diversity$habitat)
kw_low_diversity<- kruskal.test(diversity~habitat, data=low_diversity)
kw_low_diversity
# very close to being significant : p= 0.077

### Low Relative Abundance ----
low_relative_abundance <- read_excel("data/phonic_richness.xlsx", 
                                     +     sheet = "low_relative_abundance (2)")
View(low_relative_abundance)
low_relative_abundance$habitat<- as_factor(low_relative_abundance$habitat)
low_relative_abundance<- subset(low_relative_abundance, select = -c(1))

low_relative_abundance_means<- low_relative_abundance %>%
  group_by(habitat)%>%
  summarise_all(mean) %>%
  ungroup()

View(low_relative_abundance_means)
#stdev and means of the relative abundance

# Transpose the data frame
low_relative_abundance2<- low_relative_abundance %>%
  column_to_rownames(var = "habitat")

df_transposed <- t(low_relative_abundance_means)
View(df_transposed)
df_transposed <- df_transposed[-1, ]
# Calculate mean and standard deviation for each species
df_summary <- df_transposed %>%
  as.data.frame() %>%  # Convert back to data frame
  summarise_all(list(mean = mean, sd = sd), na.rm = TRUE)

# Print the summary
print(df_summary)

low_relative_abundance_means$rank <- rank(-df_transposed$ri ) 

df$rank <- rank(-df$relative_abundance) 
### low freq max richness ----
low_meta <- read_excel("data/meta_richness.xlsx", 
                       sheet = "low_presence")
View(low_meta)
boxplot(low_meta$max_richness~low_meta$habitat)

aov_max_richness<- aov(max_richness~habitat, data=low_meta)
summary(aov_max_richness)

kw_low_max_richness<- kruskal.test(max_richness~habitat, data=low_meta)
kw_low_max_richness


### Low Band Richness vs Time----
lm_lowrichness_time<- lm(low_richness ~ time+site, data=low_richness)
plot(lm_lowrichness_time)
summary(lm_lowrichness_time)
str(low_richness)

##using spearman
View(low_richness)
correlation_habitat1 <- cor(low_richness$low_richness[low_richness$site == ardmore], as.numeric(low_richness$time)[low_richness$habitat == ardmore], method = "spearman")
correlation_habitat2 <- cor(low_richness$low_richness[low_richness$site == kyles_of_bute], as.numeric(low_richness$time)[low_richness$habitat == "kyles_of_bute"], method = "spearman")
correlation_habitat3 <- cor(low_richness$low_richness[low_richness$site == skye],as.numeric(low_richness$time)[low_richness$habitat == "skye"], method = "spearman")

# Print correlation coefficients for each habitat
print(paste("Spearman correlation coefficient for habitat 1:", correlation_habitat1))
print(paste("Spearman correlation coefficient for habitat 2:", correlation_habitat2))
print(paste("Spearman correlation coefficient for habitat 3:", correlation_habitat3))

plot(low_richness$low_richness[low_richness$habitat == 3]~as.numeric(low_richness$time)[low_richness$habitat == 3])
### low freq diversity----
low_richness <- read_excel("data/meta_richness.xlsx", 
                            sheet = "low_presence")
boxplot(low_richness$simpsons~low_richness$habitat)
View(low_richness)
## anova on low frequency diversity
aov_low_freq_diversity<- aov(simpsons~habitat, data= low_richness)
summary(aov_low_freq_diversity)

kw_low_freq_diversity<- kruskal.test(simpsons~habitat, data=low_richness)
kw_low_freq_diversity



# Function to calculate rarefied species richness


### low freq richness vs time ----
low_timeseries <- read_excel("data/meta_richness.xlsx", 
                           sheet = "low_freq")

View(low_timeseries)

low_timeseries$habitat<- as.factor(low_timeseries$habitat)

#create subset of data
low_habitat_time <- low_richnesss %>%
  group_by(time,habitat) %>%
  summarise(avg = mean(richness),
            sd = sd(richness),
            se = sd(richness) /sqrt(n()))%>%
  ungroup()#gives the stats for each habitat at each time point. Has taken the average richness of each habitat for each time.


low_meta$revised_habitat<- as.factor(low_meta$revised_habitat)

#plot richness vs time
low_richness$habitat<- as.factor(low_richness$habitat)
ggplot(low_richness, aes(x = time, y = low_richness, color = habitat)) +
  geom_smooth(se=T) +  # Add lines
  labs(x = "Time", y = "Average Richness", color = "Habitat Category") +  # Labels
  theme_minimal() +  # Optional: change theme if desired 
  geom_vline(xintercept = as.numeric(as.POSIXct("1899-12-31 04:30:00")), linetype = "dashed")  # Add vertical line


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
low_richness <- low_richness %>%
  mutate(habitat = case_when(
    site %in% c("port_dinallaen", "ardmore", "gallanach_bay") ~ "1",
    site %in% c("craignish", "skye", "kintyre") ~ "2",
    site %in% c("gansey_bay", "kyles_of_bute", "isle_of_soay", "canna") ~ "3",
    TRUE ~ NA_character_  # In case there are unmatched sites
  )) #match sites to habitat

View(low_richness)
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
