# Dissertation Modeling Analysis
# 12/03/2024
# Carla Leone

## Load the data
indices <- read_excel("data/results.clean.xlsx", 
                      sheet = "matched_times")
meta_richness <- read_excel("data/meta_richness.xlsx", 
                            sheet = "big_sheet (2)")
meta_richness
View(indices)
indices<- subset(indices, select = -c(time) ) #remove unnecessary columns

# merge the datasets
indices <- indices %>%
  rename(minute = recording_minute)
merged <- merge(indices, meta_richness, by = c("site", "minute"))
View(merged)                

#ACI Low
avg_aci_low <- merged %>%
  group_by(time,max_richness) %>%
  summarise(avg_aci = mean(ACI_low),
            sd_aci = sd(ACI_low),
            se_aci = sd(ACI_low) /sqrt(n()))%>%
  ungroup()#gives the stats for each habitat at each time point. Has taken the average richness of each habitat for each time.

View(avg_aci_low)


#plot
ggplot(avg_aci_low, aes(x = time, y = avg_aci, color = habitat)) +
  geom_line() +  # Add lines
  geom_point() +  # Add points
  labs(x = "Time", y = "Average ACI Low Frequency Band", color = "Habitat Category") +  # Labels
  theme_minimal()  # Optional: change theme if desired


# aci vs max richness
merged$max_richness<- as.factor(merged$max_richnes)
richness_aci <- merged %>%
  group_by(time, max_richness) %>%
  summarise(avg_aci = mean(ACI_low),
            sd_aci = sd(ACI_low),
            se_aci = sd(ACI_low) /sqrt(n()))%>%
  ungroup()

#plot
ggplot(richness_aci, aes(x = time, y = avg_aci, color = max_richness)) +
  geom_line() +  # Add lines
  geom_point() +  # Add points
  labs(x = "Time", y = "Average ACI Low Frequency Band", color = "Habitat Category") +  # Labels
  theme_minimal()  # Optional: change theme if desired



### Try a model
lm_ratios<- lm(invert_dominance~time + habitat, data = ratios)
summary(lm_ratios)
