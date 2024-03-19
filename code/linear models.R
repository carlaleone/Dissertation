# Dissertation Modeling Analysis
# 12/03/2024
# Carla Leone

## Load the data----
indices <- read_excel("data/results.clean.xlsx", 
                      sheet = "matched_times")
phonic <- read_excel("data/meta_richness.xlsx", 
                            sheet = "big_sheet (2)")

View(indices)
indices<- subset(indices, select = -c(time) ) #remove unnecessary columns

# merge the datasets
indices <- indices %>%
  rename(minute = recording_minute)
merged <- merge(indices, meta_richness, by = c("site", "minute"))
View(merged)                

#ACI Low----
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


# aci vs max richness ----
merged$max_richness<- as.factor(merged$max_richnes)
richness_aci <- merged %>%
  group_by(time, richness) %>%
  summarise(avg_aci = mean(ACI_low),
            sd_aci = sd(ACI_low),
            se_aci = sd(ACI_low) /sqrt(n()))%>%
  ungroup()

#plot aci low vs time
time_aci <- merged %>%
  group_by(time, site) %>%
  summarise(avg_aci = mean(ACI_low),
            sd_aci = sd(ACI_low),
            se_aci = sd(ACI_low) /sqrt(n()))%>%
  ungroup()

ggplot(merged, aes(x = ACI_full, y = richness, color = habitat)) +
  geom_line() +  # Add lines
  geom_point() +  # Add points
  labs(x = "Time", y = "Average ACI Low Frequency Band", color = "Habitat Category") +  # Labels
  theme_minimal()  # Optional: change theme if desired

### Model with aci and other variables----
ggplot(merged,(aes(x=ACI_full, y = richness))+
         geom_smooth())
plot(merged$richness~ACI_low + habitat, data=merged)

lm1<- lm(richness ~ ACI_low + site, data=merged)
summary(lm1)

### Try a model ----
lm_ratios<- lm(invert_dominance~time + habitat, data = ratios)
summary(lm_ratios)

### Basic Models ----
boxplot(merged$ACI_full~merged$habitat)

lm_time<- lm(ACI_low~time + habitat, data=merged)
summary(lm_time)
plot(merged$ACI_low~merged)