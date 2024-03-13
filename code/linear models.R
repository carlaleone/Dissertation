# Dissertation Modeling Analysis
# 12/03/2024
# Carla Leone

## Load the data
indices <- read_excel("data/results.clean.xlsx", 
                      sheet = "matched_times")
View(indices)
indices<- subset(indices, select = -c(time) ) #remove unnecessary columns

# merge the datasets
indices <- indices %>%
  rename(minute = recording_minute)
merged <- merge(indices, meta_richness, by = c("site", "minute"))
View(merged)                