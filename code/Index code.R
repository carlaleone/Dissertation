# Exploring initial data with 2 habitats
# Carla Leone
# January 27th 2024

## Load the data and the packages ----
install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(readxl)
install.packages("xlsx")
library(xlsx)
total_results <- read_excel("total_results.xlsx")
View(total_results)

## Create a new dataset where the wav_files column is deleted and split into two other columns, on with the file and one with the minutes
results_clean<- total_results %>%
  mutate(recording_minute = str_sub(wav_files, 16),
         file_name = substr(wav_files, 1, 15))

results_clean<- results_clean %>%
  mutate(recording_minute = gsub(".WAV$", "", recording_minute),
         wav_files = gsub(".WAV$", "", wav_files))

View(results_clean)


### Make needed columns into characters ----
results_clean$site<- as.factor(results_clean$site)
results_clean$habitat<- as.factor(results_clean$habitat)

indices<- results_clean %>%
  select(-wav_files, -recording, -recording_minute, -file_name, -site)

## Group by site to find the average values----
site <- indices %>%
  group_by(site) %>%
  summarize_all(.funs = list(mean = mean, sd = sd), na.rm = TRUE)%>%
  ungroup()

View(site)

site$habitat<- c(1, 3, 2, 1, 3, 3, 2, 3, 1, 2)
site$habitat<- as.factor(site$habitat)

## Group by habitat ----
habitats <- indices %>%
  group_by(habitat) %>%
  summarize_all(.funs = list(mean = mean, sd = sd), na.rm = TRUE)%>%
  ungroup()

View(habitats)

# gives the average and stdev for each site for each measured index 

## Trying some initial plots ----
ggplot(site, aes(x = habitat, y = ACI_high_mean, fill = habitat)) +
  geom_boxplot() +
  theme_minimal()

## Stats ----
# NDMS + SIMPER

write.csv(results_clean, "results.clean.csv", row.names = FALSE)
