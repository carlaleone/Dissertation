# Random Forest ML
# 30/03/2024
# Carla Leone

## Load the data and merge the indices with other information----
#load packages
library(randomForest)
library(caret)

indices <- read_excel("data/results.clean.xlsx", 
                      sheet = "matched_times")

phonic <- read_excel("data/phonic_richness.xlsx", 
                     sheet = "big_sheet (3)")


View(indices)
indices<- subset(indices, select = -c(long, lat) ) #remove unnecessary columns

# merge the datasets
View(phonic)
View(indices)
indices <- indices %>%
  rename(minute = recording_minute)
phonic$wav_files <- paste(phonic$recording, phonic$minute, sep = "")
merged <- merge(indices, phonic, by = c("site", "wav_files"))
#----
#----
### Phonic Richness Response ----

## RF ON Habitat 1 ----
str(merged)
merged_habitats<- subset(merged, select = c(1, 4:24,51,57))
View(merged_habitats)
merged_habitats$richness<- as.factor(merged_habitats$richness)
## Split data into habitats
# Data frame for habitat category 1
df_habitat_1 <- merged_habitats %>%
  filter(habitat == 1)%>%
  select(-c(habitat, site))
df_habitat_1$richness<- as.factor(df_habitat_1$richness)
levels(df_habitat_1$richness)
df_habitat_1$richness <- droplevels(df_habitat_1$richness)
str(df_habitat_1)

# training
set.seed(222)
ind1 <- sample(2, nrow(df_habitat_1), replace = TRUE, prob = c(0.6, 0.4))
train1 <- df_habitat_1[ind1==1,]
test1 <- df_habitat_1[ind1==2,] 


rf_1<- randomForest(richness~., data= train1, importance= TRUE, proximity= TRUE)
print(rf_1)


p1 <- predict(rf_test_class, train_class)
confusionMatrix(p1, train_class$ richness)
p1

p2 <- predict(rf_1, test1)
confusionMatrix(p2, test1$ richness)


varImpPlot(rf_1,
           sort = T,
           n.var = 10,
           main = "Top 10 Variable Importance")
?varImpPlot

partialPlot(rf_test_class, classify, M_high)
## RF ON Habitat 2 ----
df_habitat_2 <- merged_habitats %>%
  filter(habitat == 2) %>%
  select(-c(habitat, site))
df_habitat_2$richness<- as.factor(df_habitat_2$richness)
View(df_habitat_2)
# training
set.seed(222)
ind2 <- sample(2, nrow(df_habitat_2), replace = TRUE, prob = c(0.6, 0.4))
train2 <- df_habitat_2[ind2==1,]  
test2 <- df_habitat_2[ind2==2,] 


#random forest

rf2 <- randomForest(richness~., data=train2, proximity=TRUE) 
print(rf2)

p1.2 <- predict(rf2, train2)
confusionMatrix(p1.2, train2$ richness)

p2.2<- predict(rf2, test2)
confusionMatrix(p2.2, test2$ richness)

varImpPlot(rf2,
           sort = T,
           n.var = 10,
           main = "Top 10 Variable Importance")
## RF ON Habitat 3 ----
df_habitat_3 <- merged_habitats %>%
  filter(habitat == 3)%>%
  select(-c(habitat, site))
df_habitat_1$richness<- as.factor(df_habitat_1$richness)


# training
set.seed(222)
ind3 <- sample(2, nrow(df_habitat_3), replace = TRUE, prob = c(0.6, 0.4))
train3 <- df_habitat_3[ind3==1,]
test3 <- df_habitat_3[ind3==2,] 


rf_3<- randomForest(richness~., data= train3, importance= TRUE, proximity= TRUE)
print(rf_3)


p1.3 <- predict(rf_3, train3)
confusionMatrix(p.31, train3$ richness)
p1.3

p2.3 <- predict(rf3, test3)
confusionMatrix(p2.3, test3$ richness)


varImpPlot(rf_3,
           sort = T,
           n.var = 10,
           main = "Top 10 Variable Importance")

#----
#----
### Habitat Complexity as a response ----
## Load the data set with all calculated indices ----
results_clean <- read_excel("data/results.clean.xlsx", 
                            sheet = "results.clean")
str(results_clean)
results_clean$habitat<- as.factor(results_clean$habitat)
results_clean <- subset (results_clean, select = c(2:23, 26)) # select columns for indices and habitat

## The model ----
# training
set.seed(222)
ind_class <- sample(2, nrow(results_clean), replace = TRUE, prob = c(0.6, 0.4))
train_class <- results_clean[ind_class==1,]
test_class <- results_clean[ind_class==2,] 


rf_test_class<- randomForest(habitat~., data= train_class, importance= TRUE, proximity= TRUE)
print(rf_test_class)
plot(rf_test_class)

p1 <- predict(rf_test_class, train_class)
confusionMatrix(p1, train_class$ habitat)
p1

p2 <- predict(rf_test_class, test_class)
confusionMatrix(p2, test_class$ habitat)
p2
?confusionMatrix
table(p2, test_class$ habitat)

varImpPlot(rf_test_class,
           sort = T,
           n.var = 10,
           main = "Top 10 Variable Importance")