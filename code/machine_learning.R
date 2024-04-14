# Machine Learning Techniques
# 30/03/2024
# Carla Leone

## Load the data and merge the indices with other information----
#load packages
library(randomForest)
library(caret)
library(tidyverse)
library(readxl)

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

## Ben Williams: UMAP ----
install.packages("umap")
library(umap)
View(results_clean)
#normalize the data 
umap_data <- as.data.frame(lapply(results_clean[2:23], normalise))
View(umap_data)
umap_labels<- results_clean[, "habitat"]
umap<- umap(umap_data)
umap
?umap
plot(umap, umap_labels)
## k-nn from coding clubs ----
library(class)
library(gridExtra)
install.packages("gmodels")
library(gmodels)
# first class# first normalize the data
normalise <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
# apply to the data set
View(merged_avg)
merged_avg$habitat<- as.factor(merged_avg$habitat)
results.norm <- as.data.frame(lapply(merged_avg[3:23], normalise))
View(results.norm)

# Generating seed
set.seed(1234)

# Randomly generating our training and test sampels with a respective ratio of 2/3 and 1/3
datasample <- sample(2, nrow(results.norm), replace = TRUE, prob = c(0.6, 0.4))

# Generate training set
results.training <- results.norm[datasample == 1,]
View(results.training)

# Generate test set 
results.test <- results.norm[datasample == 2,]
View(results.test)
# Generate training labels
resultsTraining.labels <- merged_avg[datasample == 1, 2]
nrow(resultsTraining.labels)
cl <- factor(c(1,2,3,2))

# Generate test labels
resultsTest.labels <- merged_avg[datasample == 2, 2]
resultsTest.labels<- factor(c(3,2,1,3,3,1))
results.knn <- knn(train = results.training, test = results.test, cl, k = 3)

# creating a dataframe from known (true) test labels
test.labels <- data.frame(resultsTest.labels)

# combining predicted and known species classes
class.comparison <- data.frame(results.knn, test.labels)

# giving appropriate column names
names(class.comparison) <- c("Predicted Species", "Observed Species")
class.comparison


## Random Forest to predict habitat on avg dataset ----
merged_cleaner<- subset(merged_habitats, select = -c(richness))
merged_cleaner$habitat<- as.factor(merged_cleaner$habitat)
merged_avg<- merged_cleaner %>%
  group_by(site, habitat)%>%
  summarise_all(list(mean=mean,
                     sd = sd)) %>%
  ungroup()

merged_avg<- subset(merged_avg, select = -c(site))
merged_avg$habitat.y<- as.factor(merged_avg$habitat.y)
merged_avg<- subset(merged_avg, select = -c(site))
merged_avg$habitat<- as.factor(merged_avg$habitat)
str(merged_avg)
#training
library(randomForest)
library(caret)

# training
set.seed(222)
ind <- sample(2, nrow(results_clean), replace = TRUE, prob = c(0.6, 0.4))
train <- results_clean[ind==1,]
str(train)#65 values 
test <- results_clean[ind==2,] # 131 values

View(train)

rf_test<- randomForest(habitat~., data= results_clean, importance= TRUE, proximity= TRUE)
print(rf_test)
plot(rf_test)

p1 <- predict(rf_test, train)
confusionMatrix(p1, train$ habitat)

p2 <- predict(rf_test, test)
confusionMatrix(p2, test$ habitat)
# p value = 0.2963

#Variable Importance
varImpPlot(rf_test,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf_test)

# adjust the model? Not sure what this does
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

# not super sure what this does but lets see
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
#what do the nodes mean?

## Random Forest for richness with all the data ----
str(merged)
merged_habitats<- subset(merged, select = c(1, 4:24,27,51,57))
str(merged_habitats)
merged_habitats$richness<- as.factor(merged_habitats$richness)
## Split data into habitats

#Data frame for habitat category 1
df_habitat_1 <- merged_habitats %>%
  filter(habitat == 1)%>%
  select(-c(habitat, site))
View(df_habitat_1)
df_habitat_1$richness<- as.factor(df_habitat_1$richness)

# training
set.seed(222)
ind <- sample(2, nrow(df_habitat_1), replace = TRUE, prob = c(0.6, 0.4))
train <- df_habitat_1[ind==1,]
test <- df_habitat_1[ind==2,] 


rf_test<- randomForest(richness~., data= df_habitat_1, importance= TRUE, proximity= TRUE, mtry = 3)
print(rf_test)
plot(rf_test)

p1 <- predict(rf_test, train)
confusionMatrix(p1, train$ richness)

p2 <- predict(rf_test, test)
confusionMatrix(p2, test$ richness)
# p value = 0.2963

#Variable Importance
varImpPlot(rf_test,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf_test)


## Random forest for habitat with all data ----
str(merged)
classify<- subset(merged, select = c(3:24,57))
classify$habitat<- as.factor(classify$habitat)
classify<- subset(classify, select= -c(habitat.y))
str(classify)

# training
set.seed(222)
ind_class <- sample(2, nrow(results_clean), replace = TRUE, prob = c(0.6, 0.4))
train_class <- results_clean[ind_class==1,]
test_class <- results_clean[ind_class==2,] 

View(train)
View(classify)

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
?varImpPlot

install.packages("pdp")
library(pdp)
pdp_plot<- partial(rf_test_class, pred.var= "SE_high")
pdp_data <- as.data.frame(pdp_plot)
(sehigh<- ggplot(pdp_data, aes(x = SE_high, y = yhat)) +
  geom_line(color = "green") +
  labs(x = "SE High", y = "Partial Dependence") +
  #ggtitle("Partial Dependence Plot for X1") +
  theme_classic() +
    theme( axis.text = element_text(size = 12),
           axis.title = element_text(size = 18),
           axis.line = element_line(size = 1.5))
)
library(gridExtra)
grid.arrange(sehigh, ndsi, mhigh, hhigh, ncol = 2)

# Create a data frame from the PDP results
pdp_data <- as.data.frame(pdp_plot)

# Plot the line graph using ggplot2
ggplot(pdp_data, aes(x = M_high, y = yhat)) +
  geom_line(color = "blue") +
  #geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "lightblue", alpha = 0.3) +
  labs(x = "X1", y = "Partial Dependence") +
  ggtitle("Partial Dependence Plot for X1") +
  theme_classic()

##----
#----
#Scaled rf ----
scaled_data<- scale(results_clean[,-c(23)])
str(scaled_data)


#----
#----

## RF ON Habitat 1 ----
str(merged)
merged_habitats<- subset(merged, select = c(1, 3:24,26, 52))
str(merged_habitats)
merged_habitats$richness<- as.factor(merged_habitats$richness)
merged_habitats$habitat<- as.factor(merged_habitats$habitat.x)
merged_habitats<- subset(merged_habitats, select = -c(habitat.x))
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
train2
train2 <- na.omit(train2)
test2 <- df_habitat_2[ind2==2,] 
test2<- na.omit(test2)

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
