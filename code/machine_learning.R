# Machine Learning Techniques
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
ind <- sample(2, nrow(merged_avg), replace = TRUE, prob = c(0.6, 0.4))
train <- merged_avg[ind==1,]
str(train)#65 values 
test <- merged_avg[ind==2,] # 131 values

View(train)

rf_test<- randomForest(habitat~., data= merged_avg, importance= TRUE, proximity= TRUE)
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
str(classify)

# training
set.seed(222)
ind_class <- sample(2, nrow(classify), replace = TRUE, prob = c(0.4, 0.6))
train_class <- classify[ind_class==1,]
test_class <- classify[ind_class==2,] 

View(train)
View(classify)

rf_test_class<- randomForest(habitat~., data= classify, importance= TRUE, proximity= TRUE)
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
           n.var = 12,
           main = "Variable Importance")

partialPlot(rf_test_class, classify, M_high)

classify$M_high<- unlist(classify$M_high)
classify$habitat<- unlist(classify$habitat)
