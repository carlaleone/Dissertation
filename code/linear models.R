# Dissertation Modeling Analysis
# 12/03/2024
# Carla Leone

## Load the data----
indices <- read_excel("data/results.clean.xlsx", 
                      sheet = "matched_times")
phonic <- read_excel("data/meta_richness.xlsx", 
                            sheet = "big_sheet (2)")

View(indices)
indices<- subset(indices, select = -c(long, lat) ) #remove unnecessary columns

# merge the datasets
indices <- indices %>%
  rename(minute = recording_minute)
merged <- merge(indices, phonic, by = c("site", "minute"))
View(merged)                
str(merged)
## Random Forest Attempt----
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)

library(datasets)
data<- iris
View(data)

str(merged)
#193 observations of 67 variables
merged2<- subset(merged, select = c(1, 4:25, 51))
View(merged2)
merged2<- merged2 %>%
  column_to_rownames(var = "site")

# dataset with only richness and the indices
merged3<- subset(merged2, select = -c(site))
merged3$richness<- as.factor(merged3$richness)
View(merged3)

#training
set.seed(222)
ind <- sample(2, nrow(merged3), replace = TRUE, prob = c(0.7, 0.3))
train <- merged3[ind==1,] #65 values 
test <- merged3[ind==2,] # 131 values

#random forest
rf <- randomForest(richness~., data=train, proximity=TRUE) 
print(rf)

p1 <- predict(rf, train)
confusionMatrix(p1, train$ richness)

# adjust the model because clearly has a lot of error
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

#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

MDSplot(rf, train$richness)

### Try forest with low richness ----
#193 observations of 67 variables
merged7<- subset(merged, select = c(4:25, 52))
View(merged7)

# dataset with only richness and the indices
merged7$low_richness<- as.factor(merged7$low_richness)
View(merged3)

#training
set.seed(222)
ind <- sample(2, nrow(merged7), replace = TRUE, prob = c(0.7, 0.3))
train <- merged7[ind==1,] #65 values 
test <- merged7[ind==2,] # 131 values

#random forest
rf <- randomForest(low_richness~., data=train, proximity=TRUE) 
print(rf)

p1 <- predict(rf, train)
confusionMatrix(p1, train$ richness)

# adjust the model because clearly has a lot of error
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

#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

MDSplot(rf, train$richness)



## Try the random forest plot to predict simpsons instead of richness?----
merged4<- subset(merged, select = c( 4:25, 59))
merged4$habitat.y<- as.factor(merged4$habitat.y)
View(merged4)

#training
set.seed(222)
ind <- sample(2, nrow(merged4), replace = TRUE, prob = c(0.7, 0.3))
train <- merged4[ind==1,] #65 values 
test <- merged4[ind==2,] # 131 values

#random forest
rf <- randomForest(habitat.y~., data=train, proximity=TRUE) 
print(rf)
plot(rf)

p1 <- predict(rf, train)
confusionMatrix(p1, train$ habitat.y)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ habitat.y)
# confusion matrix results:
# accuracy = 0.968, p value < 0.01

# adjust the model because clearly has a lot of error
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

#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)
# important variables:
# M High, SE high, H high,SE full, H full, AEI High, M full
MDSplot(rf, train$richness)

### Try it with the new habitats----
str(merged)
merged5<- subset(merged, select = c(1, 4:25, 60))
merged5$habitat.y<- as.factor(merged5$habitat.y)
View(merged5)

#training
set.seed(222)
ind <- sample(2, nrow(merged5), replace = TRUE, prob = c(0.7, 0.3))
train <- merged5[ind==1,] #65 values 
test <- merged5[ind==2,] # 131 values

#random forest
rf <- randomForest(revised_habitat~., data=train, proximity=TRUE) 
model <- randomForest(revised_habitat ~ . + site, data = train, importance = TRUE)
print(model)
print(rf)
plot(rf)


p3<-  predict(model, train)
confusionMatrix(p1, train$ revised_habitat)

p4<- predict(model, test)
confusionMatrix(p2, test$ revised_habitat)

p1 <- predict(rf, train)
confusionMatrix(p1, train$ revised_habitat)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ revised_habitat)
# confusion matrix results:
# accuracy = 1, p value < 0.01

# adjust the model because clearly has a lot of error
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

#Variable Importance
varImpPlot(model,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(model)
# important variables:
# M High, M full, SE high, SE full, H full, H high, BI high

MDSplot(rf, train$revised_habitat)


### Looking at random forest for time variation ----
str(merged)

# code for summarizing simpsons, may be useful at some point
merged_habitats<-  merged %>%
  subset(select= c(1, 4:25, 60)) %>%
  group_by(site,habitat.y) %>%
  summarize(across(2:21, mean))%>%
  ungroup()
View(merged_habitats)
merged_habitats<- subset(merged_habitats, select= -c(1))
merged_habitats$habitat.y<- as.factor(merged_habitats$habitat.y)

#save the excel 
install.packages("openxlsx")
library(openxlsx)
write.xlsx(merged_habitats, "merged_habitats.xlsx")
write.xlsx(merged5, "merged_full.xlsx")

## Now run the forest model
merged6<- subset(merged, select = c( 4:25, 30))
merged6$time<- as.factor(merged6$time)
View(merged6)

#training
set.seed(222)
ind <- sample(2, nrow(merged_habitats), replace = TRUE, prob = c(0.7, 0.3))
train <- merged_habitats[ind==1,] #65 values 
test <- merged_habitats[ind==2,] # 131 values

#random forest
rf <- randomForest(habitat.y~., data=train, proximity=TRUE) 
print(rf)
plot(rf)

p1 <- predict(rf, train)
confusionMatrix(p1, train$ habitat.y)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ habitat.y)
# confusion matrix results:
# accuracy = 1, p value < 0.01

# adjust the model because clearly has a lot of error
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)
# not super sure what this does but lets see
## DOES NOT HELP PREDICT TIME

### Try a PCA with the indices----
indices_pca_data<- subset(merged_simpsons, select = -c(2))
indices_pca_data<- indices_pca_data %>%
  column_to_rownames(var = "site") # make site the name of the rows

# try the PCA
pca_indices<- rda(indices_pca_data, scale=T)
plot(pca_indices)
summary(pca_indices)
#pca 1 heavy on all the SEs and Hs
#pca 2 is mainy just the AEI
biplot(pca_indices)
screeplot(pca_indices)
# is it even worth it to use PC2?

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