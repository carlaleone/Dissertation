# Dissertation Modeling Analysis
# 12/03/2024
# Carla Leone

## Load the data----
library(readxl)
library(tidyverse)
indices <- read_excel("data/results.clean.xlsx", 
                      sheet = "matched_times")
results_clean <- read_excel("data/results.clean.xlsx", 
                            sheet = "results.clean")
str(results_clean)
results_clean$habitat<- as.factor(results_clean$habitat)
results_clean <- subset (results_clean, select = c(2:23, 26))
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
View(merged)                
str(merged)

library(openxlsx)
write.xlsx(merged, "merged.xlsx")
### Data Exploration ----
library(lme4)
hist(merged$richness)
lm1<- lm(low_richness~habitat, data=merged)
kw_lwrich<- kruskal.test(low_richness~habitat, data=merged)
kw_lwrich
kw_sunrise<- kruskal.test(low_richness~sunrise, data=merged)
kw_sunrise


lme1<- lmer(richness~ACI_low+ ACI_high +ACI_full +AEI_low +AEI_high +AEI_full+ BI_low +BI_high+ BI_full+ H_low+ H_high+ H_full+ TE_low+ TE_high+ TE_full +SE_low+ SE_high+ SE_full  +M_low+ M_high+ M_full+ NDSI + (1|site), data= merged )
print(colnames(merged))

avg_richness<- merged %>%
  group_by(habitat.y) %>%
  summarise(avg_richness = mean(richness),
            sd = sd(richness))%>%
  ungroup()
View(avg_richness)
boxplot(merged$low_richness~merged$habitat)
boxplot(meta_richness$low_richness~meta_richness$habitat)
boxplot(merged$low_richness...26~merged$habitat.y)
boxplot(merged$richness~merged$habitat.y)
merged$wind<- as.factor(merged$wind)
## Random Forest Attempt----
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)
library(readxl)
library(tidyverse)
library(vegan)
library(datasets)
data<- iris
View(data)

str(merged)
library(readxl)
merged3 <- read_excel("data/merged.xlsx", 
                     sheet = "merged3")
View(merged3)
#training
set.seed(222)
ind <- sample(2, nrow(merged_habitats), replace = TRUE, prob = c(0.7, 0.3))
train <- merged_habitats[ind==1,] #65 values 
train
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
?tuneRF

# not super sure what this does but lets see
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

#Variable Importance
varImpPlot(rf1,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

MDSplot(rf, train$richness)

library(corrplot)
?corrplot
corrplot(cor(matrix_hab1_numeric))
cor(matrix_hab1)
matrix_hab1<- as.matrix(df_habitat_1)
matrix_hab1<- subset(matrix_hab1, select = -c(richness))
str(matrix_hab1)
View(matrix_hab1_numeric)
column_names <- matrix_hab1[1, ]

matrix_hab1_numeric <- as.numeric(matrix_hab1)
matrix_hab1 <- matrix_hab1[-1, ]
colnames(matrix_hab1) <- column_names

### Try forest with low richness ----
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
varImpPlot(rf1,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf1)
MDSplot(rf1, train1$richness)
# checking correlations between the different variables
View(merged_habitats)
cor_habitats<- subset(merged_habitats, select = -c(site, habitat.y))
library(corrplot)
correlation_matrix <- cor(cor_habitats[, -ncol(merged_habitats)], df$richness)
print(correlation_matrix)
corrplot(correlation_matrix)

# Data frame for habitat category 2
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

# adjust the model because clearly has a lot of error
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

# not super sure what this does but lets see
hist(treesize(rf2),
     main = "No. of Nodes for the Trees",
     col = "green")

#Variable Importance
varImpPlot(rf2,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf2)
 str(merged_habitats)
 #trying with the top important variables
df_habitat_2.2 <- merged_habitats %>%
  filter(habitat == 2) %>%
  select(-c(1, 2:8,10:12, 14:18, 20, 24,))
df_habitat_2.2$richness<- as.factor(df_habitat_2.2$richness)
View(df_habitat_2.2)
# training
set.seed(222)
ind2 <- sample(2, nrow(df_habitat_2.2), replace = TRUE, prob = c(0.7, 0.3))
train2 <- df_habitat_2.2[ind2==1,]  
test2 <- df_habitat_2.2[ind2==2,] 

rf.2.2<- randomForest(richness~., data=train2, proximity=TRUE) 
print(rf.2.2)
MDSplot(rf2, train2$richness)
View(merged_habitats)
# Data frame for habitat category 3
df_habitat_1 <- merged_habitats %>%
  filter(habitat == 1)%>%
  select(-c(habitat, site))
df_habitat_1$richness<- as.factor(df_habitat_1$richness)
View(df_habitat_1)

is.na(df_habitat_1$richness)
any(is.na(train1$richness))
levels(train1$richness)
table(df_habitat_1$richness)
# training
set.seed(222)
ind1 <- sample(2, nrow(df_habitat_1), replace = TRUE, prob = c(0.6, 0.4))
train1 <- df_habitat_1[ind1==1,]  
test1<- df_habitat_1[ind1==2,] 


#random forest
rf1 <- randomForest(richness~., data=train1, proximity=TRUE, mtry= 4) 
print(rf1)

p1<- predict(rf3, train3)
confusionMatrix(p1.3, train3$ richness)

p2<- predict(rf1, test1)
confusionMatrix(p2, test1$ richness)

# adjust the model because clearly has a lot of error
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

# not super sure what this does but lets see
hist(treesize(rf2),
     main = "No. of Nodes for the Trees",
     col = "green")

#Variable Importance
varImpPlot(rf3,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf2)

MDSplot(rf2, train2$richness)


df_habitat_1 <- merged_habitats %>%
  filter(habitat.y == 1)%>%
  select(-c(habitat.y, site))
df_habitat_3$richness<- as.factor(df_habitat_3$richness)
View(df_habitat_1)

# training
set.seed(222)
ind3 <- sample(2, nrow(df_habitat_3), replace = TRUE, prob = c(0.6, 0.4))
train3 <- df_habitat_3[ind3==1,]  
test3 <- df_habitat_3[ind3==2,] 


#random forest
rf3 <- randomForest(richness~., data=train3, proximity=TRUE, mtry= 4) 
print(rf3)

p1.3<- predict(rf3, train3)
confusionMatrix(p1.3, train3$ richness)

p2.3<- predict(rf3, test3)
confusionMatrix(p2.3, test3$ richness)
#193 observations of 67 variables----
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


### RF with Habitats ----

merged_cleaner<- subset(merged_habitats, select = -c(richness))
merged_cleaner$habitat<- as.factor(merged_cleaner$habitat)
merged_avg<- merged_cleaner %>%
  group_by(site, habitat)%>%
  summarise_all(list(mean=mean)) %>%
  ungroup()

merged_avg<- subset(merged_avg, select = -c(site))
merged_avg$habitat.y<- as.factor(merged_avg$habitat.y)
str(merged_avg)
#training
library(randomForest)
library(caret)
set.seed(222)
ind <- sample(2, nrow(merged_avg), replace = TRUE, prob = c(0.8, 0.2))
train <- merged_avg[ind==1,] #65 values 
test <- merged_avg[ind==2,] # 131 values


levels(train$habitat.y)
set.seed(71)
rf_test<- randomForest(habitat.y~., data= merged_avg, importance= TRUE, proximity= TRUE)
print(rf_test)
#random forest
?randomForest
rf <- randomForest(habitat.y~., data=train, proximity=TRUE, ntree=1000, mtry= 2) 
print(rf)
plot(rf)

p1 <- predict(rf, train)
confusionMatrix(p1, train$ habitat.y)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ habitat.y)


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

# ACI Low----
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
### Looking at all indices ----
library(readxl)
results_clean <- read_excel("data/results.clean.xlsx", 
                            sheet = "results.clean")
View(results_clean)
str(results_clean)
results_clean2<- subset(results_clean, select = c(2:24,26))
results_clean2$site<- tolower(gsub(" ", "_", results_clean2$site))
results_clean2$habitat<- as.factor(results_clean2$habitat)
View(results_clean2)
results_clean2<- results_clean2 %>%
  group_by(site,habitat)%>%
  summarise_all(list(mean=mean))%>%
  ungroup()

results_clean2<- results_clean2 %>%
  column_to_rownames(var = "site")

dist_indices<- vegdist(results_clean2, method="bray")
cluster_indices<- hclust(dist_indices, method = "average", members = NULL)
plot(cluster_indices)

pca_indices<- prcomp(results_clean2, scale=TRUE)
summary(pca_indices)
screeplot(pca_indices)
pca_indices
biplot(pca_indices)


                   
### Richness vs time ----
str(merged)
time<- subset(merged3, select=c(sunrise, time, richness, sunrise_time, site, habitat, day, boat, weather, wind))
View(time)
time$habitat<- as.factor(time$habitat)
time$wind<- as.numeric(time$wind)

#wind
wind<-  time %>%
  group_by(site, wind, habitat, weather) %>%
  summarize(mean = mean(richness)) %>%
  ungroup()
View(wind)
plot(wind$mean~wind$wind)
summary(lm(mean~wind, data = wind))

#Weather
weather<- time %>%
  group_by(site, weather) %>%
  summarize(mean = mean(richness)) %>%
  ungroup()
View(weather)
boxplot(weather$mean~weather$weather)
kruskal.test(mean~weather, data = weather)
#linear plot
ggplot(time, aes(x = time, y = richness, color = site)) +
geom_smooth(se=F) +
  geom_point(data = time, aes(x = sunrise_time, y = NA), shape = 17, color = "black", size = 3) +
  labs(x = "Time", y = "Full Max Richness", color = "Habitat") +
  facet_wrap(~habitat)+
  theme_minimal()

sunrise_data <- subset(time, sunrise == "yes")
View(sunrise_data)
only_dawn<- sunrise_data%>%
  group_by(site, habitat) %>%
  summarise(mean = mean(richness),
            sd = sd(richness))%>%
  ungroup()
View(only_dawn)
boxplot(only_dawn$mean~only_dawn$habitat)
kw_onlydawn<- kruskal.test(mean~habitat, data= only_dawn)
kw_onlydawn

ggplot() +
  geom_smooth(data = time, aes(x = time, y = richness, color = site), se = FALSE) +
  geom_point(data = sunrise_data, aes(x = time, y = richness), shape = 17, color = "black", size = 3) +
  labs(x = "Time", y = "Full Max Richness", color = "Site") +
  facet_wrap(~habitat) +
  theme_minimal()

# Plot
# Summarize data to calculate mean sunrise time per site
summarized_df <- time %>%
  group_by(site) %>%
  summarize(mean_sunrise_time = mean(sunrise_time)) %>%
  ungroup()
View(summarized_df)
# Plot
ggplot(time, aes(x = time, y = richness, color = site)) +
  geom_line() +
  geom_point(data = summarized_df, aes(x = sunrises, y = richness_time), color = "black", size = 3)
  

sunrise_time<- c("1900-01-01 05:00:00", "1900-01-01 06:30:00", "1900-01-01 04:30:00", "1900-01-01 05:30:00", "1900-01-01 04:30:00", "1900-01-01 06:00:00", "1900-01-01 04:30:00", "1900-01-01 04:30:00", "1900-01-01 05:00:00", "1900-01-01 06:30:00")
summarized_df$sunrises<- posix_time
str(summarized_df)
posix_time <- as.POSIXct(sunrise_time, format = "%Y-%m-%d %H:%M:%S")
summarized_df$richness_time<- c(6,4,6,5,5,6,5,6,6,5)
summarized_df<- subset(summarized_df, select = c(sunrises, richness_time))
#day night and dawn
danw_data<- time%>%
  group_by(site, day) %>%
  summarise(mean = mean(richness),
            sd = sd(richness))%>%
  ungroup()
View(danw_data)
boxplot(danw_data$mean~danw_data$day)

kruskal.test(mean~day, data=danw_data)
