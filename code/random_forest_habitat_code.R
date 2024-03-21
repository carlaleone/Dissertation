# Carla Leone
# 20/03/2024
# Random Forest Code for Dissertation

## merged_full code----
View(merged_full)
merged_full$habitat.y<- as.factor(merged_full$habitat.y)

#training
set.seed(222)
ind <- sample(2, nrow(merged_full), replace = TRUE, prob = c(0.7, 0.3))
train <- merged_full[ind==1,]  
test <- merged_full[ind==2,] 

#random forest
rf <- randomForest(habitat.y~., data=train, proximity=TRUE) 
print(rf)
plot(rf)

p1 <- predict(rf, train)
confusionMatrix(p1, train$ revised_habitat)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ revised_habitat)


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
# M High, M full, SE high, SE full, H full, H high, BI high

MDSplot(rf, train$revised_habitat)

## merged_habitat code ----
merged_habitats$habitat.y<- as.factor(merged_habitats$habitat.y)

#training
set.seed(222)
ind_2 <- sample(2, nrow(merged_habitats), replace = TRUE, prob = c(0.7, 0.3))
train_2 <- merged_habitats[ind==1,] #65 values 
test_2 <- merged_habitats[ind==2,] # 131 values

#random forest
rf_2 <- randomForest(habitat.y~., data=train, proximity=TRUE) 
print(rf_2)
plot(rf_2)

p1_2 <- predict(rf_2, train_2)
confusionMatrix(p1_2, train_2$ habitat.y)

p2_2 <- predict(rf_2, test_2)
confusionMatrix(p2_2, test_2$ habitat.y)

varImpPlot(rf_2,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rf_2)

