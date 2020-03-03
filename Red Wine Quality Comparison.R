#CLEARING THE ENVIRONMENT
rm(list = ls())

#IMPORTING THE DATA
getwd()
setwd('C:\\Users\\Shekhar Lamba\\Documents\\Datasets')
wine <- read.csv('winequality-red.csv')
head(wine)
View(wine)
str(wine)

#DATA PREPARATION
table(wine$quality)
wine$quality <- ifelse(wine$quality > 6, 'Good', 'Bad')
wine$quality <- factor(wine$quality)
summary(wine)

#EDA
my_stats <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  mini <- min(x)
  p1 <- quantile(x, 0.01)
  p3 <- quantile(x, 0.03)
  p5 <- quantile(x, 0.05)
  p10 <- quantile(x, 0.1)
  q1 <- quantile(x, 0.25)
  q2 <- quantile(x, 0.5)
  q3 <- quantile(x, 0.75)
  p90 <- quantile(x, 0.9)
  p95 <- quantile(x, 0.95)
  p97 <- quantile(x, 0.97)
  p99 <- quantile(x, 0.99)
  maxi <- max(x)
  lc <- m - (3 * s)
  uc <- m + (3 * s)
  outlier_flag_min <- mini < lc
  outlier_flag_max <- maxi > uc
  return(c(No_of_values = n, Mean = m, Std_dev = s, Outlier_Flag_Min = outlier_flag_min,
           LC = lc, Min = mini, P1 = p1, P3 = p3, P5 = p5, P10 = p10, Q1 = q1, Q2 = q2, Q3 = q3, P90 = p90, P95 = p95, P97 = p97, P99 = p99, 
           Max = maxi, UC = uc, Outlier_Flag_Max = outlier_flag_max))
  
}
num_var <- sapply(wine, is.numeric)
diag_stats <- t(data.frame(apply(wine[num_var], 2, my_stats)))
View(diag_stats)

#GRAPHICAL ANALYSIS
library(ggplot2)
library(GGally)
library(tidyverse)
ggpairs(wine, columns = c(1:11), lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.1)),
        upper = list(continuous = wrap("cor", size = 2.8)), title = 'Relationship Between Different Attributes') 
wine %>%
  gather(key = 'variable', value = 'value', c(1:4, 8:11)) %>%
  ggplot(aes(x = reorder(variable, value, FUN = 'median'), y = value, fill = variable)) +
  geom_boxplot(show.legend = F) +
  labs(x = element_blank(), y = element_blank(), title = 'Boxplot for Variables') +
  theme_bw() + coord_flip()
wine %>%
  gather(key = 'variable', value = 'value', c(6, 7)) %>%
  ggplot(aes(x = reorder(variable, value, FUN = 'median'), y = value, fill = variable)) +
  geom_boxplot(show.legend = F) +
  labs(x = element_blank(), y = element_blank(), title = 'Boxplot for Variable') +
  theme_bw() + coord_flip()
wine %>% 
  gather(key = 'variable', value = 'value', 5) %>%
  ggplot(aes(x = reorder(variable, value, FUN = 'median'), y = value, fill = variable)) +
  geom_boxplot(show.legend = F) +
  labs(x = element_blank(), y = element_blank(), title = 'Boxplot for Variables') +
  theme_bw() + coord_flip()

#OUTLIER TREATMENT
wine$density[wine$density < 0.99108468] <- 0.99108468
wine$pH[wine$pH < 2.84795380] <- 2.84795380
wine$fixed.acidity[wine$fixed.acidity > 13.5429262] <- 13.5429262
wine$volatile.acidity[wine$volatile.acidity > 1.0649996] <- 1.0649996
wine$citric.acid[wine$citric.acid > 0.8553790] <- 0.8553790
wine$residual.sugar[wine$residual.sugar > 6.7685897] <- 6.7685897
wine$chlorides[wine$chlorides > 0.2286624] <- 0.2286624
wine$free.sulfur.dioxide[wine$free.sulfur.dioxide > 47.2553927] <- 47.2553927
wine$total.sulfur.dioxide[wine$total.sulfur.dioxide > 145.1537658] <- 145.1537658
wine$density[wine$density > 1.0024087] <- 1.0024087
wine$pH[wine$pH > 3.7742726] <- 3.7742726
wine$sulphates[wine$sulphates > 1.1666698] <- 1.1666698
wine$alcohol[wine$alcohol > 13.6199859] <- 13.6199859

#MORE GRAPHICAL INSIGHTS
library(plyr)
ggplot(wine, aes(x = quality, fill = quality)) + geom_bar() + labs(title = 'Density Comparison of Good and Bad Quality Red Wine')
mean_attributes <- ddply(wine, 'quality', summarise, mean.f.a = mean(fixed.acidity), mean.v.a = mean(volatile.acidity),
                         mean.c.a = mean(citric.acid), mean.r.s = mean(residual.sugar), mean.c = mean(chlorides), mean.f.s.d = mean(free.sulfur.dioxide),
                         mean.t.s.d = mean(total.sulfur.dioxide), mean.d = mean(density), mean.p.h = mean(pH), mean.s = mean(sulphates), 
                         mean.a = mean(alcohol))
ggplot(wine, aes(x = fixed.acidity, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.f.a, color = quality), linetype = 'dashed') +
  labs(title = 'Comparing Good and Bad Quality Red Wine by fixed.acidity')
ggplot(wine, aes(x = volatile.acidity, color = quality)) + geom_density() +
  geom_vline(data = mean_attributes, aes(xintercept = mean.v.a, color = quality), linetype = 'dashed') +
  labs(title = 'Comparing Good and Bad Quality Red Wine by volatile.acidity')
ggplot(wine, aes(x = citric.acid, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.c.a, color = quality), linetype = "dashed") +
  labs(title = 'Comparing Good and Bad Quality Red Wine by citric.acid')
ggplot(wine, aes(x = residual.sugar, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.r.s, color = quality), linetype = "dashed") +
  labs(title = 'Comparing Good and Bad Quality Red Wine by residual.sugar')
ggplot(wine, aes(x = chlorides, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.c, color = quality), linetype = "dashed") +
  labs(title = 'Comparing Good and Bad Quality Red Wine by chlorides')
ggplot(wine, aes(x = free.sulfur.dioxide, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.f.s.d, color = quality), linetype = "dashed") +
  labs(title = 'Comparing Good and Bad Quality Red Wine by free.sulfur.dioxide')
ggplot(wine, aes(x = total.sulfur.dioxide, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.t.s.d, color = quality), linetype = "dashed") +
  labs(title = 'Comparing Good and Bad Quality Red Wine by total.sulfur.dioxide')
ggplot(wine, aes(x = density, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.d, color = quality), linetype = "dashed") +
  labs(title = 'Comparing Good and Bad Quality Red Wine by density')
ggplot(wine, aes(x = pH, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.p.h, color = quality), linetype = "dashed") +
  labs(title = 'Comparing Good and Bad Quality Red Wine by pH')
ggplot(wine, aes(x = sulphates, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.s, color = quality), linetype = "dashed") +
  labs(title = 'Comparing Good and Bad Quality Red Wine by sulphates')
ggplot(wine, aes(x = alcohol, color = quality)) + geom_density() + 
  geom_vline(data = mean_attributes, aes(xintercept = mean.a, color = quality), linetype = "dashed") +
  labs(title = 'Comparing Good and Bad Quality Red Wine by alcohol')

#DIVIDING THE DATASET INTO TRAIN AND TEST
set.seed(1234)
train_ind <- sample(1:nrow(wine), size = floor(.8 * nrow(wine)))
training_data <- wine[train_ind, ]
testing_data <- wine[-train_ind, ]

#KNN MODELLING_________________________________________________________________________________________________

#INITIAL KNN MODEL
library(caret)
library(ROSE)
set.seed(1234)
knn_model_init <- train(quality ~ ., data = training_data, method = 'knn', preProcess = c('center', 'scale'))
print(knn_model_init)
plot(knn_model_init)

#INITIAL KNN PREDICTION AND ACCURACY
knn_predict_init <- predict(knn_model_init, newdata = testing_data)
confusionMatrix(knn_predict_init, testing_data$quality, positive = 'Good') #Acc = 85.94%, Rec = 29.55%
roc.curve(testing_data$quality, knn_predict_init, plotit = T) #AUC = 0.622

#MODIFIED KNN MODEL
knn_control <- trainControl(method = 'repeatedcv', number = 10, repeats = 5)
knn_grid <- expand.grid(k = seq(3, 20, 2))
set.seed(1234)
knn_model_mod <- train(quality ~ ., data = training_data, method = 'knn', preProcess = c('center', 'scale'),
                       trControl = knn_control, tuneGrid = knn_grid)
print(knn_model_mod)
plot(knn_model_mod)

#FINAL KNN PREDICTION AND ACCURACY
knn_predict_final <- predict(knn_model_mod, newdata = testing_data)
confusionMatrix(knn_predict_final, testing_data$quality, positive = 'Good') #Acc = 87.81%, Rec = 29.55%
roc.curve(testing_data$quality, knn_predict_final, plotit = T) #AUC = 0.633

#DECISION TREE MODELLING_______________________________________________________________________________________

#INITIAL DECISION TREE MODEL
library(rpart)
library(rpart.plot)
set.seed(1234)
tree_model_init <- rpart(quality ~ ., data = training_data, method = 'class')
rpart.plot(tree_model_init, type = 3, extra = 1, cex = .6)

#INITIAL TREE PREDICTION AND ACCURACY
tree_predict_init <- predict(tree_model_init, newdata = testing_data, type = 'class')
confusionMatrix(tree_predict_init, testing_data$quality, positive = 'Good') #Acc = 88.12%, Rec = 36.36%
roc.curve(testing_data$quality, tree_predict_init, plotit = T) #AUC = 0.664

#MODIFIED DECISION TREE MODEL
tree_control <- rpart.control(minsplit = 15, maxdepth = 6, minbucket = 5, xval = 10)
set.seed(1234)
tree_model_mod <- rpart(quality ~ ., data = training_data, method = 'class', control = tree_control)
rpart.plot(tree_model_mod, type = 3, extra = 1, cex = .5)

#PRUNING THE TREE
printcp(tree_model_mod) #min xerror
plotcp(tree_model_mod)
set.seed(1234)
pruned_tree <- prune(tree_model_mod, cp = 0.020231)
rpart.plot(pruned_tree, type = 3, extra = 1, cex = .7)

#FINAL TREE PREDICTION AND ACCURACY
tree_predict_final <- predict(pruned_tree, newdata = testing_data, type = 'class')
confusionMatrix(tree_predict_final, testing_data$quality, positive = 'Good') #Acc = 88.75%, Rec = 43.18%
roc.curve(testing_data$quality, tree_predict_final, plotit = T) #AUC = 0.696

#RANDOM FOREST MODELLING_______________________________________________________________________________________

#INITIAL RF MODEL
rf_control <- trainControl(method = 'repeatedcv', number = 10, search = 'grid')
set.seed(1234)
rf_model_init <- train(quality ~ ., data = training_data, method = 'rf', metric = 'Accuracy',
                       trControl = rf_control, importance = T)
print(rf_model_init)
varImp(rf_model_init)

#INITIAL RF PREDICTION AND ACCURACY
rf_predict_init <- predict(rf_model_init, newdata = testing_data)
confusionMatrix(rf_predict_init, testing_data$quality, positive = 'Good') #Acc = 90%, Rec = 38.64%
roc.curve(testing_data$quality, rf_predict_init, plotit = T) #AUC = 0.684

#BEST MTRY
rf_grid <- expand.grid(.mtry = c(1:9))
set.seed(1234)
rf_mtry <- train(quality ~ ., data = training_data, method = 'rf', metric = 'Accuracy',
                 trControl = rf_control, tuneGrid = rf_grid, importance = T)
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry

#BEST NTREE
rf_grid <- expand.grid(.mtry = best_mtry)
ntree_values <- list()
for(i in c(200, 250, 300, 350, 400, 450, 500, 550, 600, 700, 800, 900, 1000, 2000)) {
  set.seed(1234)
  rf_ntree <- train(quality ~ ., data = training_data, method = 'rf', metric = 'Accuracy', 
                    trControl = rf_control, tuneGrid = rf_grid, importance = T, ntree = i)
  current_iter <- toString(i)
  ntree_values[[current_iter]] <- rf_ntree
}
results_ntree <- resamples(ntree_values)
summary(results_ntree) #500

#MODIFIED RF MODEL
set.seed(1234)
rf_model_mod <- train(quality ~ ., data = training_data, method = 'rf', metric = 'Accuracy',
                      trControl = rf_control, tuneGrid = rf_grid, ntree = 500, importance = T)
print(rf_model_mod)
varImp(rf_model_mod)

#FINAL RF PREDICTION AND ACCURACY
rf_predict_final <- predict(rf_model_mod, newdata = testing_data)
confusionMatrix(rf_predict_final, testing_data$quality, positive = 'Good') #Acc = 90.31%, Rec = 45.46%
roc.curve(testing_data$quality, rf_predict_final, plotit = T) #AUC = 0.715
