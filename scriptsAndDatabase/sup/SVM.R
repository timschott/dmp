## SVM
setwd("~/Documents/7thSemester/dmp/corpus")
library(scales)
library("RSQLite")
library("dplyr")
library(caret)
library(ggcorrplot)
library(randomForest)
library(MASS)
library(randomForest)
library(e1071)
# let's look at what data is correlated. and then normalize our feature set. 

# load in csv
normalized_df <- read.csv('normalized.csv', stringsAsFactors = FALSE)
colnames(normalized_df)
str(normalized_df)

# remove identifying characteristics in a duplicate

normalized_df <- normalized_df %>% dplyr::select(-c(X))
normalized_df$label2 <- as.factor(normalized_df$label2)
intrain <- createDataPartition(y = normalized_df$label2, p= 0.75, list = FALSE)
training <- normalized_df[intrain,]
testing <- normalized_df[-intrain,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(17)

svm_Linear <- train(label2 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    tuneLength = 10)

test_pred <- predict(svm_Linear, newdata = testing)
test_pred
confusionMatrix(test_pred, testing$label2)


# how hard of a C do we want?

grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(27)
svm_Linear_Grid <- train(label2 ~., data = training, method = "svmLinear",
                           trControl=trctrl,
                           tuneGrid = grid,
                           tuneLength = 10)
svm_Linear_Grid

plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)

confusionMatrix(test_pred_grid, testing$label2)

testing$word_counts_vec

test_pred_grid
testing$label2
testing$word_counts_vec

# initial SVM results: misclassifying Billy Budd and Eureka as detective.
# my guess is that has to do with their small # of words. let's try using
# my pruned feature set and see what we get. 

## dialogue freq, i_freq, consecutive repeat, self, perceive

culled <- normalized_df %>% dplyr::select(c(i_freq, dialogue_freq,
                                             consecutive_repeat_freq_vec,
                                             self_freq, perceive_freq, label2))
# free 21 savage
set.seed(21)

intrain <- createDataPartition(y = normalized_df$label2, p= 0.75, list = FALSE)
training <- normalized_df[intrain,]
testing <- normalized_df[-intrain,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

                                          
