## SVM_FINAL
setwd("~/Documents/7thSemester/dmp/corpus")
library(scales)
library("RSQLite")
library("dplyr")
library(caret)
library(ggcorrplot)
library(randomForest)
library(MASS)
library(randomForest)
library(kernlab)
library(e1071)
# let's look at what data is correlated. and then normalize our feature set. 

# load in csv
normalized_df <- read.csv('normalized.csv', stringsAsFactors = FALSE)
colnames(normalized_df)
str(normalized_df)

# remove identifying characteristics in a duplicate

normalized_df <- normalized_df %>% dplyr::select(-c(X))
normalized_df$label2 <- as.factor(normalized_df$label2)

# reduce down to important variables 
culled <- normalized_df %>% dplyr::select(c(i_freq, dialogue_freq,
                                            consecutive_repeat_freq_vec,
                                            self_freq, perceive_freq, label2))
# free 21 savage
set.seed(25)

partition <- createDataPartition(y = culled$label2, p= 0.80, list = FALSE)
training <- culled[partition,]
testing <- culled[-partition,]

trctrl <- trainControl(method="LOOCV")
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(label2 ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         tuneGrid = grid,
                         tuneLength = 5)
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)

confusionMatrix(test_pred_grid, testing$label2)

test_pred_grid
testing$label2
testing$i_freq

normalized_df$i_freq




library(gmodels)
CrossTable(testing$label2, test_pred_grid,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual label', 'predicted label'))

## We can also use ROC curves.  How is the ROC curve created?
# the second column is P(A).
RC <- roc(testing$label2,test_pred_grid)
plot(RC, legacy.axes = TRUE)

tuned_svm <- tune(svm, label2~., data = culled,
                   kernel="linear", range=list(cost=10^(-2:2), gamma=c(0.1, 0.25,0.5,0.75,1,2)),
                   tunecontrol = tune.control(cross =50), probability=TRUE)
tuned_svm$best.model
tuned_svm$best.performance

# https://datascienceplus.com/understanding-linear-svm-with-r/
print(tuned_svm)
svm_good_model<-svm(label2~., data=training, kernel="linear",cost=tuned_svm$best.parameters$cost, gamma=tuned_svm$best.parameters$gamma, probability=TRUE)
y_pred_prob<-predict(svm_good_model,newdata = testing, probability=TRUE)
table(testing$label2,y_pred_prob)
attr(y_pred_prob, "probabilities")[,2]
library(gmodels)
CrossTable(y_pred,testing$label2,prop.chisq = FALSE)

library(pROC)
r =roc(testing$label2, attr(y_pred_prob, "probabilities")[,2])
plot(r)
