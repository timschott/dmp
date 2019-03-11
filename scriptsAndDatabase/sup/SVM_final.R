## SVM_FINAL
setwd("~/Documents/7thSemester/dmp/corpus")
library(scales)
library("RSQLite")
library("dplyr")
library(caret)
library(pROC)
library(ggcorrplot)
library(randomForest)
library(MASS)
library(randomForest)
library(kernlab)
library(e1071)
# let's look at what data is correlated. and then normalize our feature set. 

# load in csv
normalized_df <- read.csv('particularly_normalized.csv', stringsAsFactors = FALSE)
colnames(normalized_df)
str(normalized_df)

# remove identifying characteristics in a duplicate

normalized_df <- normalized_df %>% dplyr::select(-c(X))
normalized_df$label2 <- as.factor(normalized_df$label2)

# reduce down to important variables 
culled <- normalized_df %>% dplyr::select(c(i_freq, dialogue_freq,
                                            consecutive_repeat_freq_vec,
                                            self_freq, perceive_freq, label2))

grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5,10, 15, 20, 300))

# free ..25 savage ?
set.seed(25)

for(i in (seq(1,16))){
  fit <- svm(label2 ~., data = culled, kernel = "linear",
             cross=50,
             cost = grid$C[i])
  print(length(fit$SV)/5)
}

fit <- svm(label2 ~., data = culled, kernel = "linear",
           cross=50,
           cost = 15)


w = t(fit$coefs) %*% fit$SV
sort(w)
# alright so the most consequential are dialgoue and anaphora
sub <- culled %>% dplyr::select(dialogue_freq, consecutive_repeat_freq_vec,label2)
# sub_test <- testing %>% dplyr::select(dialogue_freq, consecutive_repeat_freq_vec,label2)

for(i in seq(1,16)){
test_svm <- fit <- svm(label2 ~., data = sub, kernel = "linear",
                       cross=50,
                       cost = grid$C[i])
print(length(fit$SV)/2)
}

smaller_svm <- svm(label2 ~., data = sub, kernel = "linear",
           cross=50,
           cost = 5)
# parameter coefficients
small_w = t(smaller_svm$coefs) %*% smaller_svm$SV

# plot

plot(smaller_svm, sub, svSymbol="x")

# confusion matrix for subset 

# manual coefficient calc. 
t(small_w %*% t(as.matrix(sub[,-3]))) - smaller_svm$rho
# for one row
t(small_w %*% t(as.matrix(sub[1,-3]))) - smaller_svm$rho[1]
b <- -1 * smaller_svm$rho
(small_w %*% (as.matrix(sub[,-3])) + b) / sqrt(small_w %*% t(small_w))

(small_w %*% sub[1,-3]) + b

# old 
## We can also use ROC curves.  How is the ROC curve created?
# the second column is P(A).
RC <- roc(sub$label2,test_pred_grid)
plot(RC, legacy.axes = TRUE)

tuned_svm <- tune(svm, label2~., data = culled,
                   kernel="linear", range=list(cost=10^(-2:2), gamma=c(0.1, 0.25,0.5,0.75,1,2)),
                   tunecontrol = tune.control(cross =50), probability=TRUE)
tuned_svm$best.model
tuned_svm$best.performance







