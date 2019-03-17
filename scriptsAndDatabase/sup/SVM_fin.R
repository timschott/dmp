### SVM "REAL TALK" FINAL
### ft. inital total LOOCV into a 90-10 (final) LOOCV and a PLOT
### GOING TO BE TOUGH TO GET THIS GRAPH TO WORK 
### ALLONS-Y
setwd("~/Documents/7thSemester/dmp/corpus")

library(scales)
library("dplyr")
library(cvTools)
library(caret)
library(pROC)
library(e1071)

big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)

normalized_df <- read.csv('particularly_normalized.csv', stringsAsFactors = FALSE)
colnames(normalized_df)
str(normalized_df)

# remove identifying characteristics in a duplicate

normalized_df <- normalized_df %>% dplyr::select(-c(X))
normalized_df$label2 <- as.factor(normalized_df$label2)

# reduce down to important variables 
# I'm going to start with a "full" model
# and then reduce down based on the most consequential vars
# I'm not going to include Dialogue in this. 
# because my decision tree work shows that it's kind of fraught

culled <- normalized_df %>% dplyr::select(c(consecutive_repeat_freq_vec,
                                            i_freq, perceive_freq, label2))
# C-Value grid (the "cost" parameter that sets how flexible your classifier is)

grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 5, 6.25, 7.5, 10, 12.5 ,15 ,17.5,20))

set.seed(11)
sv_count <- c(0)

# LOOCV
for(i in (seq(1,18))){
  fit <- svm(label2 ~., data = culled, kernel = "linear",
             cross=50,
             cost = grid$C[i])
  print(paste(grid$C[i], length(fit$SV)/3))
  sv_count <- append(sv_count, length(fit$SV)/3)
}
sv_count <- sv_count[-c(1)]

plot(grid$C, sv_count, xlab="cost", ylab="support vector count", main="SVM Cost Tuning", type="p")
# best without overfitting is around that elbow, with C = 2.5, 22 support vecs
# let's look at the weights and select our variables. 
# overall we do not see *good* performance with this model. 

# math.

w = t(fit$coefs) %*% fit$SV
# the weights: 
# anaphora is strongly pulling it towards lyrical
# perception is strongly pulling it towards detective fictions.
# anaphora: 0.7237152
# i_freq: -0.3074419
# perceive_freq: -1.345788

# so we're going to work with anaphora and perception 
sub <- culled %>% dplyr::select(consecutive_repeat_freq_vec, perceive_freq,label2)

set.seed(22)
sv_count <- c(0)

# 50 fold ie LOOCV to use all my data
# C = 2.5 
folds <- cvFolds(NROW(sub), K=50)
results <- c(0)
names <- c(0)
for(i in seq(1, 50)){
  # run LOOCV each time 50 times and see what the average accuracy is. 
  
  train_temp <- sub[folds$subsets[folds$which != i], ] #Set the training set

  test_temp <- sub[folds$subsets[folds$which == i], ] 

  temp_fit <- svm(label2 ~., data = train_temp, kernel = "linear",
                 cost = 1)
  test_grid_temp <- predict(temp_fit, newdata = test_temp)

  mat <- confusionMatrix(test_grid_temp, test_temp$label2)
  if(mat$overall[1] != 1){
    print(i)
    names <- append(names, big_boy[i,1])
  }
  results<-append(results, mat$overall[1])
}
# plot
results <- results[-c(1)]
sum(results)/50

# with 50 fold cross validation I achieved 84% accuracy. 
# that's 8 incorrect predictions out of 50. 

# here's the test samples that stymied it: 
names <- names[-c(1)]

# [1] "billyBudd"            "eureka"               "heartOfDarkness"     
# [4] "theSoundAndTheFury"   "wideSargassoSea"      "theBigSleep"         
# [7] "theCircularStaircase" "theMysteryOfRoom75"  

fin_fit <- svm(label2 ~., data = sub, kernel = "linear",
           cost = 1)

plot(fin_fit, sub, svSymbol="x", dataSymbol="o", color.palette=terrain.colors, symbolPallete=rainbow(1), scale = FALSE)

# we can now look closely at the support vectors
# particularly those that are closest to the margin. 

w = t(fin_fit$coefs) %*% fin_fit$SV


# i think i am still fundamentally confused about 
# how the cross validation makes the model "better"

# at 84% percent right now but with lots of support vex.... 


