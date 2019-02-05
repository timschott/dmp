## SVM
setwd("~/Documents/7thSemester/dmp/corpus")
library(scales)
library("RSQLite")
library("dplyr")
library(caret)
library(randomForest)

big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)
colnames(big_boy)
str(big_boy)
big_boy$numeric_labels_vec[27:50] <- 0
novel_df <- big_boy %>% select(-c(labels_vec, titles_vec, numeric_labels_vec))

novel_df$label2 <- ifelse(big_boy$numeric_labels_vec==1,'lyrical','no')
novel_df$label2 <- as.factor(novel_df$label2)
outcomeName <- 'label2'

set.seed(19)
splitIndex <- createDataPartition(novel_df[,outcomeName], p = .80, list = FALSE, times = 1)
novel_train <- novel_df[ splitIndex,]
novel_test  <- novel_df[-splitIndex,]

prop.table(table(novel_train$label2))
prop.table(table(novel_test$label2))

m <- randomForest(novel_train[,-31], novel_train$label2, 
                  sampsize = round(0.8*(length(novel_train$label2))),ntree = 500, 
                  mtry = sqrt(30), importance = TRUE)

# The next line displays the out-of-bag accuracy:

print(m) 
predict(m, newdata = head(novel_test), type = "prob")

#library(caret)
x <- varImp(m)
varImpPlot(m,type=2)
