## DT
setwd("~/Documents/7thSemester/dmp/corpus")
library(scales)
library("RSQLite")
library("dplyr")
library(caret)
library(ggcorrplot)
library(randomForest)
library(MASS)
library(randomForest)

# let's look at what data is correlated. and then normalize our feature set. 

# load in csv
big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)
colnames(big_boy)
str(big_boy)

# remove identifying characteristics in a duplicate
novel_df <- big_boy %>% dplyr::select(-c(labels_vec, titles_vec, numeric_labels_vec))

# add label as a factor with 2 levels. 
novel_df$label2 <- ifelse(big_boy$numeric_labels_vec==1,'lyrical','no')
novel_df$label2 <- as.factor(novel_df$label2)
outcomeName <- 'label2'

# now let's check correlations

correlationMatrix <- cor(novel_df[,1:30])

# summarize the correlation matrix
print(correlationMatrix)

# Plot the correlation matrix -> with a correlation plot. 
ggcorrplot(correlationMatrix, method = "circle")

# find attributes that are highly corrected (ideally >0.75)
# just using 50% for our example
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.50)

# print indices of highly correlated attributes
print(sort(highlyCorrelated))

# so clearly they are all fairly correlated.. 

# scale

scaled.dat <- as.data.frame(scale(novel_df[,1:30]))
colMeans(scaled.dat) 

normalized_novel_df <- as.data.frame(cbind(scaled.dat, novel_df$label2))
colnames(normalized_novel_df)[31] <- c("label2")
str(normalized_novel_df)

write.csv(normalized_novel_df,'normalized.csv')
cat(colnames(normalized_novel_df))

# weird results -- the mean of dialogue is zero after this calculation
## i initially did "scaling" but this time I'm going to actually
## normalize the set

# data[column_name] = (data[column_name] - data[column_name].min()) / (data[column_name].max() - data[column_name].min())

max(novel_df$dialogue_freq)
min(novel_df$dialogue_freq)

new_df <- novel_df

for(i in seq(1, 30)){
  for(j in seq(1, 50)){
    new_df[j,i] <- (new_df[j,i] - min(new_df[,i])) / (max(new_df[,i]) - min(new_df[,i]))
  }
}
  
write.csv(new_df,'particularly_normalized.csv')

#######
# step wise feature selection 

training.samples <- createDataPartition(normalized_novel_df$label2,p = 0.8, list = FALSE)
train.data  <- normalized_novel_df[training.samples, ]
test.data <- normalized_novel_df[-training.samples, ]

# Fit the model
# null model means not dependent on anything. 
modelnull <- glm(label2 ~ 1, data = train.data, family = binomial)

# the biggest model. all of our data
modelfull <- glm(label2 ~ ., data = train.data, family = binomial)

# trace just gets rid of our output 
# tells us to get rid of tri cep 
stepAIC(modelfull, direction = "backward", scope=list(upper=modelfull,lower=modelnull),trace = TRUE)  

# stepwise AIC says use: sent_counts_vec, consecutive_counts_vec, dialogue_freq.

#### try our hand using just normalized data but still every feature. 
#### Feature Selection from GINI
set.seed(19)
splitIndex <- createDataPartition(new_df[,outcomeName], p = .80, list = FALSE, times = 1)
novel_train <- new_df[ splitIndex,]
novel_test  <- new_df[-splitIndex,]

prop.table(table(novel_train$label2))
prop.table(table(novel_test$label2))

m <- randomForest(new_df[,-31], new_df$label2, 
                  sampsize = round(0.8*(length(new_df$label2))),ntree = 500, 
                  mtry = sqrt(30), importance = TRUE)

# The next line displays the out-of-bag accuracy:

print(m) 
predict(m, newdata = head(novel_test), type = "prob")

#library(caret)
x <- varImp(m)
varImpPlot(m,type=2, sort=TRUE, main="Variable Importance")

# random forest says that most important is:
# dialogue, perceive, para, consecutive_repeat, ;
# dialogue is a little bit of a red herring I think due to the instances
# of 0 messing us up. imputing from the mean made it ecven more important
# but the other 4 are interesting because they're Frequencies and not
# raw counts 
# 3/8
# okay well the new results are pretty good. 
# i fixed dialogue with my random sampling and it still reigns supreme.
# paragraph count bubbles to the top. 
# 5 features and 50 data points is a tough bargain.... 
# i think ill manually eschew paragraphs because i don't think that's as interesting
# so i am going to work with dialogue, perceive, anaphora, self

#a random forest with just those features

subset <- new_df %>% dplyr::select(c(dialogue_freq, 
                                         perceive_freq, self_freq, consecutive_counts_vec, label2))

set.seed(299)
splitIndex <- createDataPartition(subset[,outcomeName], p = .80, list = FALSE, times = 1)
novel_train <- subset[ splitIndex,]
novel_test  <- subset[-splitIndex,]

prop.table(table(subset$label2))
prop.table(table(subset$label2))

m <- randomForest(label2~ ., subset, 
                  sampsize = round(0.8*(length(subset$label2))),ntree = 500, 
                  mtry = sqrt(6), importance = TRUE)

print(m) 
## 4% OOB!
round(importance(m), 2)
m$predicted
m$confusion
