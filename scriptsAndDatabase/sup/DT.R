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
novel_df <- big_boy %>% select(-c(labels_vec, titles_vec, numeric_labels_vec))

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

novel_df$dialogue_freq[3] <- 0.2206338
novel_df$dialogue_freq[17] <- 0.2206338
novel_df$dialogue_freq[20] <- 0.2206338

# normalize

scaled.dat <- as.data.frame(scale(novel_df[,1:30]))
colMeans(scaled.dat) 

normalized_novel_df <- as.data.frame(cbind(scaled.dat, novel_df$label2))
colnames(normalized_novel_df)[31] <- c("label2")
str(normalized_novel_df)

write.csv(normalized_novel_df,'normalized.csv')
cat(colnames(normalized_novel_df))
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
splitIndex <- createDataPartition(normalized_novel_df[,outcomeName], p = .80, list = FALSE, times = 1)
novel_train <- normalized_novel_df[ splitIndex,]
novel_test  <- normalized_novel_df[-splitIndex,]

prop.table(table(novel_train$label2))
prop.table(table(novel_test$label2))

m <- randomForest(normalized_novel_df[,-31], normalized_novel_df$label2, 
                  sampsize = round(0.8*(length(normalized_novel_df$label2))),ntree = 500, 
                  mtry = sqrt(30), importance = TRUE)

# The next line displays the out-of-bag accuracy:

print(m) 
predict(m, newdata = head(novel_test), type = "prob")

#library(caret)
x <- varImp(m)
varImpPlot(m,type=2)

# random forest says that most important is:
# dialogue, perceive, consecutive_repeat, i, self;
# dialogue is a little bit of a red herring I think due to the instances
# of 0 messing us up. imputing from the mean made it ecven more important
# but the other 4 are interesting because they're Frequencies and not
# raw counts

#a random forest with just those features

subset <- normalized_novel_df %>% dplyr::select(c(dialogue_freq, i_freq, consecutive_counts_vec, 
                                         perceive_freq, self_freq, label2))

set.seed(299)
splitIndex <- createDataPartition(subset[,outcomeName], p = .80, list = FALSE, times = 1)
novel_train <- subset[ splitIndex,]
novel_test  <- subset[-splitIndex,]

prop.table(table(subset$label2))
prop.table(table(subset$label2))

m <- randomForest(subset[,-6], normalized_novel_df$label2, 
                  sampsize = round(0.8*(length(subset$label2))),ntree = 500, 
                  mtry = sqrt(6), importance = TRUE)

print(m) 
## 2% OOB.



