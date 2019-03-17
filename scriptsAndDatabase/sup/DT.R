## DT
setwd("~/Documents/7thSemester/dmp/corpus")
library(scales)
library("RSQLite")
library("dplyr")
library(caret)
# install.packages("reprtree")
library(reprtree)
library(ggcorrplot)
library("rpart.plot")
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
ggcorrplot(correlationMatrix, method = "circle", title="Correlation Matrix")

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
new_df_copy <- new_df

max(new_df$perceive_freq)
min(new_df$perceive_freq)
min(new_df[,24])
(new_df[7,24] - min(new_df[,24]))/ (max(new_df[,24]) - min(new_df[,24]))

# okay, so absalom does not have the highest perceive frequency
# so we shouldn't be seeing a 1 in that column

new_df$perceive_freq[1]
# we need to hold the column steady while the row increases 

# i = 1 (first column)
# j = 1, 2, 3, 4, ... 50

# i = 2 (second column)
# j = 1, 2, 3, 4, ... 50

# okay - this loop is totally wrong! 
# need to fill up a *new* dataframe on each call
# or else you're going to be referring to the recently entered values
# as your min and max.

for(i in seq(1, 30)){
  for(j in seq(1, 50)){
    new_df_copy[j,i] <- (new_df[j,i] - min(new_df[,i])) / (max(new_df[,i]) - min(new_df[,i]))
  }
}
  
normalized_df <- new_df_copy

write.csv(normalized_df,'particularly_normalized.csv')

#######
# read in better data. 

new_df <- read.csv('particularly_normalized.csv', stringsAsFactors = FALSE)

new_df <- new_df %>% dplyr::select(-c(X))
new_df$label2 <- as.factor(new_df$label2)
#### try our hand using just normalized data but still every feature. 
#### Feature Selection from GINI
set.seed(25)
splitIndex <- createDataPartition(new_df[,31], p = .80, list = FALSE, times = 1)
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
m
# 2 % out of bag error 

#library(caret)
x <- varImp(m)
t <- varImpPlot(m,type=2, sort=TRUE, main="Variable Importance")
meanDecreaseGini <- sort(t)
# random forest says that most important is:
# dialogue, perceive,consecutive_repeat_freq i_freq
# 3/15 after good data influx. 
# okay well the new results are pretty good. 
# i fixed dialogue with my random sampling and it still reigns supreme.
# paragraph count bubbles to the top. 
# 4 features and 50 data points is a tough bargain.... 
# so i am going to work with dialogue, perceive, I, anaphora_freq

#a random forest with just those features

# dialogue continues to dominate my other features. what if i get rid of it? 

subset <- new_df %>% dplyr::select(c(perceive_freq, i_freq, dialogue_freq, consecutive_repeat_freq_vec, label2))

subset$label2 = factor(subset$label2)
# rf <- randomForest(Species ~ ., data=iris)
set.seed(49)
splitIndex <- createDataPartition(subset[,5], p = .80, list = FALSE, times = 1)
novel_train <- subset[ splitIndex,]
novel_test  <- subset[-splitIndex,]

prop.table(table(subset$label2))

m <- randomForest(label2~ ., subset, 
                  sampsize = round(0.8*(length(subset$label2))),ntree = 500, 
                  importance = TRUE)

reprtree:::plot.getTree(m, main="Decision Tree")
testtree <- ReprTree(m, novel_test)
plot(testtree)

m$confusion[4]
# 0 % OOB with 4 traits. Totally overfitting. Still need to remove features.

varImp(m)
round(importance(m), 2)

# https://stackoverflow.com/questions/37897252/plot-confusion-matrix-in-r-using-ggplot

confusion_matrix <- as.data.frame(table(m$predicted, subset$label2))

TClass <- factor(c("detective", "detective", "lyrical", "lyrical"))
PClass <- factor(c("detective", "lyrical", "detective","lyrical"))
Y      <- c(22,2,4,22)
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y= PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ff6961", high = "#61f7ff") +
  theme_bw() + theme(legend.position = "none")+
  ggtitle("Decision Tree Confusion Matrix") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("True Class") +
  ylab("Predicted Class")



# every single decision tree that uses dialogue boasts perfect performance
# the best attribute? somehow a double edged sword.
# this leads me to think suspiciously about my own work
# i'm not *that* confident in it so i will keep treading....

# next step: make all 6 confusion matrices to prove this point

# then kick into svm, still with these four variables, reduce down to prove
# my point that even with *meh* data you can predict these bad boys

# try with each unique pair. 
names <- c("Perceive", "I", "Dialogue", "Anaphora")

TClass <- factor(c("detective", "detective", "lyrical", "lyrical"))
PClass <- factor(c("detective", "lyrical", "detective","lyrical"))

mispred_perceive_I <- c(0)
mispred_perceive_dialogue <- c(0)
mispred_perceive_anaphora <- c(0)
mispred_I_dialogue<- c(0)
mispred_I_anaphora <- c(0)
mispred_dialogue_anaphora <- c(0)

for(i in seq(1,4)){
  for(j in seq(i+1,4)){
    if(i==4){
      break
    }
    
    m <- randomForest(as.data.frame(cbind(subset[,i], subset[,j])),subset$label2,  
                      sampsize = round(0.8*(length(subset$label2))),ntree = 500, 
                      importance = TRUE)
    # create a confusion matrix per iteration.
    
    print('=========')
    
    # pull out confusion matrix 
    
    Y <- c(m$confusion[1],m$confusion[2],m$confusion[3],m$confusion[4])
    
    # bind together as a data frame
    
    df <- data.frame(TClass, PClass, Y)
    # args for the graph 
    
    title <- paste(names[i], "&", names[j], "Results")
    file_name <- paste0(names[i], "_and_", names[j], "_confusion_mat.png")
    # create confusion plot 
    g <- ggplot(data =  df, mapping = aes(x = TClass, y= PClass)) +
      # file in confusion numbers 
      geom_tile(aes(fill = Y), colour = "white") +
      geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
      # set colors 
      scale_fill_gradient(low = "#ff6961", high = "#61f7ff") +
      theme_bw() + theme(legend.position = "none")+
      ggtitle(title) +
      # add a title
      theme(plot.title = element_text(hjust = 0.5)) +
      # add labels
      xlab("True Class") +
      ylab("Predicted Class")
    
    ggsave(filename = file_name, plot=g)
    
    ## can I also capture which novels were mispredicted?
    ## might be a little more laborious
    ## check if the predicted level equals the true level. 
    ## record each of these in their own vector
    
    for(k in seq(1:50)){
      # print(m$predicted[i])
      if(k %in% seq(1,26) && m$predicted[k]!=as.factor("lyrical")){
        if(i == 1 && j == 2){
          mispred_perceive_I <- append(mispred_perceive_I, big_boy[k,1])
        
        }
        if(i == 1 && j == 3){
          mispred_perceive_dialogue <- append(mispred_perceive_dialogue, big_boy[k,1])
          
        }
        if(i == 1 && j == 4){
          mispred_perceive_anaphora <- append(mispred_perceive_anaphora, big_boy[k,1])
          
        }
        if(i == 2 && j == 3){
          mispred_I_dialogue <- append(mispred_I_dialogue, big_boy[k,1])
          
        }
        if(i == 2 && j == 4){
          mispred_I_anaphora <- append(mispred_I_anaphora, big_boy[k,1])
          
        }
        if(i == 3 && j == 4){
          mispred_dialogue_anaphora <- append(mispred_dialogue_anaphora, big_boy[k,1])
        }
      }
      if(k %in% seq(27,50) && m$predicted[k]!=as.factor("no")){
        if(i == 1 && j == 2){
          mispred_perceive_I <- append(mispred_perceive_I, big_boy[k,1])
          
        }
        if(i == 1 && j == 3){
          mispred_perceive_dialogue <- append(mispred_perceive_dialogue, big_boy[k,1])
          
        }
        if(i == 1 && j == 4){
          mispred_perceive_anaphora <- append(mispred_perceive_anaphora, big_boy[k,1])
          
        }
        if(i == 2 && j == 3){
          mispred_I_dialogue <- append(mispred_I_dialogue, big_boy[k,1])
          
        }
        if(i == 2 && j == 4){
          mispred_I_anaphora <- append(mispred_I_anaphora, big_boy[k,1])
          
        }
        if(i == 3 && j == 4){
          mispred_dialogue_anaphora <- append(mispred_dialogue_anaphora, big_boy[k,1])
        }
      }
    }
  }
}

mispred_perceive_I <- mispred_perceive_I[-c(1)]
mispred_perceive_dialogue <- mispred_perceive_dialogue[-c(1)]
mispred_perceive_anaphora <- mispred_perceive_anaphora[-c(1)]
mispred_I_dialogue<- mispred_I_dialogue[-c(1)]
mispred_I_anaphora <- mispred_I_anaphora[-c(1)]
mispred_dialogue_anaphora <- mispred_dialogue_anaphora[-c(1)]

## write to file

writeLines(mispred_perceive_I, "mispred_perceive_I.txt")
writeLines("none", "mispred_perceive_dialogue.txt")
writeLines(mispred_perceive_anaphora, "mispred_perceive_anaphora.txt")
writeLines("blood Meridian, theSpiralStaircase", "mispred_I_dialogue.txt")
writeLines(mispred_I_anaphora, "mispred_I_anaphora.txt")
writeLines("bloodMeridian", "mispred_dialogue_anaphora.txt")




