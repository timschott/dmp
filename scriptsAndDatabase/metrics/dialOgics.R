### dialOgics 
setwd("~/Documents/7thSemester/dmp/corpus")
library(scales)
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library(stringi)
library("tm")
library(qdap)

big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
detective_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='0'")
detective_para_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='paragraph' AND Label ='0'")
detective_titles <- sort(unique(detective_para_df$Title))
dbDisconnect(con)

# okay, plenty of sentences.


detective_sent_df$Unit[633:700]
detective_sent_df$Unit[701:777]
detective_sent_df$Unit[5655:5699]

detective_para_df$Unit[39:50]
replace_contraction("'What can I do for you?' he asked.")
replace_contraction("Thomas' sword")
# manually replace I'd I'm I'll I've 

x <- c("ralph's","marco's", "tims'", "I'm, 'tis")
gsub("([a-z])(?:'[a-z])|([a-z])(?:'[a-z])",paste0("\\1","s"),x, perl=TRUE)
# okay
# now to work on tims'
# for possessives ending in s apost.
gsub("([a-z])(?:')|([a-z])(?:'[a-z])","\\1",x, perl=TRUE)

# cool.

# okay so move through the paragraphs, run these gsubs, and then
# we should be good to count tick marks.

test <- detective_para_df$Unit[1999:2299]

test <- gsub("I'm", "Im", test)
test <- gsub("I'll", "Ill", test)
test <- gsub("I've", "Im", test)
test <- gsub("I'd", "Id", test)
test <- gsub("'tis", "tis", test)
test <- gsub("o'clock", "o clock", test)
test <- gsub("([a-z])(?:'[a-z])|([a-z])(?:'[a-z])",paste0("\\1","s"),test, perl=TRUE)
test<-gsub("([a-z])(?:')|([a-z])(?:'[a-z])","\\1",test, perl=TRUE)

test[1:10]
sent <- c("'Now, Mr Milburgh,' he said brusquely, 'I want to ask you: Have you ever seen a piece of paper like this before?'")
grep("'", sent)
str_count(sent, ",")
count <- c(0)
counts <- str_count(test, "'")

for(i in seq(1, length(counts))){
  if(counts[i]%%2!=0){
    counts[i] <- counts[i]-1
  }
}
sum(counts/2)
191 / 301

zero <- c(0,0,0)
kinda <- c(0,0,0,1,1)
(length(which(kinda>.5))==length(kinda))
zero >1


# now let's try this for every book. 
# also along the way i'm going to count up ? ! —
# but i will do too loops because the first one is gonna be gsub heavy.

detective_dialogue_freq <- c(0)

for(i in seq(from =1, to=24)){
  temp_count_vec <- c(0)
  
  paras <- filter(detective_para_df, Title==detective_titles[i])
  paras <- paras$Unit
  paras <- gsub("I'm", "Im", paras)
  paras <- gsub("I'll", "Ill", paras)
  paras <- gsub("I've", "Im", paras)
  paras <- gsub("I'd", "Id", paras)
  paras <- gsub("'tis", "tis", paras)
  paras <- gsub("o'clock", "o clock", paras)
  paras <- gsub("([a-z])(?:'[a-z])|([a-z])(?:'[a-z])",paste0("\\1","s"),paras, perl=TRUE)
  paras<- gsub("([a-z])(?:')|([a-z])(?:'[a-z])","\\1",paras, perl=TRUE)
  temp_count_vec <- str_count(paras, "'")
  
  if((length(which(temp_count_vec<.5))==length(temp_count_vec)) || i==13){
    temp_count_vec<-c(0)
    temp_count_vec_1 <- str_count(paras, "“")
    temp_count_vec_2 <-str_count(paras, "”")
    temp_count_vec <- temp_count_vec_1 + temp_count_vec_2
  }
  
  for(i in seq(1, length(temp_count_vec))){
    if(temp_count_vec[i]%%2!=0){
      temp_count_vec[i] <- temp_count_vec[i]-1
    }
  }
  
  detective_dialogue_freq <- append(detective_dialogue_freq, length(which(temp_count_vec >1))/length(paras))
}

detective_dialogue_freq <- detective_dialogue_freq[-c(1)]


# investigate 3 and 13. 

# 13 is very odd....3 was not properly encoded but I fixed it. 
ash <- filter(detective_para_df, Title==detective_titles[3])
ash.paras <- ash$Unit
ash.paras[1:10]
(length(which(kinda>.5))==length(kinda))

# lyrical baby

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
lyrical_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='1'")
lyrical_para_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='paragraph' AND Label ='1'")
lyrical_titles <- sort(unique(lyrical_para_df$Title))
dbDisconnect(con)

# okay, plenty of sentences.

lyrical_dialogue_freq <- c(0)

for(i in seq(from =1, to=26)){
  temp_count_vec <- c(0)
  
  paras <- filter(lyrical_para_df, Title==lyrical_titles[i])
  paras <- paras$Unit
  paras <- gsub("I'm", "Im", paras)
  paras <- gsub("I'll", "Ill", paras)
  paras <- gsub("I've", "Im", paras)
  paras <- gsub("I'd", "Id", paras)
  paras <- gsub("'tis", "tis", paras)
  paras <- gsub("o'clock", "o clock", paras)
  paras <- gsub("([a-z])(?:'[a-z])|([a-z])(?:'[a-z])",paste0("\\1","s"),paras, perl=TRUE)
  paras<- gsub("([a-z])(?:')|([a-z])(?:'[a-z])","\\1",paras, perl=TRUE)
  temp_count_vec <- str_count(paras, "'")
  
  if((length(which(temp_count_vec<.5))==length(temp_count_vec))){
    temp_count_vec<-c(0)
    temp_count_vec_1 <- str_count(paras, "“")
    temp_count_vec_2 <-str_count(paras, "”")
    temp_count_vec <- temp_count_vec_1 + temp_count_vec_2
  }
  
  if(i==13){
    print("i is mine")
    temp_count_vec<-c(0)
    temp_count_vec <- str_count(paras, "^—")
    print(length(which(temp_count_vec >=1)))
  }
  
  if(i==25){
    temp_count_vec<-c(0)
    temp_count_vec_1 <- str_count(paras, "‘")
    temp_count_vec_2 <-str_count(paras, "’")
    temp_count_vec <- temp_count_vec_1 + temp_count_vec_2
  }
  
  for(j in seq(1, length(temp_count_vec))){
    if(temp_count_vec[j]%%2!=0 && i!=13){
      print(i)
      temp_count_vec[j] <- temp_count_vec[j]-1
    }
  }
  
  lyrical_dialogue_freq <- append(lyrical_dialogue_freq, length(which(temp_count_vec >=1))/(length(paras)*2))
}

lyrical_dialogue_freq <- lyrical_dialogue_freq[-c(1)]
lyrical_dialogue_freq[3] <- 0
lyrical_dialogue_freq[17] <- 0
lyrical_dialogue_freq[20] <- 0

# lyrical baby
# hotfix for portrait's hyphens
port <- filter(lyrical_para_df, Title==lyrical_titles[13])
# 3 and 17 and 20 should be zero as they are not encoded with dialogue
# wide uses single curly. 
wide <- filter(lyrical_para_df, Title==lyrical_titles[25])
wide$Unit[1:10]

dialogue_freq <- c(lyrical_dialogue_freq, detective_dialogue_freq)

big_boy <- as.data.frame(cbind(big_boy, dialogue_freq), stringsAsFactors = FALSE)
colnames(big_boy)
write.csv(big_boy,'starts.csv')
