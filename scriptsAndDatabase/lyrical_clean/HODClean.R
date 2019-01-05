setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
install.packages("rJava")
library(rJava)
library("openNLPdata")

stock <- c("Title", "Type", "ID", "Unit", "Label")

#output <- unlist(tokenize_sentences(test_sentence))
# Mr. Mrs. 
heartOfDarkness <- scan("rawTexts/lyrical/conrad-heart-of-darkness.txt",what="character",sep="\n")

heartOfDarkness.start<- which(heartOfDarkness == "I")
heartOfDarkness.end <- which(heartOfDarkness == "sky--seemed to lead into the heart of an immense darkness.")

heartOfDarkness<-heartOfDarkness[heartOfDarkness.start: heartOfDarkness.end]

#get rid of volume markers. 
heartOfDarkness.sents<-heartOfDarkness[-(1)]
heartOfDarkness.sents<-heartOfDarkness.sents[-(1160)]
heartOfDarkness.sents<-heartOfDarkness.sents[-(2128)]

heartOfDarkness.sents <- gsub('Mr\\.', 'Mr', heartOfDarkness.sents)

#paste is dumb with big inputs so break in half to be more manageable

#break near middle at full sentence. 
first_half <- heartOfDarkness.sents[1:1543]
second_half<- heartOfDarkness.sents[-(1:1543)]

heartOfDarkness.sents.first <- paste0(first_half, collapse = "\n")
heartOfDarkness.sents.first <- unlist(tokenize_sentences(heartOfDarkness.sents.first))

heartOfDarkness.sents.second <- paste0(second_half, collapse = "\n")
heartOfDarkness.sents.second <- unlist(tokenize_sentences(heartOfDarkness.sents.second))

#recombine
heartOfDarkness.sents <- c(heartOfDarkness.sents.first,heartOfDarkness.sents.second)

heartOfDarkness.sents <- gsub('\"', "'", heartOfDarkness.sents)

#Mrs doesn't actually occur but well keep the pattern. heartOfDarkness.sents <- gsub('Mrs\\.', 'Mrs', heartOfDarkness.sents)
### ToDo: Fix the puncutation lowercase problem

bad_spots <-c(0)
for(i in seq(1:length(heartOfDarkness.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(heartOfDarkness.sents[i], nchar(heartOfDarkness.sents[i]), nchar(heartOfDarkness.sents[i]))
  test2 <- substr(heartOfDarkness.sents[i+1], 1, 1)
  test3 <- substr(heartOfDarkness.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      heartOfDarkness.sents[i] <- paste(heartOfDarkness.sents[i], heartOfDarkness.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
heartOfDarkness.sents[bad_spots]
heartOfDarkness.sents <- heartOfDarkness.sents[-c(bad_spots)]

bad_spots <-c(0)
for(i in seq(1:length(heartOfDarkness.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(heartOfDarkness.sents[i], nchar(heartOfDarkness.sents[i])-1, nchar(heartOfDarkness.sents[i]))
  test2 <- substr(heartOfDarkness.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      #print(i)
      heartOfDarkness.sents[i] <- paste(heartOfDarkness.sents[i], heartOfDarkness.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

heartOfDarkness.sents[bad_spots]
heartOfDarkness.sents <- heartOfDarkness.sents[-c(bad_spots)]

heartOfDarkness.title <- rep("heartOfDarkness", 2402)
heartOfDarkness.sents.type <- rep("sentence", 2402)
print(length(heartOfDarkness.sents))
# now put those into a matrix 

heartOfDarkness.sents.counter <- seq(1, 2402)
heartOfDarkness.label <- rep("1", 2402)
# paste together HOD_SENT_COUNTER

heartOfDarkness.sents.id <- paste0("HEART_OF_DARKNESS_", "SENT_", heartOfDarkness.sents.counter)

heartOfDarkness.sents.matrix <- cbind(heartOfDarkness.title, heartOfDarkness.sents.type, heartOfDarkness.sents.id, heartOfDarkness.sents, heartOfDarkness.label)

heartOfDarkness.sents.df <- as.data.frame(heartOfDarkness.sents.matrix)
colnames(heartOfDarkness.sents.df) <- c("Title", "Type", "ID", "Unit", "Label")

#write to database. 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
# dbExecute(con, "DELETE FROM textTable WHERE Type='sentence' AND Title='heartOfDarkness'")

### Columns. 
# we should no longer need to set the column names
# dbWriteTable(con, "textTable", heartOfDarkness.sents.df[0, ])
dbWriteTable(con, "textTable", heartOfDarkness.sents.df, append=TRUE, row.names=FALSE)

dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='heartOfDarkness' LIMIT 2")

dbListTables(con)

dbDisconnect(con)

#### into words. 
heartOfDarkness <- scan("rawTexts/lyrical/conrad-heart-of-darkness.txt",what="character",sep="\n")
heartOfDarkness.start<- which(heartOfDarkness == "I")
heartOfDarkness.end <- which(heartOfDarkness == "sky--seemed to lead into the heart of an immense darkness.")

heartOfDarkness<-heartOfDarkness[heartOfDarkness.start: heartOfDarkness.end]

## Grep out VOLUME markers

heartOfDarkness.temp<-heartOfDarkness[-(1)]
heartOfDarkness.temp<-heartOfDarkness.temp[-(1160)]
heartOfDarkness.temp<-heartOfDarkness.temp[-(2128)]
heartOfDarknessCount <-sum(sapply(heartOfDarkness.temp, str_count, "\\w+"))

## Replace Contractions
heartOfDarkness.temp<-replace_contraction(heartOfDarkness.temp)

#### WORDS? 

heartOfDarkness.temp <- paste(heartOfDarkness.temp, collapse=" ")
heartOfDarkness.temp <-tolower(heartOfDarkness.temp)
heartOfDarkness.temp <- strsplit(heartOfDarkness.temp, "\\W")

heartOfDarkness.temp <-unlist(heartOfDarkness.temp)

darkness.not.blanks <- which(heartOfDarkness.temp != "")

heartOfDarknessWords <- heartOfDarkness.temp[darkness.not.blanks]

heartOfDarknessWords[1:10]

# and do the same thing in adding it to the data base. 

heartOfDarkness.title <- rep("heartOfDarkness", 39085)
heartOfDarkness.words.type <- rep("word", 39085)
heartOfDarkness.words.counter <- seq(1, 39085)
heartOfDarkness.words.id <- paste0("HOD_", "WORD_", heartOfDarkness.words.counter)

heartOfDarkness.words.matrix <- cbind(heartOfDarkness.title, heartOfDarkness.words.type, heartOfDarkness.words.id, heartOfDarknessWords)

heartOfDarkness.words.df <- as.data.frame(heartOfDarkness.words.matrix)

#writeLines(heartOfDarknessWords, "heartOfDarknessWords.txt")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(heartOfDarkness.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", heartOfDarkness.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' LIMIT 10")
dbDisconnect(con)

### now add the paragraphs.. from python 
#### should just put the python file in this directory and then i can use the output 
## without having to manually update via copy paste. 

heartOfDarkness.paragraphs <- read.csv("paras/HOD_paras.csv", stringsAsFactors = FALSE)
# get rid of junk
heartOfDarkness.paragraphs <- heartOfDarkness.paragraphs[-c(1:12, 75, 113, 212:292),]

colnames(heartOfDarkness.paragraphs) <- c("arbitrary", "para")

heartOfDarkness.paragraphs <- heartOfDarkness.paragraphs %>%
  transmute(paragraph = str_replace_all(para, "[\n]", " "))
heartOfDarkness.paragraphs <- heartOfDarkness.paragraphs %>% 
  transmute(para=  gsub("\"", "'", paragraph))

heartOfDarkness.title <- rep("heartOfDarkness", 197)
heartOfDarkness.paragraphs.type <- rep("paragraph", 197)
heartOfDarkness.paragraphs.counter <- seq(1, 197)
heartOfDarkness.paragraphs.id <- paste0("HEART_OF_DARKNESS_", "PARAGRAPH_", heartOfDarkness.paragraphs.counter)
heartOfDarkness.label <- rep("1", 197)
heartOfDarkness.paragraphs.matrix <- cbind(heartOfDarkness.title, heartOfDarkness.paragraphs.type, heartOfDarkness.paragraphs.id, heartOfDarkness.paragraphs$para, heartOfDarkness.label)
heartOfDarkness.paragraphs.df <- as.data.frame(heartOfDarkness.paragraphs.matrix)
colnames(heartOfDarkness.paragraphs.df) <- c("Title", "Type", "ID", "Unit", "Label")

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
### Columns. 
# we should no longer need to set the column names
dbWriteTable(con, "textTable", heartOfDarkness.paragraphs.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' and Title = 'heartOfDarkness' LIMIT 2")
summary(con)
dbDisconnect(con)
