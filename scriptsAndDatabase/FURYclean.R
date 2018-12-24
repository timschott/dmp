setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
# library(rJava)
# library("openNLPdata")


stock <- c("Title", "Type", "ID", "Unit")
fury <- scan("rawTexts/william-faulkner-the-sound-and-the-fury.txt",what="character",sep="\n")
fury.start <- which(fury=="Through the fence, between the curling flower spaces, I could see them")
fury.fin <- which(fury=="window and doorway, and signboard, each in its ordered place.")
fury<-fury[fury.start:fury.fin]

# gsubing.
# _
fury <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE,fury)
fury <- gsub('Mrs.', 'Mrs', perl=TRUE,fury)
fury <- gsub('Mr.', 'Mr', perl=TRUE,fury)
fury <- gsub('APRIL EIGHTH, 1928', '',fury)
fury <- gsub('JUNE SECOND, 1910', '',fury)
fury <- gsub('APRIL SIXTH, 1928', '',fury)
fury <- gsub('_', '', perl=TRUE,fury)
print(length(fury))

first_bite <- fury[1:2499]
second_bite<- fury[2500:4999]
third_bite <- fury[5000:7499]
fourth_bite<- fury[7500:8598]

fury.sents.first <- paste0(first_bite, collapse = "\n")
fury.sents.first <- unlist(tokenize_sentences(fury.sents.first))

fury.sents.second <- paste0(second_bite, collapse = "\n")
fury.sents.second <- unlist(tokenize_sentences(fury.sents.second))

fury.sents.third <- paste0(third_bite, collapse = "\n")
fury.sents.third <- unlist(tokenize_sentences(fury.sents.third))

fury.sents.fourth <- paste0(fourth_bite, collapse = "\n")
fury.sents.fourth <- unlist(tokenize_sentences(fury.sents.fourth))

fury.sents <- c(fury.sents.first, fury.sents.second, fury.sents.third, fury.sents.fourth)

print(length(fury.sents))

bad_spots <-c(0)
for(i in seq(1:length(fury.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(fury.sents[i], nchar(fury.sents[i])-1, nchar(fury.sents[i]))
  test2 <- substr(fury.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?”", "!”") && test2==tolower(test2)){
      fury.sents[i] <- paste(fury.sents[i], fury.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

fury.sents[bad_spots]
fury.sents <- fury.sents[-c(bad_spots)]
print(length(fury.sents))

bad_spots<-c(0)

## standalone, you need the third condition. 
for(i in seq(1:length(fury.sents))){
  #if the sentence ends with a punctuation mark and the next sentence starts with a lowercase, combine them
  test <- substr(fury.sents[i], nchar(fury.sents[i]), nchar(fury.sents[i]))
  test2 <- substr(fury.sents[i+1], 1, 1)
  test3 <- substr(fury.sents[i], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
    fury.sents[i] <- paste(fury.sents[i], fury.sents[i+1])
    # print(fury.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}
bad_spots <- bad_spots[-c(1)]
fury.sents <- fury.sents[-bad_spots]
print(length(fury.sents))

fury.sents.df <- as.data.frame(fury.sents, stringsAsFactors = FALSE)


fury.title <- rep("theSoundAndTheFury", 9197)
fury.sents.type <- rep("sentence", 9197)
fury.sents.counter<-seq(1, 9197)
fury.sents.id <- paste0("THE_SOUND_AND_THE_FURY", "SENT_", fury.sents.counter)
print(length(fury.sents.id))
fury.sents.matrix <- cbind(fury.title, fury.sents.type, fury.sents.id, fury.sents)
fury.sents.df <- as.data.frame(fury.sents.matrix, stringsAsFactors = FALSE)
colnames(fury.sents.df) <- stock
# okay i think it's good now.
# fury To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", fury.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theSoundAndTheFury' LIMIT 2")
dbDisconnect(con)

# words. 

stock <- c("Title", "Type", "ID", "Unit")
fury <- scan("rawTexts/william-faulkner-the-sound-and-the-fury.txt",what="character",sep="\n")
fury.start <- which(fury=="Through the fence, between the curling flower spaces, I could see them")
fury.fin <- which(fury=="window and doorway, and signboard, each in its ordered place.")
fury<-fury[fury.start:fury.fin]

# gsubing.
# _
fury <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE,fury)
fury <- gsub('Mrs.', 'Mrs', perl=TRUE,fury)
fury <- gsub('Mr.', 'Mr', perl=TRUE,fury)
fury <- gsub('APRIL EIGHTH, 1928', '',fury)
fury <- gsub('JUNE SECOND, 1910', '',fury)
fury <- gsub('APRIL SIXTH, 1928', '',fury)
fury <- gsub('_', '', perl=TRUE,fury)

fury <- gsub('T P', 'TP', perl=TRUE,fury)

fury.not.blanks <- which(fury != "")
fury <- fury[fury.not.blanks]

fury.temp <- fury
fury.temp <- paste(fury.temp, collapse=" ")
fury.temp <-tolower(fury.temp)
# a better regex that is going to maintain contractions. important! 

fury.temp <- unlist(strsplit(fury.temp, "[^\\w’]", perl=TRUE))
fury.not.blanks <- which(fury.temp != "")
fury.words <- fury.temp[fury.not.blanks]
length(fury.words)


fury.title <- rep("theSoundAndTheFury", 96472)
fury.words.type <- rep("word", 96472)
fury.words.counter <- seq(1, 96472)
fury.words.id <- paste0("THE_SOUND_AND_THE_FURY_", "WORD_", fury.words.counter)

fury.words.matrix <- cbind(fury.title, fury.words.type, fury.words.id, fury.words)

fury.words.df <- as.data.frame(fury.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(fury.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", fury.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theSoundAndTheFury' LIMIT 10")
dbDisconnect(con)

# paras. 

fury.paragraphs <- read.csv("Python_Scripts/checkCorpus/FURY_paras.csv", stringsAsFactors = FALSE)
fury.paragraphs <- fury.paragraphs[-c(1:20, 3232:3235),]
colnames(fury.paragraphs) <- c("arb", "paras")

fury.paragraphs <- fury.paragraphs %>%
  transmute(paragraph = gsub('APRIL EIGHTH, 1928', '', perl=TRUE, paras))

fury.paragraphs <- fury.paragraphs %>%
  transmute(paras = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, paragraph))

fury.paragraphs <- fury.paragraphs %>%
  transmute(paragraph = gsub('JUNE SECOND, 1910', '', perl=TRUE, paras))

fury.paragraphs <- fury.paragraphs %>%
  transmute(paras = gsub('APRIL SIXTH, 1928', '', perl=TRUE, paragraph))

fury.paragraphs <- fury.paragraphs %>%
  transmute(paragraph = gsub('_', '', perl=TRUE, paras))

fury.paragraphs <- fury.paragraphs %>%
  transmute(paras = gsub('T P', 'TP', perl=TRUE, paragraph))

# cool.
fury.paragraphs <- fury.paragraphs %>% filter(paras!="")

print(length(fury.paragraphs$paras))

fury.paragraphs <- fury.paragraphs %>%
  transmute(paragraph = gsub('\n', ' ', perl=TRUE, paras))

fury.title <- rep("theSoundAndTheFury", 3208)
fury.para.type <- rep("paragraph", 3208)
fury.para.counter<-seq(1, 3208)
fury.para.id <- paste0("THE_SOUND_AND_THE_FURY_", "PARAGRAPH_", fury.para.counter)
print(length(fury.para.id))
fury.para.matrix <- cbind(fury.title, fury.para.type, fury.para.id, fury.paragraphs)
fury.para.df <- as.data.frame(fury.para.matrix, stringsAsFactors = FALSE)
colnames(fury.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", fury.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theSoundAndTheFury' LIMIT 2")
dbDisconnect(con)

# fury done.