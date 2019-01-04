setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
rm(list=ls())
sec <- scan("rawTexts/detective/agatha-christie-the-secret-adversary.txt",what="character",sep="\n")
sec.start <- which(sec=="IT was 2 p.m. on the afternoon of May 7, 1915. The Lusitania had been struck by two torpedoes in succession and was sinking rapidly, while the boats were being launched with all possible speed. The women and children were being lined up awaiting their turn. Some still clung desperately to husbands and fathers; others clutched their children closely to their breasts. One girl stood alone, slightly apart from the rest. She was quite young, not more than eighteen. She did not seem afraid, and her grave, steadfast eyes looked straight ahead.")
sec.fin <- which(sec =="“And a damned good sport too,” said Tommy.")
sec<- sec[sec.start:sec.fin]
print(length(sec))

sec.paragraphs <- as.data.frame(sec, stringsAsFactors=FALSE)
colnames(sec.paragraphs) <- c("paras")

chaps <- grep('CHAPTER', sec.paragraphs$paras)
sec.paragraphs <- as.data.frame(sec.paragraphs[-c(chaps)], stringsAsFactors=FALSE)
colnames(sec.paragraphs) <- c("paras")

sec.paragraphs<- sec.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

sec.paragraphs <- sec.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

sec.paragraphs <- sec.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

sec.paragraphs <- sec.paragraphs %>% 
  filter(paragraphs!="")

sec.paragraphs <- sec.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(sec.paragraphs)

sec.paragraphs <- sec.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

## 

print(length(sec.paragraphs$paras))

sec.paragraphs$paras[1] <- "It was 2 PM on the afternoon of May 7, 1915. The Lusitania had been struck by two torpedoes in succession and was sinking rapidly, while the boats were being launched with all possible speed. The women and children were being lined up awaiting their turn. Some still clung desperately to husbands and fathers; others clutched their children closely to their breasts. One girl stood alone, slightly apart from the rest. She was quite young, not more than eighteen. She did not seem afraid, and her grave, steadfast eyes looked straight ahead."

sec.title <- rep("theSecretAdversary", 3240)
sec.para.type <- rep("paragraph",3240)
sec.para.counter<-seq(1, 3240)
sec.para.id <- paste0("THE_SECRET_ADVERSARY_", "PARAGRAPH_", sec.para.counter)
sec.label <- rep("0", 3240)
print(length(sec.para.id))

sec.para.matrix <- cbind(sec.title, sec.para.type, sec.para.id, sec.paragraphs, sec.label)
sec.para.df <- as.data.frame(sec.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(sec.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", sec.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theSecretAdversary' LIMIT 2")
dbDisconnect(con)

# sentences. 


sec <- sec.paragraphs$paras

first_bite <- sec[1:2499]
second_bite <- sec[2500:3240]

sec.sents.first <- paste0(first_bite, collapse = "\n")
sec.sents.first <- unlist(tokenize_sentences(sec.sents.first))

sec.sents.second <- paste0(second_bite, collapse = "\n")
sec.sents.second <- unlist(tokenize_sentences(sec.sents.second))

sec.sents <- c(sec.sents.first, sec.sents.second)
sec.sents.df <- as.data.frame(sec.sents, stringsAsFactors = FALSE)
print(length(sec.sents.df$sec.sents))

# loops. 


bad_spots <-c(0)
for(i in seq(1:length(sec.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(sec.sents[i], nchar(sec.sents[i]), nchar(sec.sents[i]))
  test2 <- substr(sec.sents[i+1], 1, 1)
  test3 <- substr(sec.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      sec.sents[i] <- paste(sec.sents[i], sec.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
# sec.sents[bad_spots]
sec.sents <- sec.sents[-c(bad_spots)]


bad_spots <-c(0)
for(i in seq(1:length(sec.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(sec.sents[i], nchar(sec.sents[i])-1, nchar(sec.sents[i]))
  test2 <- substr(sec.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if((test %in% c('?”', '!”') && test2==tolower(test2))){
      #print(i)
      sec.sents[i] <- paste(sec.sents[i], sec.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

sec.sents[bad_spots]
sec.sents <- sec.sents[-c(bad_spots)]

print(length(sec.sents))

sec.sents.df <- as.data.frame(sec.sents, stringsAsFactors = FALSE)
sec.sents <- sec.sents[sec.sents!="“..."]
sec.sents <- sec.sents[sec.sents!=""]
print(length(sec.sents))

sec.title <- rep("theSecretAdversary", 7809)
sec.sents.type <- rep("sentence", 7809)
sec.sents.counter<-seq(1, 7809)
sec.sents.id <- paste0("THE_SECRET_ADVERSARY_", "SENT_", sec.sents.counter)
sec.label <- rep("0", 7809)
print(length(sec.sents.id))

sec.sents.matrix <- cbind(sec.title, sec.sents.type, sec.sents.id, sec.sents, sec.label)
sec.sents.df <- as.data.frame(sec.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(sec.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", sec.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theSecretAdversary' LIMIT 2")
dbDisconnect(con)

# words.

sec.temp <- sec
sec.temp <- paste(sec.temp, collapse=" ")
sec.temp <-tolower(sec.temp)
# a better regex that is going to maintain contractions. important! 

sec.temp <- unlist(strsplit(sec.temp, "[^\\w’]", perl=TRUE))
sec.not.blanks <- which(sec.temp != "")
sec.words <- sec.temp[sec.not.blanks]
print(length(sec.words))

sec.words<- sec.words[which(sec.words!="^’")]
sec.words<- sec.words[which(sec.words!="’")]
print(length(sec.words))
sec.words[9999:10099]

sec.title <- rep("theSecretAdversary", 76046)
sec.words.type <- rep("word", 76046)
sec.words.counter <- seq(1, 76046)
sec.words.id <- paste0("THE_SECRET_ADVERSARY_", "WORD_", sec.words.counter)
sec.label<- rep("0", 76046)
sec.words.matrix <- cbind(sec.title, sec.words.type, sec.words.id, sec.words, sec.label)

sec.words.df <- as.data.frame(sec.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(sec.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", sec.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theSecretAdversary' LIMIT 10")
dbDisconnect(con)

# secret adversary finito.