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

## Woolf, Orlando. 
stock <- c("Title", "Type", "ID", "Unit", "Label")
orlando <- scan("rawTexts/lyrical/virginia-woolf-orlando.txt",what="character", sep="\n")
orlando.start <- which(orlando=="He--for there could be no doubt of his sex, though the fashion of the")
orlando.end<- which(orlando=="Eight.")
# no head matter 
orlando<-orlando[orlando.start:orlando.end]
# get rid of chapter markers 
orlando<- gsub('CHAPTER [0-9]\\.', "", orlando)
#https://rdrr.io/cran/qdap/man/replace_abbreviation.html
orlando <- replace_abbreviation(orlando)
orlando <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, orlando)

print(length(orlando))


first_bite <- orlando[1:2499]
second_bite<- orlando[2500:4999]
third_bite <- orlando[5000:6339]

orlando.sents.first <- paste0(first_bite, collapse = "\n")
orlando.sents.first <- unlist(tokenize_sentences(orlando.sents.first))

orlando.sents.second <- paste0(second_bite, collapse = "\n")
orlando.sents.second <- unlist(tokenize_sentences(orlando.sents.second))

orlando.sents.third <- paste0(third_bite, collapse = "\n")
orlando.sents.third <- unlist(tokenize_sentences(orlando.sents.third))

orlando.sents <- c(orlando.sents.first, orlando.sents.second, orlando.sents.third)

orlando.sents.df <- as.data.frame(orlando.sents, stringsAsFactors = FALSE)

# single quote connector loop

bad_spots <-c(0)
for(i in seq(1:length(orlando.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(orlando.sents[i], nchar(orlando.sents[i])-1, nchar(orlando.sents[i]))
  test2 <- substr(orlando.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      orlando.sents[i] <- paste(orlando.sents[i], orlando.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

orlando.sents[bad_spots]
orlando.sents <- orlando.sents[-c(bad_spots)]
print(length(orlando.sents))

orlando.sents.df <- as.data.frame(orlando.sents, stringsAsFactors = FALSE)

## no quotes constructor loop

bad_spots<-c(0)
orlando.sents[92:94]

## standalone, you need the third condition. 
for(i in seq(1:length(orlando.sents))){
  #if the sentence ends with a punctuation mark and the next sentence starts with a lowercase, combine them
  test <- substr(orlando.sents[i], nchar(orlando.sents[i]), nchar(orlando.sents[i]))
  test2 <- substr(orlando.sents[i+1], 1, 1)
  test3 <- substr(orlando.sents[i], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
    orlando.sents[i] <- paste(orlando.sents[i], orlando.sents[i+1])
    # print(orlando.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}
bad_spots <- bad_spots[-c(1)]
orlando.sents <- orlando.sents[-bad_spots]
print(length(orlando.sents))


orlando.title <- rep("orlando", 3292)
orlando.sents.type <- rep("sentence", 3292)
orlando.sents.counter<-seq(1, 3292)
orlando.sents.id <- paste0("ORLANDO_", "SENT_", orlando.sents.counter)
print(length(orlando.sents.id))
orlando.sents.matrix <- cbind(orlando.title, orlando.sents.type, orlando.sents.id, orlando.sents)
orlando.sents.df <- as.data.frame(orlando.sents.matrix, stringsAsFactors = FALSE)
colnames(orlando.sents.df) <- stock
# okay i think it's good now.
# orlando To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", orlando.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='orlando' LIMIT 2")
dbDisconnect(con)

### words. 


stock <- c("Title", "Type", "ID", "Unit")
orlando <- scan("rawTexts/virginia-woolf-orlando.txt",what="character", sep="\n")
orlando.start <- which(orlando=="He--for there could be no doubt of his sex, though the fashion of the")
orlando.end<- which(orlando=="Eight.")
# no head matter 
orlando<-orlando[orlando.start:orlando.end]
# get rid of chapter markers 
orlando<- gsub('CHAPTER [0-9]\\.', "", orlando)
#https://rdrr.io/cran/qdap/man/replace_abbreviation.html
orlando <- replace_abbreviation(orlando)
orlando <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, orlando)

print(length(orlando))
length(orlando)

orlando.not.blanks <- which(orlando != "")
orlando <- orlando[orlando.not.blanks]

orlando.temp <- orlando
orlando.temp <- paste(orlando.temp, collapse=" ")
orlando.temp <-tolower(orlando.temp)
# a better regex that is going to maintain contractions. important! 

orlando.temp <- unlist(strsplit(orlando.temp, "[^\\w']", perl=TRUE))
orlando.not.blanks <- which(orlando.temp != "")
orlando.words <- orlando.temp[orlando.not.blanks]

print(length(orlando.words))

orlando.title <- rep("orlando", 79547)
orlando.words.type <- rep("word", 79547)
orlando.words.counter <- seq(1, 79547)
orlando.words.id <- paste0("ORLANDO_", "WORD_", orlando.words.counter)

orlando.words.matrix <- cbind(orlando.title, orlando.words.type, orlando.words.id, orlando.words)

orlando.words.df <- as.data.frame(orlando.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(orlando.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", orlando.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='orlando' LIMIT 10")
dbDisconnect(con)

####### paragraphs. ####

orlando.paragraphs <- read.csv("Python_Scripts/checkCorpus/ORLANDO_paras.csv", stringsAsFactors = FALSE)

orlando.paragraphs <- orlando.paragraphs[-c(1:18, 470:473),]
colnames(orlando.paragraphs) <- c("arb", "paras")

# cleanzo

orlando.paragraphs <- orlando.paragraphs %>%
  transmute(paragraph = gsub('CHAPTER [0-9]\\.', '', perl=TRUE, paras))

orlando.paragraphs <- orlando.paragraphs %>%
  transmute(paras = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, paragraph))

orlando.paragraphs <- orlando.paragraphs %>%
  transmute(paragraphs = replace_abbreviation(paras))

print(length(orlando.paragraphs$paragraphs))

orlando.title <- rep("orlando", 451)
orlando.para.type <- rep("paragraph", 451)
orlando.para.counter<-seq(1, 451)
orlando.label <- rep("1", 451)
orlando.para.id <- paste0("ORLANDO_", "PARAGRAPH_", orlando.para.counter)
print(length(orlando.para.id))
orlando.para.matrix <- cbind(orlando.title, orlando.para.type, orlando.para.id, orlando.paragraphs, orlando.label)
orlando.para.df <- as.data.frame(orlando.para.matrix, stringsAsFactors = FALSE)
colnames(orlando.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", orlando.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='orlando' LIMIT 2")
dbDisconnect(con)
### orlandone.






