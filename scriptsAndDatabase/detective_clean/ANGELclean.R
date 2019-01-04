setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

# let's make one big regex..
rm(list=ls())
angel <- scan("rawTexts/detective/edgar-wallace-the-angel-of-terror.txt",what="character",sep="\n")
angel.start <- which(angel=="The hush of the court, which had been broken when the foreman of the jury returned their verdict, was intensified as the Judge, with a quick glance over his pince-nez at the tall prisoner, marshalled his papers with the precision and method which old men display in tense moments such as these. He gathered them together, white paper and blue and buff and stacked them in a neat heap on a tiny ledge to the left of his desk. Then he took his pen and wrote a few words on a printed paper before him.")
angel.end <- which(angel=="But Marcus Stepney did not go alone. For the last two miles of the journey he had carried a bag containing the greater part of five million francs that the girl had brought from the boat. Jean did not remember this until she was on her way to the city of the hills, and by that time money did not interest her.")
angel<-angel[angel.start:angel.end]
angel.paragraphs <- as.data.frame(angel, stringsAsFactors=FALSE)
colnames(angel.paragraphs) <- c("paras")

chaps <- grep('Chapter', angel.paragraphs$paras)
angel.paragraphs <- as.data.frame(angel.paragraphs[-c(chaps)], stringsAsFactors=FALSE)
colnames(angel.paragraphs) <- c("paras")
angel.paragraphs <- angel.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

colnames(angel.paragraphs) <- c("paras")

angel.paragraphs<- angel.paragraphs %>%
  transmute(paragraphs=gsub("\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

angel.paragraphs <- angel.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

angel.paragraphs <- angel.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

angel.paragraphs <- angel.paragraphs %>% 
  filter(paragraphs!="")

angel.paragraphs <- angel.paragraphs %>% 
  filter(paragraphs!="  ")

## 

print(length(angel.paragraphs$paragraphs))

angel.title <- rep("theAngelOfTerror", 2429)
angel.para.type <- rep("paragraph",2429)
angel.para.counter<-seq(1, 2429)
angel.para.id <- paste0("THE_ANGEL_OF_TERROR_", "PARAGRAPH_", angel.para.counter)
angel.label <- rep("0", 2429)
print(length(angel.para.id))

angel.para.matrix <- cbind(angel.title, angel.para.type, angel.para.id, angel.paragraphs, angel.label)
angel.para.df <- as.data.frame(angel.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(angel.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", angel.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theAngelOfTerror' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Type='paragraph' OR Type ='sentence' AND Title='theAngelOfTerror'")
dbDisconnect(con)


# sents.
angel <- angel.paragraphs$paragraphs

first_bite <- angel[1:2429]

angel.sents.first <- paste0(first_bite, collapse = "\n")
angel.sents.first <- unlist(tokenize_sentences(angel.sents.first))

angel.sents <- c(angel.sents.first)
angel.sents.df <- as.data.frame(angel.sents, stringsAsFactors = FALSE)

print(length(angel.sents.df$angel.sents))


bad_spots <-c(0)
for(i in seq(1:length(angel.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a caangelal letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(angel.sents[i], nchar(angel.sents[i]), nchar(angel.sents[i]))
  test2 <- substr(angel.sents[i+1], 1, 1)
  test3 <- substr(angel.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if((test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3))){
      #print(i)
      angel.sents[i] <- paste(angel.sents[i], angel.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
angel.sents[bad_spots]
angel.sents <- angel.sents[-c(bad_spots)]

print(length(angel.sents))

bad_spots <-c(0)
for(i in seq(1:length(angel.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(angel.sents[i], nchar(angel.sents[i])-1, nchar(angel.sents[i]))
  test2 <- substr(angel.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      angel.sents[i] <- paste(angel.sents[i], angel.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
angel.sents[bad_spots]
angel.sents <- angel.sents[-c(bad_spots)]

angel.sents <- angel.sents[angel.sents!=""]
print(length(angel.sents))

angel.title <- rep("theAngelOfTerror", 4413)
angel.sents.type <- rep("sentence", 4413)
angel.sents.counter<-seq(1, 4413)
angel.sents.id <- paste0("THE_ANGEL_OF_TERROR_", "SENT_", angel.sents.counter)
angel.label <- rep("0", 4413)
print(length(angel.sents.id))

angel.sents.matrix <- cbind(angel.title, angel.sents.type, angel.sents.id, angel.sents, angel.label)
angel.sents.df <- as.data.frame(angel.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(angel.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", angel.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theAngelOfTerror' LIMIT 2")
dbDisconnect(con)

angel.temp <- angel
angel.temp <- paste(angel.temp, collapse=" ")
angel.temp <-tolower(angel.temp)
# a better regex that is going to maintain contractions. important! 

angel.temp <- unlist(strsplit(angel.temp, "[^\\w']", perl=TRUE))
angel.not.blanks <- which(angel.temp != "")
angel.words <- angel.temp[angel.not.blanks]
print(length(angel.words))

angel.words<- angel.words[which(angel.words!="'")]
print(length(angel.words))
angel.words<-gsub("^'","", angel.words)
print(length(angel.words))

angel.title <- rep("theAngelOfTerror", 63264)
angel.words.type <- rep("word", 63264)
angel.words.counter <- seq(1, 63264)
angel.words.id <- paste0("THE_ANGEL_OF_TERROR_", "WORD_", angel.words.counter)
angel.label<- rep("0", 63264)
angel.words.matrix <- cbind(angel.title, angel.words.type, angel.words.id, angel.words, angel.label)

angel.words.df <- as.data.frame(angel.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(angel.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", angel.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theAngelOfTerror' LIMIT 10")
dbDisconnect(con)
# angel done.