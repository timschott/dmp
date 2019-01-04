setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
rm(list=ls())
pit <- scan("rawTexts/detective/arthur-j-rees-the-shrieking-pit.txt",what="character",sep="\n")
pit.start <- which(pit=="Colwyn had never seen anything quite so eccentric in a public room as the behaviour of the young man breakfasting alone at the alcove table in the bay embrasure, and he became so absorbed in watching him that he permitted his own meal to grow cold, impatiently waving away the waiter who sought with obtrusive obsequiousness to recall his wandering attention by thrusting the menu card before him.")
pit.end <- 1878
pit<-pit[pit.start:pit.end]

pit.paragraphs <- as.data.frame(pit, stringsAsFactors=FALSE)
colnames(pit.paragraphs) <- c("paras")

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paras=  gsub("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paragraphs=  gsub("CHAPTER I", "", paras))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paras=  gsub("\\*", "", paragraphs))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paragraphs = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paras))

pit.paragraphs <- pit.paragraphs %>% 
  filter(paragraphs!="")

pit.paragraphs <- pit.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(pit.paragraphs$paragraphs))

# database time. 
pit.title <- rep("theShriekingPit", 1740)
pit.para.type <- rep("paragraph",1740)
pit.para.counter<-seq(1, 1740)
pit.para.id <- paste0("THE_SHRIEKING_PIT_", "PARAGRAPH_", pit.para.counter)
pit.label <- rep("0", 1740)
print(length(pit.para.id))

pit.para.matrix <- cbind(pit.title, pit.para.type, pit.para.id, pit.paragraphs, pit.label)
pit.para.df <- as.data.frame(pit.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(pit.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", pit.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theShriekingPit' LIMIT 2")
dbDisconnect(con)
# dbExecute(con, "DELETE FROM textTable WHERE Type='paragraph' OR Type= 'sentence' AND Title='theShriekingPit'")

# sents 

pit <- pit.paragraphs$paragraphs

first_bite <- pit[1:1740]

pit.sents.first <- paste0(first_bite, collapse = "\n")
pit.sents.first <- unlist(tokenize_sentences(pit.sents.first))

pit.sents <- c(pit.sents.first)
pit.sents.df <- as.data.frame(pit.sents, stringsAsFactors = FALSE)

print(length(pit.sents.df$pit.sents))


bad_spots <-c(0)
for(i in seq(1:length(pit.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(pit.sents[i], nchar(pit.sents[i]), nchar(pit.sents[i]))
  test2 <- substr(pit.sents[i+1], 1, 1)
  test3 <- substr(pit.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      pit.sents[i] <- paste(pit.sents[i], pit.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
pit.sents[bad_spots]
pit.sents <- pit.sents[-c(bad_spots)]


bad_spots <-c(0)
for(i in seq(1:length(pit.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(pit.sents[i], nchar(pit.sents[i])-1, nchar(pit.sents[i]))
  test2 <- substr(pit.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if((test %in% c("?'", "!'") && test2==tolower(test2) && test2!='I')|| (test %in% c('?”', '!”') && test2=='I')){
      #print(i)
      pit.sents[i] <- paste(pit.sents[i], pit.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

pit.sents[bad_spots]
pit.sents <- pit.sents[-c(bad_spots)]

pit.sents.df <- as.data.frame(pit.sents, stringsAsFactors = FALSE)
pit.sents <- pit.sents[pit.sents!=""]
print(length(pit.sents))

pit.title <- rep("theShriekingPit", 5194)
pit.sents.type <- rep("sentence", 5194)
pit.sents.counter<-seq(1, 5194)
pit.sents.id <- paste0("THE_SHRIEKING_PIT_", "SENT_", pit.sents.counter)
pit.label <- rep("0", 5194)
print(length(pit.sents.id))

pit.sents.matrix <- cbind(pit.title, pit.sents.type, pit.sents.id, pit.sents, pit.label)
pit.sents.df <- as.data.frame(pit.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(pit.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", pit.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theShriekingPit' LIMIT 2")
dbDisconnect(con)

# words.

pit.temp <- pit
pit.temp <- paste(pit.temp, collapse=" ")
pit.temp <-tolower(pit.temp)
# a better regex that is going to maintain contractions. important! 

pit.temp <- unlist(strsplit(pit.temp, "[^\\w']", perl=TRUE))
pit.not.blanks <- which(pit.temp != "")
pit.words <- pit.temp[pit.not.blanks]
print(length(pit.words))

pit.words<- pit.words[which(pit.words!="'")]
print(length(pit.words))
pit.words<-gsub("^'","", pit.words)
pit.words<-gsub("_morale_","morale", pit.words)
print(length(pit.words))
pit.title <- rep("theShriekingPit", 100006)
pit.words.type <- rep("word", 100006)
pit.words.counter <- seq(1, 100006)
pit.words.id <- paste0("THE_SHRIEKING_PIT_", "WORD_", pit.words.counter)
pit.label<- rep("0", 100006)
pit.words.matrix <- cbind(pit.title, pit.words.type, pit.words.id, pit.words, pit.label)

pit.words.df <- as.data.frame(pit.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(pit.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", pit.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theShriekingPit' LIMIT 10")
dbDisconnect(con)
# pit done.




