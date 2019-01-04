setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

brown <- scan("rawTexts/detective/gk-chesterton-the-innocence-of-father-brown.txt", what="character", sep="\n")
print(length(brown))

brown.paragraphs <- as.data.frame(brown, stringsAsFactors=FALSE)
colnames(brown.paragraphs) <- c("paras")

brown.paragraphs <- brown.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

brown.paragraphs <- brown.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs))

brown.paragraphs <- brown.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "", paras))

brown.paragraphs <- brown.paragraphs %>% 
  transmute(paras=  gsub("\\*", "", paragraphs))

brown.paragraphs <- brown.paragraphs %>% 
  transmute(paragraphs = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paras))

brown.paragraphs <- brown.paragraphs %>% 
  filter(paragraphs!="")

brown.paragraphs <- brown.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(brown.paragraphs$paragraphs))

brown.title <- rep("theInnocenceOfFatherBrown", 1483)
brown.para.type <- rep("paragraph",1483)
brown.para.counter<-seq(1, 1483)
brown.para.id <- paste0("THE_INNOCENCE_OF_FATHER_BROWN_", "PARAGRAPH_", brown.para.counter)
brown.label <- rep("0", 1483)
print(length(brown.para.id))

brown.para.matrix <- cbind(brown.title, brown.para.type, brown.para.id, brown.paragraphs, brown.label)
brown.para.df <- as.data.frame(brown.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(brown.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", brown.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theInnocenceOfFatherBrown' LIMIT 2")
dbDisconnect(con)

# sentences 
brown <- brown.paragraphs$paragraphs

first_bite <- brown[1:1483]

brown.sents.first <- paste0(first_bite, collapse = "\n")
brown.sents.first <- unlist(tokenize_sentences(brown.sents.first))

brown.sents <- c(brown.sents.first)
brown.sents.df <- as.data.frame(brown.sents, stringsAsFactors = FALSE)

print(length(brown.sents.df$brown.sents))

bad_spots <-c(0)
for(i in seq(1:length(brown.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(brown.sents[i], nchar(brown.sents[i]), nchar(brown.sents[i]))
  test2 <- substr(brown.sents[i+1], 1, 1)
  test3 <- substr(brown.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      brown.sents[i] <- paste(brown.sents[i], brown.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
brown.sents[bad_spots]
brown.sents <- brown.sents[-c(bad_spots)]

print(length(brown.sents))

bad_spots <-c(0)
for(i in seq(1:length(brown.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(brown.sents[i], nchar(brown.sents[i])-1, nchar(brown.sents[i]))
  test2 <- substr(brown.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if((test %in% c("?”", "!”") && test2==tolower(test2))){
      #print(i)
      brown.sents[i] <- paste(brown.sents[i], brown.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
brown.sents[bad_spots]
brown.sents <- brown.sents[-c(bad_spots)]
brown.sents <- brown.sents[brown.sents!=""]
print(length(brown.sents))

brown.title <- rep("theInnocenceOfFatherBrown", 4260)
brown.sents.type <- rep("sentence", 4260)
brown.sents.counter<-seq(1, 4260)
brown.sents.id <- paste0("THE_INNOCENCE_OF_FATHER_BROWN_", "SENT_", brown.sents.counter)
brown.label <- rep("0", 4260)
print(length(brown.sents.id))

brown.sents.matrix <- cbind(brown.title, brown.sents.type, brown.sents.id, brown.sents, brown.label)
brown.sents.df <- as.data.frame(brown.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(brown.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", brown.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theInnocenceOfFatherBrown' LIMIT 2")
dbDisconnect(con)


# okay. words.
brown.temp <- brown
brown.temp <- paste(brown.temp, collapse=" ")
brown.temp <-tolower(brown.temp)
# a better regex that is going to maintain contractions. important! 

brown.temp <- unlist(strsplit(brown.temp, "[^\\w’]", perl=TRUE))
brown.not.blanks <- which(brown.temp != "")
brown.words <- brown.temp[brown.not.blanks]
print(length(brown.words))

brown.words<- brown.words[which(brown.words!="’")]
print(length(brown.words))

brown.title <- rep("theInnocenceOfFatherBrown", 79438)
brown.words.type <- rep("word", 79438)
brown.words.counter <- seq(1, 79438)
brown.words.id <- paste0("THE_INNOCENCE_OF_FATHER_BROWN_", "WORD_", brown.words.counter)
brown.label<- rep("0", 79438)
brown.words.matrix <- cbind(brown.title, brown.words.type, brown.words.id, brown.words, brown.label)

brown.words.df <- as.data.frame(brown.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(brown.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", brown.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theInnocenceOfFatherBrown' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
