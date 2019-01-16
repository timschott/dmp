setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

stock <- c("Title", "Type", "ID", "Unit", "Label")
sea <- scan("rawTexts/lyrical/Jean-Rhys-Wide-Sargasso-Sea.txt",what="character", sep="\n")
sea.start <- which(sea=="They say when trouble come close ranks, and so the white people did. But we were not in their ranks. The Jamaican ladies had never approved of my mother, ‘because she pretty like pretty self’ Christophine said.")
sea.end<- which(sea=="Grace Poole was sitting at the table but she had heard the scream too, for she said, ‘What was that?’ She got up, came over and looked at me. I lay still, breathing evenly with my eyes shut. ‘I must have been dreaming,’ she said. Then she went back, not to the table but to her bed. I waited a long time after I heard her snore, then I got up, took the keys and unlocked the door. I was outside holding my candle. Now at last I know why I was brought here and what I have to do. There must have been a draught for the flame flickered and I thought it was out. But I shielded it with my hand and it burned up again to light me along the dark passage.")
sea <- sea[sea.start:sea.end]
print(length(sea))
grep("Part Two", sea)
grep("Part three", sea)
sea <- sea[-c(254, 1111)]
sea <- gsub("Mr\\.", "Mr", sea)
sea <- gsub("Mrs\\.", "Mrs", sea)
print(length(sea))
sea <- sea[sea!=""]
print(length(sea))
sea.paragraphs <- sea
sea.title <- rep("wideSargassoSea", 1175)
sea.para.type <- rep("paragraph", 1175)
sea.para.counter<-seq(1, 1175)
sea.label <- rep("1", 1175)
sea.para.id <- paste0("WIDE_SARGASSO_SEA_", "PARAGRAPH_", sea.para.counter)
print(length(sea.para.id))

sea.para.matrix <- cbind(sea.title, sea.para.type, sea.para.id, sea.paragraphs, sea.label)
sea.para.df <- as.data.frame(sea.para.matrix, stringsAsFactors = FALSE)
colnames(sea.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", sea.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='wideSargassoSea' LIMIT 2")
dbDisconnect(con)

## sents.
first_bite <- sea[1:1175]

sea.sents.first <- paste0(first_bite, collapse = "\n")
sea.sents.first <- unlist(tokenize_sentences(sea.sents.first))

sea.sents <- c(sea.sents.first)

sea.sents.df <- as.data.frame(sea.sents, stringsAsFactors = FALSE)


bad_spots <-c(0)
for(i in seq(1:length(sea.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(sea.sents[i], nchar(sea.sents[i]), nchar(sea.sents[i]))
  test2 <- substr(sea.sents[i+1], 1, 1)
  test3 <- substr(sea.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      sea.sents[i] <- paste(sea.sents[i], sea.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

sea.sents <- sea.sents[-c(bad_spots)]
print(length(sea.sents))


bad_spots <-c(0)
for(i in seq(1:length(sea.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(sea.sents[i], nchar(sea.sents[i])-1, nchar(sea.sents[i]))
  test2 <- substr(sea.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?’", "!’") && test2==tolower(test2)){
      sea.sents[i] <- paste(sea.sents[i], sea.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

sea.sents[bad_spots]
sea.sents <- sea.sents[-c(bad_spots)]
print(length(sea.sents))

# cool. 

sea.title <- rep("wideSargassoSea", 4278)
sea.sents.type <- rep("sentence", 4278)
sea.sents.counter<-seq(1, 4278)
sea.sents.id <- paste0("WIDE_SARGASSO_SEA_", "SENT_", sea.sents.counter)
sea.label <- rep("1", 4278)
print(length(sea.sents.id))

sea.sents.matrix <- cbind(sea.title, sea.sents.type, sea.sents.id, sea.sents, sea.label)
sea.sents.df <- as.data.frame(sea.sents.matrix, stringsAsFactors = FALSE)
colnames(sea.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", sea.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='wideSargassoSea' LIMIT 2")
dbDisconnect(con)

###
sea.temp <- sea
sea.temp <- paste(sea.temp, collapse=" ")
sea.temp <-tolower(sea.temp)
# a better regex that is going to maintain contractions. important! 

sea.temp <- unlist(strsplit(sea.temp, "[^\\w’]", perl=TRUE))
sea.not.blanks <- which(sea.temp != "")
sea.words <- sea.temp[sea.not.blanks]
print(length(sea.words))

sea.words<- sea.words[which(sea.words!="")]
print(length(sea.words))
grep("", sea.words)
sea.words[23392]
sea.words<- sea.words[which(sea.words!="’")]
print(length(sea.words))

sea.title <- rep("wideSargassoSea", 47349)
sea.words.type <- rep("word", 47349)
sea.words.counter <- seq(1, 47349)
sea.words.id <- paste0("WIDE_SARGASSO_SEA_", "WORD_", sea.words.counter)
sea.label <- rep("1", 47349)
sea.words.matrix <- cbind(sea.title, sea.words.type, sea.words.id, sea.words, sea.label)

sea.words.df <- as.data.frame(sea.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(sea.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", sea.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='wideSargassoSea' LIMIT 10")
dbDisconnect(con)
# sick! 