setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

# just do the paragraphs first then distill down. 

room.paragraphs <- read.csv("Python_Scripts/checkCorpus/75_paras.csv", stringsAsFactors = FALSE)
room.paragraphs$X0[20]
room.paragraphs <- room.paragraphs[-c(1:15,774:777),]
colnames(room.paragraphs) <- c("arb", "paragraphs")

room.paragraphs <- room.paragraphs %>% 
  transmute(paras=  gsub("\n", " ", paragraphs) )

room.paragraphs<- room.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

room.paragraphs <- room.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

room.paragraphs <- room.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

room.paragraphs <- room.paragraphs %>% 
  transmute(paras=  gsub("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs) )

room.paragraphs <- room.paragraphs %>% 
  filter(paras!="")

room.paragraphs <- room.paragraphs %>% 
  filter(paras!="  ")
colnames(room.paragraphs)

room.paragraphs <- room.paragraphs %>% 
  transmute(paragraphs = replace_abbreviation(paras))

room.paragraphs <- room.paragraphs %>% 
  transmute(paras=  gsub("MR\\.", "Mr", paragraphs))

print(length(room.paragraphs$paras))

room.paragraphs <- room.paragraphs %>% 
  filter(paras!="")

room.paragraphs <- room.paragraphs %>% 
  filter(paras!="  ")
room.paragraphs <- room.paragraphs %>% 
  transmute(paragraphs=  gsub("_", "", paras))
print(length(room.paragraphs$paragraphs))
room.paragraphs$paragraphs[555:568]

print(length(room.paragraphs$paragraphs))

room.title <- rep("theMysteryOfRoom75", 758)
room.para.type <- rep("paragraph",758)
room.para.counter<-seq(1, 758)
room.para.id <- paste0("THE_MYSTERY_OF_ROOM_75_", "PARAGRAPH_", room.para.counter)
room.label <- rep("0", 758)
print(length(room.para.id))

room.para.matrix <- cbind(room.title, room.para.type, room.para.id, room.paragraphs, room.label)
room.para.df <- as.data.frame(room.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(room.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", room.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theMysteryOfRoom75' LIMIT 2")
dbDisconnect(con)

room <- room.paragraphs$paragraphs

# sents
first_bite <- room[1:758]
first_bite <- gsub("No\\.", "No", perl=TRUE, first_bite)

room.sents.first <- paste0(first_bite, collapse = "\n")
room.sents.first <- unlist(tokenize_sentences(room.sents.first))

room.sents <- c(room.sents.first)
room.sents.df <- as.data.frame(room.sents, stringsAsFactors = FALSE)

print(length(room.sents.df$room.sents))

bad_spots <-c(0)
for(i in seq(1:length(room.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(room.sents[i], nchar(room.sents[i]), nchar(room.sents[i]))
  test2 <- substr(room.sents[i+1], 1, 1)
  test3 <- substr(room.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      room.sents[i] <- paste(room.sents[i], room.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
room.sents[bad_spots]
room.sents <- room.sents[-c(bad_spots)]
room.sents <- room.sents[room.sents!=""]
print(length(room.sents))

room.sents[1999:2005]

room.title <- rep("theMysteryOfRoom75", 2819)
room.sents.type <- rep("sentence", 2819)
room.sents.counter<-seq(1, 2819)
room.sents.id <- paste0("THE_MYSTERY_OF_ROOM_75_", "SENT_", room.sents.counter)
room.label <- rep("0", 2819)
print(length(room.sents.id))

room.sents.matrix <- cbind(room.title, room.sents.type, room.sents.id, room.sents, room.label)
room.sents.df <- as.data.frame(room.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(room.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", room.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theMysteryOfRoom75' LIMIT 2")
dbDisconnect(con)
# words. 
room.temp <- room
room.temp <- paste(room.temp, collapse=" ")
room.temp <-tolower(room.temp)
# a better regex that is going to maintain contractions. important! 

room.temp <- unlist(strsplit(room.temp, "[^\\w']", perl=TRUE))
room.not.blanks <- which(room.temp != "")
room.words <- room.temp[room.not.blanks]
print(length(room.words))
room.words<- room.words[which(room.words!="^'")]
room.words<- room.words[which(room.words!="'")]
room.words<- room.words[which(room.words!="''")]
print(length(room.words))
room.words.df <- as.data.frame(room.words, stringsAsFactors = FALSE)

room.title <- rep("theMysteryOfRoom75", 48754)
room.words.type <- rep("word", 48754)
room.words.counter <- seq(1, 48754)
room.words.id <- paste0("THE_MYSTERY_OF_ROOM_75_", "WORD_", room.words.counter)
room.label<- rep("0", 48754)
room.words.matrix <- cbind(room.title, room.words.type, room.words.id, room.words, room.label)

room.words.df <- as.data.frame(room.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(room.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", room.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theMysteryOfRoom75' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
