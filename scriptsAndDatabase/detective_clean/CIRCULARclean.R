setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

circ <- scan("rawTexts/detective/mary-roberts-rinehart-the-circular-staircase.txt",what="character",sep="\n")
circ.start <- which(circ=="This is the story of how a middle-aged spinster lost her mind, deserted her domestic gods in the city, took a furnished house for the summer out of town, and found herself involved in one of those mysterious crimes that keep our newspapers and detective agencies happy and prosperous. For twenty years I had been perfectly comfortable; for twenty years I had had the window-boxes filled in the spring, the carpets lifted, the awnings put up and the furniture covered with brown linen; for as many summers I had said good-by to my friends, and, after watching their perspiring hegira, had settled down to a delicious quiet in town, where the mail comes three times a day, and the water supply does not depend on a tank on the roof.")
circ.fin <- which(circ=="So we sit and talk, and sometimes Liddy threatens to leave, and often I discharge her, but we stay together somehow. I am talking of renting a house next year, and Liddy says to be sure there is no ghost. To be perfectly frank, I never really lived until that summer. Time has passed since I began this story. My neighbors are packing up for another summer. Liddy is having the awnings put up, and the window boxes filled. Liddy or no Liddy, I shall advertise to-morrow for a house in the country, and I don't care if it has a Circular Staircase.")
circ <- circ[circ.start:circ.fin]
spots <- grep('[A-Z]{2,}[^a-z]', circ)
spots <- spots[-c(5,10,11,18,19,22,27,42,47,50,59,60,76,79,82)]
# circ[spots]
circ <- circ[-spots]

circ.paragraphs <- as.data.frame(circ, stringsAsFactors=FALSE)

colnames(circ.paragraphs) <- c("paras")
circ.paragraphs <- circ.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

colnames(circ.paragraphs) <- c("paras")


circ.paragraphs<- circ.paragraphs %>%
  transmute(paragraphs=gsub("\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

circ.paragraphs <- circ.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

circ.paragraphs <- circ.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

circ.paragraphs <- circ.paragraphs %>% 
  filter(paragraphs!="")

circ.paragraphs <- circ.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(circ.paragraphs)

circ.paragraphs <- circ.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

circ.paragraphs <- circ.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(circ.paragraphs$paragraphs))

circ.paragraphs <- circ.paragraphs %>% 
  filter(paragraphs!="")

circ.paragraphs <- circ.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(circ.paragraphs$paragraphs))

circ.paragraphs <- circ.paragraphs %>% 
  transmute(paras=  gsub("\"", "", paragraphs))

circ.title <- rep("thecircularStaircase", 1623)
circ.para.type <- rep("paragraph",1623)
circ.para.counter<-seq(1, 1623)
circ.para.id <- paste0("THE_circULAR_STAIRCASE_", "PARAGRAPH_", circ.para.counter)
circ.label <- rep("0", 1623)
print(length(circ.para.id))

circ.para.matrix <- cbind(circ.title, circ.para.type, circ.para.id, circ.paragraphs, circ.label)
circ.para.df <- as.data.frame(circ.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(circ.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", circ.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='thecircularStaircase' LIMIT 2")
dbDisconnect(con)

# sents.

circ <- circ.paragraphs$paras

first_bite <- circ[1:1623]

circ.sents.first <- paste0(first_bite, collapse = "\n")
circ.sents.first <- unlist(tokenize_sentences(circ.sents.first))

circ.sents <- c(circ.sents.first)
circ.sents.df <- as.data.frame(circ.sents, stringsAsFactors = FALSE)

print(length(circ.sents.df$circ.sents))

bad_spots <-c(0)
for(i in seq(1:length(circ.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(circ.sents[i], nchar(circ.sents[i]), nchar(circ.sents[i]))
  test2 <- substr(circ.sents[i+1], 1, 1)
  test3 <- substr(circ.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      circ.sents[i] <- paste(circ.sents[i], circ.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
circ.sents[bad_spots]
circ.sents <- circ.sents[-c(bad_spots)]
circ.sents <- circ.sents[circ.sents!=""]
print(length(circ.sents))

circ.sents[3333:3355]
bad_spots <-c(0)
for(i in seq(1:length(circ.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(circ.sents[i], nchar(circ.sents[i])-1, nchar(circ.sents[i]))
  test2 <- substr(circ.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      circ.sents[i] <- paste(circ.sents[i], circ.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
circ.sents[bad_spots]
circ.sents <- circ.sents[-c(bad_spots)]
print(length(circ.sents))
circ.title <- rep("theCircularStaircase", 4797)
circ.sents.type <- rep("sentence", 4797)
circ.sents.counter<-seq(1, 4797)
circ.sents.id <- paste0("THE_CIRCULAR_STAIRCASE_", "SENT_", circ.sents.counter)
circ.label <- rep("0", 4797)
print(length(circ.sents.id))

circ.sents.matrix <- cbind(circ.title, circ.sents.type, circ.sents.id, circ.sents, circ.label)
circ.sents.df <- as.data.frame(circ.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(circ.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", circ.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theCircularStaircase' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Type='sentence' AND Title='theCircularStaircase'")
dbDisconnect(con)

# words. 
circ.temp <- circ
circ.temp <- paste(circ.temp, collapse=" ")
circ.temp <-tolower(circ.temp)
# a better regex that is going to maintain contractions. important! 

circ.temp <- unlist(strsplit(circ.temp, "[^\\w']", perl=TRUE))
circ.not.blanks <- which(circ.temp != "")
circ.words <- circ.temp[circ.not.blanks]
print(length(circ.words))
circ.words<- circ.words[which(circ.words!="^'")]
circ.words<- circ.words[which(circ.words!="'")]
print(length(circ.words))
circ.words.df <- as.data.frame(circ.words, stringsAsFactors = FALSE)
circ.title <- rep("theCircularStaircase", 70631)
circ.words.type <- rep("word", 70631)
circ.words.counter <- seq(1, 70631)
circ.words.id <- paste0("THE_CIRCULAR_STAIRCASE_", "WORD_", circ.words.counter)
circ.label<- rep("0", 70631)
circ.words.matrix <- cbind(circ.title, circ.words.type, circ.words.id, circ.words, circ.label)

circ.words.df <- as.data.frame(circ.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(circ.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", circ.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theCircularStaircase' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
