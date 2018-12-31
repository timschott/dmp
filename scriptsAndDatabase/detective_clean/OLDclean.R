setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

old <- scan("rawTexts/detective/emmuska-orczy-the-old-man-in-the-corner.txt",what="character",sep="\n")
old.start <- which(old =="The man in the corner pushed aside his glass, and leant across the table.")
old.fin<-which(old=="\"He has disappeared off the face of the earth. The police are searching for him, and perhaps some day they will find him—then society will be rid of one of the most ingenious men of the age.\"")
old <- old[old.start:old.fin]

spots <- grep('[A-Z]{2,}[^a-z]', old)
spots <- spots[-c(23,48)]
old <- old[-spots]


old.paragraphs <- as.data.frame(old, stringsAsFactors=FALSE)
colnames(old.paragraphs) <- c("paras")

old.paragraphs<- old.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

old.paragraphs <- old.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

old.paragraphs <- old.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

old.paragraphs <- old.paragraphs %>% 
  filter(paragraphs!="")

old.paragraphs <- old.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(old.paragraphs)

old.paragraphs <- old.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

old.paragraphs <- old.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(old.paragraphs$paragraphs))

old.paragraphs <- old.paragraphs %>% 
  filter(paragraphs!="")

old.paragraphs <- old.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(old.paragraphs$paragraphs))

grep("<", old.paragraphs$paragraphs)

old.paragraphs <- old.paragraphs %>% 
  transmute(paras=  gsub("<I>|</I>", "", paragraphs) )
old.paragraphs$paras[7]

# to db. 
old.title <- rep("theOldManInTheCorner", 1308)
old.para.type <- rep("paragraph",1308)
old.para.counter<-seq(1, 1308)
old.para.id <- paste0("THE_OLD_MAN_IN_THE_CORNER_", "PARAGRAPH_", old.para.counter)
old.label <- rep("0", 1308)
print(length(old.para.id))

old.para.matrix <- cbind(old.title, old.para.type, old.para.id, old.paragraphs, old.label)
old.para.df <- as.data.frame(old.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(old.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", old.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theOldManInTheCorner' LIMIT 2")
dbDisconnect(con)

# sents.
old <- old.paragraphs$paras

first_bite <- old[1:1308]

old.sents.first <- paste0(first_bite, collapse = "\n")
old.sents.first <- unlist(tokenize_sentences(old.sents.first))

old.sents <- c(old.sents.first)
old.sents.df <- as.data.frame(old.sents, stringsAsFactors = FALSE)

print(length(old.sents.df$old.sents))

bad_spots <-c(0)
for(i in seq(1:length(old.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(old.sents[i], nchar(old.sents[i]), nchar(old.sents[i]))
  test2 <- substr(old.sents[i+1], 1, 1)
  test3 <- substr(old.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      old.sents[i] <- paste(old.sents[i], old.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
old.sents[bad_spots]
old.sents <- old.sents[-c(bad_spots)]

bad_spots <-c(0)
for(i in seq(1:length(old.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(old.sents[i], nchar(old.sents[i])-1, nchar(old.sents[i]))
  test2 <- substr(old.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if((test %in% c("?'", "!'") && test2==tolower(test2))){
      #print(i)
      old.sents[i] <- paste(old.sents[i], old.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

old.sents[bad_spots]
old.sents <- old.sents[-c(bad_spots)]
old.sents <- old.sents[old.sents!=""]
print(length(old.sents))

old.title <- rep("theOldManInTheCorner", 2934)
old.sents.type <- rep("sentence", 2934)
old.sents.counter<-seq(1, 2934)
old.sents.id <- paste0("THE_OLD_MAN_IN_THE_CORNER_", "SENT_", old.sents.counter)
old.label <- rep("0", 2934)
print(length(old.sents.id))

old.sents.matrix <- cbind(old.title, old.sents.type, old.sents.id, old.sents, old.label)
old.sents.df <- as.data.frame(old.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(old.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", old.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theOldManInTheCorner' LIMIT 2")
dbDisconnect(con)

# words. 
old.temp <- old
old.temp <- paste(old.temp, collapse=" ")
old.temp <-tolower(old.temp)
# a better regex that is going to maintain contractions. important! 

old.temp <- unlist(strsplit(old.temp, "[^\\w']", perl=TRUE))
old.not.blanks <- which(old.temp != "")
old.words <- old.temp[old.not.blanks]
print(length(old.words))
old.words<- old.words[which(old.words!="^'")]
old.words<- old.words[which(old.words!="'")]
print(length(old.words))
old.words.df <- as.data.frame(old.words, stringsAsFactors = FALSE)
grep("^'", old.words)
old.words <- gsub("^'", "", old.words)


old.words.df <- as.data.frame(old.words, stringsAsFactors = FALSE)

old.title <- rep("theOldManInTheCorner", 69944)
old.words.type <- rep("word", 69944)
old.words.counter <- seq(1, 69944)
old.words.id <- paste0("THE_OLD_MAN_IN_THE_CORNER_", "WORD_", old.words.counter)
old.label<- rep("0", 69944)
old.words.matrix <- cbind(old.title, old.words.type, old.words.id, old.words, old.label)

old.words.df <- as.data.frame(old.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(old.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", old.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theOldManInTheCorner' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
# nearly a million detective words. 

