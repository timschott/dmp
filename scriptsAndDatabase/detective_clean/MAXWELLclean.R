setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

# just do the paragraphs first then distill down. 

max.paragraphs <- read.csv("Python_Scripts/checkCorpus/MAXWELL_paras.csv", stringsAsFactors = FALSE)
max.paragraphs$X0[20]

max.paragraphs <- max.paragraphs[-c(1:13,1925:1928),]

colnames(max.paragraphs) <- c("arb", "paragraphs")

max.paragraphs <- max.paragraphs %>% 
  transmute(paras=  gsub("\n", " ", paragraphs) )
max.paragraphs <- max.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

colnames(max.paragraphs) <- c("paras")


max.paragraphs<- max.paragraphs %>%
  transmute(paragraphs=gsub("\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

max.paragraphs <- max.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

max.paragraphs <- max.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

max.paragraphs <- max.paragraphs %>% 
  transmute(paras=  gsub("Dr\\.", "Dr", paragraphs) )

max.paragraphs <- max.paragraphs %>% 
  filter(paras!="")

max.paragraphs <- max.paragraphs %>% 
  filter(paras!="  ")
colnames(max.paragraphs)

max.paragraphs <- max.paragraphs %>% 
  transmute(paragraphs = replace_abbreviation(paras))

max.paragraphs <- max.paragraphs %>% 
  transmute(paras=  gsub("MR\\.", "Mr", paragraphs))

print(length(max.paragraphs$paras))

max.paragraphs <- max.paragraphs %>% 
  filter(paras!="")

max.paragraphs <- max.paragraphs %>% 
  filter(paras!="  ")
max.paragraphs <- max.paragraphs %>% 
  transmute(paragraphs=  gsub("_", "", paras))

print(length(max.paragraphs$paragraphs))

max.title <- rep("theMaxwellMystery", 1911)
max.para.type <- rep("paragraph",1911)
max.para.counter<-seq(1, 1911)
max.para.id <- paste0("THE_MAXWELL_MYSTERY_", "PARAGRAPH_", max.para.counter)
max.label <- rep("0", 1911)
print(length(max.para.id))

max.para.matrix <- cbind(max.title, max.para.type, max.para.id, max.paragraphs, max.label)
max.para.df <- as.data.frame(max.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(max.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", max.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theMaxwellMystery' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Type='sentence' AND Title='theMaxwellMystery'")
dbDisconnect(con)

# sents. 
max <- max.para.df$Unit
first_bite <- max[1:1911]
first_bite <- gsub("No\\.", "No", perl=TRUE, first_bite)

max.sents.first <- paste0(first_bite, collapse = "\n")
max.sents.first <- unlist(tokenize_sentences(max.sents.first))

max.sents <- c(max.sents.first)
max.sents.df <- as.data.frame(max.sents, stringsAsFactors = FALSE)

print(length(max.sents.df$max.sents))

bad_spots <-c(0)
for(i in seq(1:length(max.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(max.sents[i], nchar(max.sents[i]), nchar(max.sents[i]))
  test2 <- substr(max.sents[i+1], 1, 1)
  test3 <- substr(max.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      max.sents[i] <- paste(max.sents[i], max.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
max.sents[bad_spots]
max.sents <- max.sents[-c(bad_spots)]
max.sents <- max.sents[max.sents!=""]
print(length(max.sents))

bad_spots <-c(0)
for(i in seq(1:length(max.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(max.sents[i], nchar(max.sents[i])-1, nchar(max.sents[i]))
  test2 <- substr(max.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      max.sents[i] <- paste(max.sents[i], max.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
max.sents[bad_spots]
max.sents <- max.sents[-c(bad_spots)]
print(length(max.sents))

max.title <- rep("theMaxwellMystery", 3508)
max.sents.type <- rep("sentence", 3508)
max.sents.counter<-seq(1, 3508)
max.sents.id <- paste0("THE_MAXWELL_MYSTERY_", "SENT_", max.sents.counter)
max.label <- rep("0", 3508)
print(length(max.sents.id))

max.sents.matrix <- cbind(max.title, max.sents.type, max.sents.id, max.sents, max.label)
max.sents.df <- as.data.frame(max.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(max.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", max.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theMaxwellMystery' LIMIT 2")
dbDisconnect(con)

max.temp <- max
max.temp <- paste(max.temp, collapse=" ")
max.temp <-tolower(max.temp)
# a better regex that is going to maintain contractions. important! 

max.temp <- unlist(strsplit(max.temp, "[^\\w']", perl=TRUE))
max.not.blanks <- which(max.temp != "")
max.words <- max.temp[max.not.blanks]
print(length(max.words))
max.words<- max.words[which(max.words!="^'")]
max.words<- max.words[which(max.words!="'")]
max.words<- max.words[which(max.words!="''")]
print(length(max.words))
max.words.df <- as.data.frame(max.words, stringsAsFactors = FALSE)

max.title <- rep("theMaxwellMystery", 60289)
max.words.type <- rep("word", 60289)
max.words.counter <- seq(1, 60289)
max.words.id <- paste0("THE_MAXWELL_MYSTERY_", "WORD_", max.words.counter)
max.label<- rep("0", 60289)
max.words.matrix <- cbind(max.title, max.words.type, max.words.id, max.words, max.label)

max.words.df <- as.data.frame(max.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(max.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", max.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theMaxwellMystery' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)

