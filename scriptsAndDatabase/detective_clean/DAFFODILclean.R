setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

daff <- scan("rawTexts/detective/edgar-wallace-the-daffodil-mystery.txt",what="character",sep="\n")
daff.start <- which(daff=="\"I am afraid I don't understand you, Mr. Lyne.\"")
daff.end <- which(daff=="\"Because,\" said Tarling, \"I was reading an article on horticulture in this morning's papers and I learnt that daffodils do not grow in the Argentine.\"")
daff<-daff[daff.start:daff.end]
print(length(daff))

spots <- grep('[A-Z]{2,}[^a-z]', daff)
daff[spots]
daff <- daff[-c(spots[-c(47)])]

daff.paragraphs <- as.data.frame(daff, stringsAsFactors=FALSE)
colnames(daff.paragraphs) <- c("paras")
daff.paragraphs <- daff.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

colnames(daff.paragraphs) <- c("paras")


daff.paragraphs <- daff.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

daff.paragraphs <- daff.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs))

daff.paragraphs <- daff.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "", paras))

daff.paragraphs <- daff.paragraphs %>% 
  transmute(paras=  gsub("\\*", "", paragraphs))

daff.paragraphs <- daff.paragraphs %>% 
  transmute(paragraphs = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paras))

daff.paragraphs <- daff.paragraphs %>% 
  filter(paragraphs!="")

daff.paragraphs <- daff.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(daff.paragraphs$paragraphs))

daff.title <- rep("theDaffodilMystery", 2300)
daff.para.type <- rep("paragraph",2300)
daff.para.counter<-seq(1, 2300)
daff.para.id <- paste0("THE_DAFFODIL_MYSTERY_", "PARAGRAPH_", daff.para.counter)
daff.label <- rep("0", 2300)
print(length(daff.para.id))

daff.para.matrix <- cbind(daff.title, daff.para.type, daff.para.id, daff.paragraphs, daff.label)
daff.para.df <- as.data.frame(daff.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(daff.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", daff.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theDaffodilMystery' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Type='paragraph' AND Title='theDaffodilMystery'")
# dbExecute(con, "DELETE FROM textTable WHERE Type='sentence' AND Title='theDaffodilMystery'")

dbDisconnect(con)

# sents. 

daff <- daff.paragraphs$paragraphs

first_bite <- daff[1:2300]

daff.sents.first <- paste0(first_bite, collapse = "\n")
daff.sents.first <- unlist(tokenize_sentences(daff.sents.first))

daff.sents <- c(daff.sents.first)
daff.sents.df <- as.data.frame(daff.sents, stringsAsFactors = FALSE)

print(length(daff.sents.df$daff.sents))
bad_spots <-c(0)
for(i in seq(1:length(daff.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(daff.sents[i], nchar(daff.sents[i])-1, nchar(daff.sents[i]))
  test2 <- substr(daff.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      daff.sents[i] <- paste(daff.sents[i], daff.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
daff.sents[bad_spots]
daff.sents <- daff.sents[-c(bad_spots)]

daff.sents <- daff.sents[daff.sents!=""]
print(length(daff.sents))
daff.sents.df <- as.data.frame(daff.sents, stringsAsFactors = FALSE)

daff.title <- rep("theDaffodilMystery", 4561)
daff.sents.type <- rep("sentence", 4561)
daff.sents.counter<-seq(1, 4561)
daff.sents.id <- paste0("THE_DAFFODIL_MYSTERY_", "SENT_", daff.sents.counter)
daff.label <- rep("0", 4561)
print(length(daff.sents.id))

daff.sents.matrix <- cbind(daff.title, daff.sents.type, daff.sents.id, daff.sents, daff.label)
daff.sents.df <- as.data.frame(daff.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(daff.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", daff.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theDaffodilMystery' LIMIT 2")
dbDisconnect(con)

# words. 
daff.temp <- daff
daff.temp <- paste(daff.temp, collapse=" ")
daff.temp <-tolower(daff.temp)
# a better regex that is going to maintain contractions. important! 

daff.temp <- unlist(strsplit(daff.temp, "[^\\w']", perl=TRUE))
daff.not.blanks <- which(daff.temp != "")
daff.words <- daff.temp[daff.not.blanks]
print(length(daff.words))

daff.words<- daff.words[which(daff.words!="'")]
print(length(daff.words))

# sick! 

daff.title <- rep("theDaffodilMystery", 68379)
daff.words.type <- rep("word", 68379)
daff.words.counter <- seq(1, 68379)
daff.words.id <- paste0("THE_DAFFODIL_MYSTERY_", "WORD_", daff.words.counter)
daff.label<- rep("0", 68379)
daff.words.matrix <- cbind(daff.title, daff.words.type, daff.words.id, daff.words, daff.label)

daff.words.df <- as.data.frame(daff.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(daff.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", daff.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theDaffodilMystery' LIMIT 10")
dbGetQuery(con, "SELECT DISTINCT title FROM textTable WHERE Label='0'")
dbDisconnect(con)
# 1.2M detective words. 
