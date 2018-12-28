setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("stringi")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
# library(rJava)
# library("openNLPdata")

# eureaka, poe. 
stock <- c("Title", "Type", "ID", "Unit")
poe <- scan("rawTexts/edgar-allen-poe-eureka.txt",what="character",sep="\n")

poe.start <- 6
poe.end <- 269
poe <- poe[poe.start:poe.end]

poe <- gsub('_', '', perl=TRUE, poe)
poe<-  gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, poe)
poe <- gsub('\"', '', perl=TRUE, poe)

length(poe)


first_bite <- poe[1:264]

poe.sents.first <- paste0(first_bite, collapse = "\n")
poe.sents.first <- unlist(tokenize_sentences(poe.sents.first))

poe.sents <- c(poe.sents.first)

poe.sents <- poe.sents[-c(1:4)]
poe.sents <- poe.sents[-c(1)]
poe.sents <- gsub("EUREKA: AN ESSAY ON THE MATERIAL AND SPIRITUAL UNIVERSE IT", "It", poe.sents)


poe.title <- rep("eureka", 1088)
poe.sents.type <- rep("sentence", 1088)
poe.sents.counter<-seq(1, 1088)
poe.sents.id <- paste0("EUREKA_", "SENT_", poe.sents.counter)
print(length(poe.sents.id))
poe.sents.matrix <- cbind(poe.title, poe.sents.type, poe.sents.id, poe.sents)
poe.sents.df <- as.data.frame(poe.sents.matrix, stringsAsFactors = FALSE)
colnames(poe.sents.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", poe.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='eureka' LIMIT 2")
dbDisconnect(con)

# words. 

stock <- c("Title", "Type", "ID", "Unit")
poe <- scan("rawTexts/edgar-allen-poe-eureka.txt",what="character",sep="\n")

poe.start <- 6
poe.end <- 269
poe <- poe[poe.start:poe.end]
poe <- poe[-c(1:9)]
poe <- gsub('_', '', perl=TRUE, poe)
poe<-  gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, poe)
poe <- gsub('\"', '', perl=TRUE, poe)

length(poe)

poe.not.blanks <- which(poe != "")
poe <- poe[poe.not.blanks]

poe.temp <- poe
poe.temp <- paste(poe.temp, collapse=" ")
poe.temp <-tolower(poe.temp)
# a better regex that is going to maintain contractions. important! 

poe.temp <- unlist(strsplit(poe.temp, "[^\\w']", perl=TRUE))
poe.not.blanks <- which(poe.temp != "")
poe.words <- poe.temp[poe.not.blanks]
print(length(poe.words))

poe.title <- rep("eureka", 38566)
poe.words.type <- rep("word", 38566)
poe.words.counter <- seq(1, 38566)
poe.words.id <- paste0("EUREKA_", "WORD_", poe.words.counter)

poe.words.matrix <- cbind(poe.title, poe.words.type, poe.words.id, poe.words)

poe.words.df <- as.data.frame(poe.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(poe.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", poe.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='eureka' LIMIT 10")

dbDisconnect(con)

# paras. 

poe.paragraphs <- read.csv("Python_Scripts/checkCorpus/EUREKA_paras.csv", stringsAsFactors = FALSE)
poe.paragraphs <- as.data.frame(unlist(strsplit(poe.paragraphs$X0, "\n", perl=TRUE)), stringsAsFactors=FALSE)
colnames(poe.paragraphs) <- ("paras")
poe.paragraphs <- as.data.frame(poe.paragraphs[-c(1:16,272),],stringsAsFactors=FALSE)
colnames(poe.paragraphs) <- ("paras")
poe.paragraphs <- poe.paragraphs %>%
  transmute(paragraph = gsub('\"', 'Mrs', perl=TRUE, paras))
poe.paragraphs <- poe.paragraphs %>%
  transmute(paras = gsub('_', '', perl=TRUE, paragraph))
print(length(poe.paragraphs$paras))


poe.title <- rep("eureka", 255)
poe.para.type <- rep("paragraph", 255)
poe.para.counter<-seq(1, 255)
poe.para.id <- paste0("EUREKA_", "PARAGRAPH_", poe.para.counter)
print(length(poe.para.id))
poe.para.matrix <- cbind(poe.title, poe.para.type, poe.para.id, poe.paragraphs)
poe.para.df <- as.data.frame(poe.para.matrix, stringsAsFactors = FALSE)
colnames(poe.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", poe.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='eureka' LIMIT 2")
dbDisconnect(con)

# euerka is done. 



