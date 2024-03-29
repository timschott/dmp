setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
# library(rJava)
# library("openNLPdata")

stock <- c("Title", "Type", "ID", "Unit", "Label")

### serpent sents. 

serpent <- scan("rawTexts/dh-lawrence-the-serpent.txt",what="character",sep="\n")
serpent.start <-which(serpent == "Beginnings of a Bull-fight")
serpent.end <- which(serpent == "'You won't let me go!' she said to him.")

serpent <- serpent[serpent.start+1:serpent.end]
chapters <- grep('CHAPTER.X{0,3}(IX|IV|V?I{0,3}).', serpent)
serpent <- serpent[-c(chapters, chapters+1)]
serpent <- gsub('CHAPTER.X{0,3}(IX|IV|V?I{0,3}).|[A-Z]{2,}|_|EXEUNT', '', perl=TRUE, serpent)
print(length(serpent))

first_bite <- serpent[1:2499]
second_bite<- serpent[2500:5764]

serpent.sents.first <- paste0(first_bite, collapse = "\n")
serpent.sents.first <- unlist(tokenize_sentences(serpent.sents.first))

serpent.sents.second <- paste0(second_bite, collapse = "\n")
serpent.sents.second <- unlist(tokenize_sentences(serpent.sents.second))

serpent.sents <- c(serpent.sents.first, serpent.sents.second)
serpent.sents.df <- as.data.frame(serpent.sents, stringsAsFactors = FALSE)

serpent.sents[1182:1185]

bad_spots <-c(0)
for(i in seq(1:length(serpent.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(serpent.sents[i], nchar(serpent.sents[i])-1, nchar(serpent.sents[i]))
  test2 <- substr(serpent.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      serpent.sents[i] <- paste(serpent.sents[i], serpent.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

serpent.sents[bad_spots]
serpent.sents <- serpent.sents[-c(bad_spots)]
print(length(serpent.sents))
# manual fix
serpent.sents[1572] <- paste0(serpent.sents[1572], serpent.sents[1573])
serpent.sents <- serpent.sents[-c(1573)]
serpent.sents.df <- as.data.frame(serpent.sents, stringsAsFactors = FALSE)
length(serpent.sents)

serpent.title <- rep("theSerpent", 13587)
serpent.sents.type <- rep("sentence", 13587)
serpent.sents.counter<-seq(1, 13587)
serpent.sents.id <- paste0("THE_SERPENT", "SENT_", serpent.sents.counter)
print(length(serpent.sents.id))
serpent.sents.matrix <- cbind(serpent.title, serpent.sents.type, serpent.sents.id, serpent.sents)
serpent.sents.df <- as.data.frame(serpent.sents.matrix, stringsAsFactors = FALSE)
colnames(serpent.sents.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", serpent.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theSerpent' LIMIT 2")
dbDisconnect(con)

### words. 

serpent <- scan("rawTexts/dh-lawrence-the-serpent.txt",what="character",sep="\n")
serpent.start <-which(serpent == "Beginnings of a Bull-fight")
serpent.end <- which(serpent == "'You won't let me go!' she said to him.")

serpent <- serpent[serpent.start+1:serpent.end]
chapters <- grep('CHAPTER.X{0,3}(IX|IV|V?I{0,3}).', serpent)
serpent <- serpent[-c(chapters, chapters+1)]
serpent <- gsub('CHAPTER.X{0,3}(IX|IV|V?I{0,3}).|[A-Z]{2,}|_|EXEUNT', '', perl=TRUE, serpent)
print(length(serpent))

serpent <- replace_abbreviation(serpent)

serpent.temp <- serpent
serpent.temp <- paste(serpent.temp, collapse=" ")
serpent.temp <-tolower(serpent.temp)
# a better regex that is going to maintain contractions. important! 

serpent.temp <- unlist(strsplit(serpent.temp, "[^\\w']", perl=TRUE))
serpent.not.blanks <- which(serpent.temp != "")
serpent.words <- serpent.temp[serpent.not.blanks]
print(length(serpent.words))

serpent.words <- gsub("\\'", '', perl=TRUE, serpent.words)
serpent.not.blanks <- which(serpent.words != "")
serpent.words <- serpent.words[serpent.not.blanks]
serpent.words <- serpent.words[-c(172686:172760)]
print(length(serpent.words))
serpent.title <- rep("theSerpent", 172685)
serpent.words.type <- rep("word", 172685)
serpent.words.counter <- seq(1, 172685)
serpent.words.id <- paste0("THE_SERPENT", "WORD_", serpent.words.counter)

serpent.words.matrix <- cbind(serpent.title, serpent.words.type, serpent.words.id, serpent.words)

serpent.words.df <- as.data.frame(serpent.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(serpent.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", serpent.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theSerpent' LIMIT 10")
dbDisconnect(con)

##### paragraphs. 

serpent.paragraphs <- read.csv("Python_Scripts/checkCorpus/SERPENT_paras.csv", stringsAsFactors = FALSE)
serpent.paragraphs$X0[13]
test <- as.data.frame(unlist(strsplit(serpent.paragraphs$X0[13:145], "\n\n", perl=TRUE)), stringsAsFactors=FALSE)
test <- as.data.frame(test[-c(222,551, 765, 854, 1038, 1169, 1130,
                1385, 1520, 1681, 1845, 1962, 2153,
                2316, 2438, 2655, 2865, 3078,
                3301, 3635, 3855, 4149, 4322, 4406, 4645, 4738),], stringsAsFactors=FALSE)
serpent.paragraphs <- test
colnames(serpent.paragraphs) <- c("paragraphs")

print(length(serpent.paragraphs$paragraphs))# 4984

serpent.title <- rep("theSerpent", 4984)
serpent.para.type <- rep("paragraph", 4984)
serpent.para.counter<-seq(1, 4984)
serpent.label <- rep("1", 4984)
serpent.para.id <- paste0("THE_SERPENT_", "PARAGRAPH_", serpent.para.counter)
print(length(serpent.para.id))

serpent.para.matrix <- cbind(serpent.title, serpent.para.type, serpent.para.id, serpent.paragraphs, serpent.label)
serpent.para.df <- as.data.frame(serpent.para.matrix, stringsAsFactors = FALSE)
colnames(serpent.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", serpent.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theSerpent' LIMIT 2")

dbDisconnect(con)
