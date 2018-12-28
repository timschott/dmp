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

stock <- c("Title", "Type", "ID", "Unit")

#### the Rainbow cleaning. 

rainbow <- scan("rawTexts/dh-lawrence-the-rainbow.txt",what="character",sep="\n")

rainbow.start<- which(rainbow == "The Brangwens had lived for generations on the Marsh Farm, in")
rainbow.end <- which(rainbow == "Truth, fitting to the over-arching heaven.")

rainbow <- rainbow[rainbow.start:rainbow.end]

rainbow <- gsub('CHAPTER.X{0,3}(IX|IV|V?I{0,3}).|[A-Z]{2,}|_|EXEUNT', '', perl=TRUE, rainbow)

rainbow <- gsub('\"', '' , rainbow, fixed=TRUE)
# lets just look for the line number. 
rainbow <- replace_abbreviation(rainbow)
# okay sentences. 

print(length(rainbow))

first_bite <- rainbow[1:2499]
second_bite<- rainbow[2500:4999]
third_bite <- rainbow[5000:7499]
fourth_bite<- rainbow[7500:9999]
fifth_bite <- rainbow[10000:12499]
sixth_bite <- rainbow[15000:17499]
seventh_bite <- rainbow[17500:19003]

rainbow.sents.first <- paste0(first_bite, collapse = "\n")
rainbow.sents.first <- unlist(tokenize_sentences(rainbow.sents.first))

rainbow.sents.second <- paste0(second_bite, collapse = "\n")
rainbow.sents.second <- unlist(tokenize_sentences(rainbow.sents.second))

rainbow.sents.third <- paste0(third_bite, collapse = "\n")
rainbow.sents.third <- unlist(tokenize_sentences(rainbow.sents.third))

rainbow.sents.fourth <- paste0(fourth_bite, collapse = "\n")
rainbow.sents.fourth <- unlist(tokenize_sentences(rainbow.sents.fourth))

rainbow.sents.fifth <- paste0(fifth_bite, collapse = "\n")
rainbow.sents.fifth <- unlist(tokenize_sentences(rainbow.sents.fifth))

rainbow.sents.sixth <- paste0(sixth_bite, collapse = "\n")
rainbow.sents.sixth <- unlist(tokenize_sentences(rainbow.sents.sixth))

rainbow.sents.seventh <- paste0(seventh_bite, collapse = "\n")
rainbow.sents.seventh <- unlist(tokenize_sentences(rainbow.sents.seventh))

rainbow.sents <- c(rainbow.sents.first, rainbow.sents.second, rainbow.sents.third, rainbow.sents.fourth, rainbow.sents.fifth, rainbow.sents.sixth, rainbow.sents.seventh)


#rainbow.sents.df <- as.data.frame(rainbow.sents, stringsAsFactors = FALSE)
#substr(rainbow.sents[1109], nchar(rainbow.sents[1109]), nchar(rainbow.sents[1109]))

bad_spots <-c(0)
for(i in seq(1:length(rainbow.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(rainbow.sents[i], nchar(rainbow.sents[i]), nchar(rainbow.sents[i]))
  test2 <- substr(rainbow.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2)){
      print(i)
      rainbow.sents[i] <- paste(rainbow.sents[i], rainbow.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

# rainbow.sents[bad_spots]
rainbow.sents <- rainbow.sents[-c(bad_spots)]
# print(length(rainbow.sents))
# print(length(rainbow.sents))

rainbow.title <- rep("theRainbow", 12372)
rainbow.sents.type <- rep("sentence", 12372)
rainbow.sents.counter<-seq(1, 12372)
rainbow.sents.id <- paste0("THE_RAINBOW_", "SENT_", rainbow.sents.counter)
print(length(rainbow.sents.id))
rainbow.sents.matrix <- cbind(rainbow.title, rainbow.sents.type, rainbow.sents.id, rainbow.sents)
rainbow.sents.df <- as.data.frame(rainbow.sents.matrix, stringsAsFactors = FALSE)
colnames(rainbow.sents.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", rainbow.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theRainbow' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Title='theRainbow'")

dbDisconnect(con)

## rainbow, words 

rainbow <- scan("rawTexts/dh-lawrence-the-rainbow.txt",what="character",sep="\n")

rainbow.start<- which(rainbow == "The Brangwens had lived for generations on the Marsh Farm, in")
rainbow.end <- which(rainbow == "Truth, fitting to the over-arching heaven.")

rainbow <- rainbow[rainbow.start:rainbow.end]

rainbow <- gsub('CHAPTER.X{0,3}(IX|IV|V?I{0,3}).|[A-Z]{2,}|_|EXEUNT|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, rainbow)

rainbow <- gsub('\"', '' , rainbow, fixed=TRUE)
rainbow <- replace_abbreviation(rainbow)

# separate. 

rainbow.temp <- rainbow
rainbow.temp <- paste(rainbow.temp, collapse=" ")
rainbow.temp <-tolower(rainbow.temp)
# a better regex that is going to maintain contractions. important! 

rainbow.temp <- unlist(strsplit(rainbow.temp, "[^\\w']", perl=TRUE))
rainbow.not.blanks <- which(rainbow.temp != "")
rainbow.words <- rainbow.temp[rainbow.not.blanks]
print(length(rainbow.words))


rainbow.title <- rep("theRainbow", 187559)
rainbow.words.type <- rep("word", 187559)
rainbow.words.counter <- seq(1, 187559)
rainbow.words.id <- paste0("THE_RAINBOW", "WORD_", rainbow.words.counter)

rainbow.words.matrix <- cbind(rainbow.title, rainbow.words.type, rainbow.words.id, rainbow.words)

rainbow.words.df <- as.data.frame(rainbow.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(rainbow.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", rainbow.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theRainbow' LIMIT 10")
dbDisconnect(con)

### paragraphs. 

rainbow.paragraphs <- read.csv("Python_Scripts/checkCorpus/RAINBOW_paras.csv", stringsAsFactors = FALSE)

rainbow.paragraphs <- rainbow.paragraphs[-c(1:29, 4571:4651),]
colnames(rainbow.paragraphs) <- c("arb", "paras")

rainbow.paragraphs <- rainbow.paragraphs %>%
  transmute(paragraph = gsub('CHAPTER.X{0,3}(IX|IV|V?I{0,3}).|[A-Z]{2,}|_|EXEUNT|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, paras))

colnames(rainbow.paragraphs)

rainbow.paragraphs$paragraph[1:2]

rainbow.paragraphs <- rainbow.paragraphs %>%
  transmute(paras = gsub('\n', ' ', perl=TRUE, paragraph))

rainbow.paragraphs <- as.data.frame(rainbow.paragraphs[-which(rainbow.paragraphs$para==""),], stringsAsFactors = FALSE)
print(length(rainbow.paragraphs$`rainbow.paragraphs[-which(rainbow.paragraphs$para == ""), ]`))
colnames(rainbow.paragraphs) <-c("paragraph")

rainbow.title <- rep("theRainbow", 4524)
rainbow.para.type <- rep("paragraph", 4524)
rainbow.para.counter<-seq(1, 4524)
rainbow.para.id <- paste0("THE_RAINBOW_", "PARAGRAPH_", rainbow.para.counter)
print(length(rainbow.para.id))
rainbow.para.matrix <- cbind(rainbow.title, rainbow.para.type, rainbow.para.id, rainbow.paragraphs)
rainbow.para.df <- as.data.frame(rainbow.para.matrix, stringsAsFactors = FALSE)
colnames(rainbow.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", rainbow.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theRainbow' LIMIT 2")
dbDisconnect(con)

## YEET.
