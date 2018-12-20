setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
install.packages("rJava")
library(rJava)
library("openNLPdata")

stock <- c("Title", "Type", "ID", "Unit")

lita <- scan("rawTexts/vladimir-nabokov-lolita-2.txt",what="character",sep="\n")

lita.start<- which(lita == "Lolita, light of my life, fire of my loins. My sin, my soul. Lo-lee-ta:")
lita.end <- which(lita == "share, my Lolita.")

lita<- lita[lita.start: lita.end]
lita <- replace_abbreviation(lita)
lita <- gsub('_', '', perl=TRUE, lita)

lita <- gsub('^[0-9]+', '', perl=TRUE, lita)

lita.not.blanks <- which(lita != "")
lita <- lita[lita.not.blanks]
9176


first_bite <- lita[1:2499]
second_bite<- lita[2500:4999]
third_bite <- lita[5000:7499]
fourth_bite <- lita[7500:9176]

lita.sents.first <- paste0(first_bite, collapse = "\n")
lita.sents.first <- unlist(tokenize_sentences(lita.sents.first))

lita.sents.second <- paste0(second_bite, collapse = "\n")
lita.sents.second <- unlist(tokenize_sentences(lita.sents.second))

lita.sents.third <- paste0(third_bite, collapse = "\n")
lita.sents.third <- unlist(tokenize_sentences(lita.sents.third))

lita.sents.fourth <- paste0(fourth_bite, collapse = "\n")
lita.sents.fourth <- unlist(tokenize_sentences(lita.sents.fourth))

lita.sents <- c(lita.sents.first, lita.sents.second, lita.sents.third, lita.sents.fourth)
length(lita.sents)
# punctuation loop.

bad_spots<-c(0)
substr(lita.sents[750], nchar(lita.sents[750]), nchar(lita.sents[750]))

for(i in seq(1:length(lita.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(lita.sents[i], nchar(lita.sents[i]), nchar(lita.sents[i]))
  test2 <- substr(lita.sents[i+1], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2)){
    lita.sents[i] <- paste(lita.sents[i], lita.sents[i+1])
    # print(lita.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}
bad_spots

lita.sents <- lita.sents[-bad_spots]

print(length(lita.sents))
# first person fun.

for(i in seq(1:length(lita.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(lita.sents[i], nchar(lita.sents[i]), nchar(lita.sents[i]))
  test2 <- substr(lita.sents[i+1], 1, 1)
  if(test %in% c('?', '!') && test2%in%c('I')){
    lita.sents[i] <- paste(lita.sents[i], lita.sents[i+1])
    # print(lita.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}
bad_spots

lita.sents <- lita.sents[-bad_spots]

print(length(lita.sents))

lita.title <- rep("lolita", 5158)
lita.sents.type <- rep("sentence", 5158)
lita.sents.counter<-seq(1, 5158)
lita.sents.id <- paste0("LOLITA_", "SENT_", lita.sents.counter)
print(length(lita.sents.id))
lita.sents.matrix <- cbind(lita.title, lita.sents.type, lita.sents.id, lita.sents)
lita.sents.df <- as.data.frame(lita.sents.matrix, stringsAsFactors = FALSE)
colnames(lita.sents.df) <- stock
# okay i think it's good now.
# lita To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", lita.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='lolita' LIMIT 2")
dbDisconnect(con)
