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

poe <- scan("rawTexts/edgar-allen-poe-narrative-of-arthur-gordon-pym.txt",what="character",sep="\n")

poe.start<- 55
poe.end <- which(poe == "UNCLE: “My eyes!—well, Kate—well, Bobby!—this is a judgment upon me, as you say. But I am a man of my word—mark that! you shall have her, boy, (plum and all), when you please. Done up, by Jove! Three Sundays all in a row! I’ll go, and take Dubble L. Dee’s opinion upon that.”")
poe <- poe[poe.start:poe.end]
poe <- replace_abbreviation(poe)
poe <- gsub('_', '', perl=TRUE, poe)
poe<-  gsub('CHAPTER [0-9]+..*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, poe)
poe <- gsub('CHAPTER [0-9]|', '',perl=TRUE, poe)

length(poe)

poe.sents.first <- paste0(poe, collapse = "\n")
poe.sents.first <- unlist(tokenize_sentences(poe.sents.first))
poe.sents <- gsub('\"', '' , poe.sents.first, fixed=TRUE)

poe.sents.df <- as.data.frame(poe.sents, stringsAsFactors = FALSE)
poe.sents.df <- poe.sents.df[-c(16,2721)]

bad_spots <-c(0)
for(i in seq(1:length(poe.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(poe.sents[i], nchar(poe.sents[i])-1, nchar(poe.sents[i]))
  test2 <- substr(poe.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?”', '!”') && test2==tolower(test2)){
      #print(i)
      poe.sents[i] <- paste(poe.sents[i], poe.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

poe.sents[bad_spots]
poe.sents <- poe.sents[-c(bad_spots)]
print(length(poe.sents))

poe.sents.df <- as.data.frame(poe.sents, stringsAsFactors = FALSE)
substr(poe.sents[114], nchar(poe.sents[114]), nchar(poe.sents[114]))


bad_spots <-c(0)
for(i in seq(1:length(poe.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # but if the sequence starts with a capital letter, eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(poe.sents[i], nchar(poe.sents[i]), nchar(poe.sents[i]))
  test2 <- substr(poe.sents[i+1], 1, 1)
  test3 <- substr(poe.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      poe.sents[i] <- paste(poe.sents[i], poe.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

poe.sents[bad_spots]
poe.sents <- poe.sents[-c(bad_spots)]
print(length(poe.sents))
poe.sents.df <- as.data.frame(poe.sents, stringsAsFactors = FALSE)

# now commit. 

pym.title <- rep("pym", 3680)
pym.sents.type <- rep("sentence", 3680)
pym.sents.counter<-seq(1, 3680)
pym.sents.id <- paste0("PYM_", "SENT_", pym.sents.counter)
print(length(pym.sents.id))
pym.sents.matrix <- cbind(pym.title, pym.sents.type, pym.sents.id, poe.sents)
pym.sents.df <- as.data.frame(pym.sents.matrix, stringsAsFactors = FALSE)
colnames(pym.sents.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", pym.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='pym' LIMIT 2")
dbDisconnect(con)

## words. 


poe <- scan("rawTexts/edgar-allen-poe-narrative-of-arthur-gordon-pym.txt",what="character",sep="\n")

poe.start<- 55
poe.end <- which(poe == "UNCLE: “My eyes!—well, Kate—well, Bobby!—this is a judgment upon me, as you say. But I am a man of my word—mark that! you shall have her, boy, (plum and all), when you please. Done up, by Jove! Three Sundays all in a row! I’ll go, and take Dubble L. Dee’s opinion upon that.”")
poe <- poe[poe.start:poe.end]
poe <- replace_abbreviation(poe)
poe <- gsub('_', '', perl=TRUE, poe)
poe<-  gsub('CHAPTER [0-9]+..*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, poe)
poe <- gsub('CHAPTER [0-9]|', '',perl=TRUE, poe)

length(poe)

poe.not.blanks <- which(poe != "")
poe <- poe[poe.not.blanks]

poe.temp <- poe
poe.temp <- paste(poe.temp, collapse=" ")
poe.temp <-tolower(poe.temp)
# a better regex that is going to maintain contractions. important! 

poe.temp <- unlist(strsplit(poe.temp, "[^\\w’]", perl=TRUE))
poe.not.blanks <- which(poe.temp != "")
poe.words <- poe.temp[poe.not.blanks]

# lots of words! 
print(length(poe.words))

poe.title <- rep("pym", 100954)
poe.words.type <- rep("word", 100954)
poe.words.counter <- seq(1, 100954)
poe.words.id <- paste0("PYM_", "WORD_", poe.words.counter)

poe.words.matrix <- cbind(poe.title, poe.words.type, poe.words.id, poe.words)

poe.words.df <- as.data.frame(poe.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(poe.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", poe.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='pym' LIMIT 10")
dbDisconnect(con)

## paragraphs. 




