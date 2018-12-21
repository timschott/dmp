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

### oscar wilde
stock <- c("Title", "Type", "ID", "Unit")
dorian <- scan("rawTexts/oscar-wilde-picture-of-dorian-gray.txt",what="character", sep="\n")
dorian.start <- which(dorian=="The studio was filled with the rich odour of roses, and when the light")
dorian.end<- which(dorian=="that they recognized who it was.")
dorian <- dorian[dorian.start:dorian.end]
print(length(dorian))
dorian<- gsub('CHAPTER [0-9]+', "", dorian)
dorian <- replace_abbreviation(dorian)
dorian <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, dorian)
print(length(dorian))

dorian.temp <- which(dorian != "")
dorian <- dorian[dorian.temp]


first_bite <- dorian[1:2499]
second_bite<- dorian[2500:4999]
third_bite <- dorian[5000:6870]

dorian.sents.first <- paste0(first_bite, collapse = "\n")
dorian.sents.first <- unlist(tokenize_sentences(dorian.sents.first))

dorian.sents.second <- paste0(second_bite, collapse = "\n")
dorian.sents.second <- unlist(tokenize_sentences(dorian.sents.second))

dorian.sents.third <- paste0(third_bite, collapse = "\n")
dorian.sents.third <- unlist(tokenize_sentences(dorian.sents.third))

dorian.sents <- c(dorian.sents.first, dorian.sents.second, dorian.sents.third)

dorian.sents.df <- as.data.frame(dorian.sents, stringsAsFactors = FALSE)

#### 

bad_spots <-c(0)

substr(dorian.sents[5], nchar(dorian.sents[5])-1, nchar(dorian.sents[5]))
substr(dorian.sents[91], nchar(dorian.sents[91])-1, nchar(dorian.sents[91]))
bad_spots <-c(0)
for(i in seq(1:length(dorian.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(dorian.sents[i], nchar(dorian.sents[i]), nchar(dorian.sents[i]))
  test2 <- substr(dorian.sents[i+1], 1, 1)
  test3 <- substr(dorian.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      dorian.sents[i] <- paste(dorian.sents[i], dorian.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

dorian.sents <- dorian.sents[-c(bad_spots)]
print(length(dorian.sents))
dorian.sents[452] <- paste0(dorian.sents[452], dorian.sents[453])

dorian.sents <- dorian.sents[-c(452)]
dorian.sents <- gsub("_", "", perl = TRUE, dorian.sents)

dorian.title <- rep("dorian", 6265)
dorian.sents.type <- rep("sentence", 6265)
dorian.sents.counter<-seq(1, 6265)
dorian.sents.id <- paste0("DORIAN_", "SENT_", dorian.sents.counter)
print(length(dorian.sents.id))
dorian.sents.matrix <- cbind(dorian.title, dorian.sents.type, dorian.sents.id, dorian.sents)
dorian.sents.df <- as.data.frame(dorian.sents.matrix, stringsAsFactors = FALSE)
colnames(dorian.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", dorian.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='dorian' LIMIT 2")
dbDisconnect(con)

## words. 


dorian <- scan("rawTexts/oscar-wilde-picture-of-dorian-gray.txt",what="character", sep="\n")
dorian.start <- which(dorian=="The studio was filled with the rich odour of roses, and when the light")
dorian.end<- which(dorian=="that they recognized who it was.")
dorian <- dorian[dorian.start:dorian.end]
print(length(dorian))
dorian<- gsub('CHAPTER [0-9]+', "", dorian)
dorian <- replace_abbreviation(dorian)
dorian <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, dorian)
dorian <- gsub('_', '', perl=TRUE, dorian)

print(length(dorian))

dorian.not.blanks <- which(dorian != "")
dorian <- dorian[dorian.not.blanks]

dorian.temp <- dorian
dorian.temp <- paste(dorian.temp, collapse=" ")
dorian.temp <-tolower(dorian.temp)
# a better regex that is going to maintain contractions. important! 

dorian.temp <- unlist(strsplit(dorian.temp, "[^\\w']", perl=TRUE))
dorian.not.blanks <- which(dorian.temp != "")
dorian.words <- dorian.temp[dorian.not.blanks]

print(length(dorian.words))

dorian.title <- rep("dorian", 79295)
dorian.words.type <- rep("word", 79295)
dorian.words.counter <- seq(1, 79295)
dorian.words.id <- paste0("DORIAN_", "WORD_", dorian.words.counter)

dorian.words.matrix <- cbind(dorian.title, dorian.words.type, dorian.words.id, dorian.words)

dorian.words.df <- as.data.frame(dorian.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(dorian.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", dorian.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='dorian' LIMIT 10")
dbDisconnect(con)

################## paras. 

dorian.paragraphs <- read.csv("Python_Scripts/checkCorpus/DORIAN_paras.csv", stringsAsFactors = FALSE)

dorian.paragraphs <- dorian.paragraphs[-c(1:21,1555:1634),]
colnames(dorian.paragraphs) <- c("arb", "paras")

dorian.paragraphs <- dorian.paragraphs %>%
  transmute(paragraph = gsub('CHAPTER [0-9]', '', perl=TRUE, paras))

dorian.paragraphs <- dorian.paragraphs %>%
  transmute(paras = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)|_', '', perl=TRUE, paragraph))

dorian.paragraphs <- dorian.paragraphs %>%
  transmute(paragraphs = replace_abbreviation(paras))

# no blanx
dorian.paragraphs <- dorian.paragraphs %>% filter(paragraphs!="")

print(length(dorian.paragraphs$paragraphs))

dorian.title <- rep("dorian", 1524)
dorian.para.type <- rep("paragraph", 1524)
dorian.para.counter<-seq(1, 1524)
dorian.para.id <- paste0("DORIAN_", "PARAGRAPH_", dorian.para.counter)
print(length(dorian.para.id))
dorian.para.matrix <- cbind(dorian.title, dorian.para.type, dorian.para.id, dorian.paragraphs)
dorian.para.df <- as.data.frame(dorian.para.matrix, stringsAsFactors = FALSE)
colnames(dorian.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", dorian.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='dorian' LIMIT 2")
dbDisconnect(con)

# dorian done. 



