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


moon <- scan("rawTexts/detective/wilkie-collins-the-moonstone.txt",what="character",sep="\n")
# roman numeral chaps
spots <- grep('[A-Z]{2,}[^a-z]', moon)
moon <- moon[-c(spots[-c(1, 27, 29, 40, 51, 55, 56, 64, 68, 69, 71)])]
moon.paragraphs <- as.data.frame(moon, stringsAsFactors=FALSE)
colnames(moon.paragraphs) <- c("paras")
moon.paragraphs <- moon.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs))

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paragraphs=  replace_abbreviation(paras))

moon.paragraphs <- moon.paragraphs %>%
  transmute(paras = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paragraphs))
moon.paragraphs <- moon.paragraphs %>% 
  transmute(paragraphs=  gsub("CHAPTER I", "", paras))

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paras=  gsub("\\* \\*\\*", "", paragraphs))

moon.paragraphs <- as.data.frame(moon.paragraphs[-c(3482, 3585)], stringsAsFactors=FALSE)
colnames(moon.paragraphs) <- c("paras")

# okay cool.

moon.paragraphs <- moon.paragraphs %>% 
  filter(paras!="")

print(length(moon.paragraphs$paras))
moon.title <- rep("theMoonstone", 3607)
moon.para.type <- rep("paragraph",3607)
moon.para.counter<-seq(1, 3607)
moon.para.id <- paste0("THE_MOONSTONE_", "PARAGRAPH_", moon.para.counter)
moon.label <- rep("0", 3607)
print(length(moon.para.id))

moon.para.matrix <- cbind(moon.title, moon.para.type, moon.para.id, moon.paragraphs, moon.label)
moon.para.df <- as.data.frame(moon.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(moon.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", moon.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theMoonstone' LIMIT 2")
dbDisconnect(con)

# moon paras ok. 
# sents.

moon <- moon.paragraphs$paras

first_bite <- moon[1:2499]
second_bite <- moon[2500:3607]

moon.sents.first <- paste0(first_bite, collapse = "\n")
moon.sents.first <- unlist(tokenize_sentences(moon.sents.first))

moon.sents.second <- paste0(second_bite, collapse = "\n")
moon.sents.second <- unlist(tokenize_sentences(moon.sents.second))

moon.sents <- c(moon.sents.first, moon.sents.second)
moon.sents.df <- as.data.frame(moon.sents, stringsAsFactors = FALSE)
print(length(moon.sents.df$moon.sents))


bad_spots <-c(0)
for(i in seq(1:length(moon.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(moon.sents[i], nchar(moon.sents[i]), nchar(moon.sents[i]))
  test2 <- substr(moon.sents[i+1], 1, 1)
  test3 <- substr(moon.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      moon.sents[i] <- paste(moon.sents[i], moon.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
# moon.sents[bad_spots]
moon.sents <- moon.sents[-c(bad_spots)]


bad_spots <-c(0)
for(i in seq(1:length(moon.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(moon.sents[i], nchar(moon.sents[i])-1, nchar(moon.sents[i]))
  test2 <- substr(moon.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if((test %in% c('?”', '!”') && test2==tolower(test2) && test2!='I')|| (test %in% c('?”', '!”') && test2=='I')){
      #print(i)
      moon.sents[i] <- paste(moon.sents[i], moon.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

moon.sents[bad_spots]
moon.sents <- moon.sents[-c(bad_spots)]

print(length(moon.sents))

moon.sents.df <- as.data.frame(moon.sents, stringsAsFactors = FALSE)

## 
moon.title <- rep("theMoonstone", 11417)
moon.sents.type <- rep("sentence", 11417)
moon.sents.counter<-seq(1, 11417)
moon.sents.id <- paste0("THE_MOONSTONE_", "SENT_", moon.sents.counter)
moon.label <- rep("0", 11417)
print(length(moon.sents.id))

moon.sents.matrix <- cbind(moon.title, moon.sents.type, moon.sents.id, moon.sents, moon.label)
moon.sents.df <- as.data.frame(moon.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(moon.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", moon.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theMoonstone' LIMIT 2")
dbDisconnect(con)

# words.
moon.temp <- moon
moon.temp <- paste(moon.temp, collapse=" ")
moon.temp <-tolower(moon.temp)
# a better regex that is going to maintain contractions. important! 

moon.temp <- unlist(strsplit(moon.temp, "[^\\w’]", perl=TRUE))
moon.not.blanks <- which(moon.temp != "")
moon.words <- moon.temp[moon.not.blanks]
print(length(moon.words))

moon.words<- moon.words[which(moon.words!="^’")]
print(length(moon.words))

moon.title <- rep("theMoonstone", 197369)
moon.words.type <- rep("word", 197369)
moon.words.counter <- seq(1, 197369)
moon.words.id <- paste0("THE_MOONSTONE", "WORD_", moon.words.counter)
moon.label<- rep("0", 197369)
moon.words.matrix <- cbind(moon.title, moon.words.type, moon.words.id, moon.words, moon.label)

moon.words.df <- as.data.frame(moon.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(moon.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", moon.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theMoonstone' LIMIT 10")
dbDisconnect(con)

# moon done. first detective novel done! 