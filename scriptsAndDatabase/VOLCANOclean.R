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

## Malcolm Lowry, Under the Volcano. 

stock <- c("Title", "Type", "ID", "Unit")
volc <- scan("rawTexts/malcolm-lowry-under-the-volcano.txt",what="character",sep="\n")
volc_start <- 65
volc_end <- 12346
volc <- volc[volc_start:volc_end]
volc <- replace_abbreviation(volc)
volc <- gsub('_', '', perl=TRUE, volc)
#volc <- gsub('I\n|II\n|III\n|IV\n|V\n|VI\n|VII\n', '', perl=TRUE, volc)
volc <- gsub('M. Laruelle', 'M Laruelle', perl=TRUE, volc)

first_bite <- volc[1:2499]
second_bite<- volc[2500:4999]
third_bite <- volc[5000:7499]
fourth_bite<- volc[7500:9999]
fifth_bite <- volc[10000:12282]

volc.sents.first <- paste0(first_bite, collapse = "\n")
volc.sents.first <- unlist(tokenize_sentences(volc.sents.first))

volc.sents.second <- paste0(second_bite, collapse = "\n")
volc.sents.second <- unlist(tokenize_sentences(volc.sents.second))

volc.sents.third <- paste0(third_bite, collapse = "\n")
volc.sents.third <- unlist(tokenize_sentences(volc.sents.third))

volc.sents.fourth <- paste0(fourth_bite, collapse = "\n")
volc.sents.fourth <- unlist(tokenize_sentences(volc.sents.fourth))

volc.sents.fifth <- paste0(fifth_bite, collapse = "\n")
volc.sents.fifth <- unlist(tokenize_sentences(volc.sents.fifth))

volc.sents <- c(volc.sents.first, volc.sents.second, volc.sents.third, volc.sents.fourth, volc.sents.fifth)
volc.sents <- gsub('\"', '' , volc.sents, fixed=TRUE)


bad_spots <-c(0)
for(i in seq(1:length(volc.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(volc.sents[i], nchar(volc.sents[i])-1, nchar(volc.sents[i]))
  test2 <- substr(volc.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?”', '!”') && test2==tolower(test2)){
      #print(i)
      volc.sents[i] <- paste(volc.sents[i], volc.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

volc.sents[bad_spots]
volc.sents <- volc.sents[-c(bad_spots)]
print(length(volc.sents))


bad_spots <-c(0)
#### lowercase closure no punc.
for(i in seq(1:length(volc.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # but if the sequence starts with a capital letter, eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(volc.sents[i], nchar(volc.sents[i]), nchar(volc.sents[i]))
  test2 <- substr(volc.sents[i+1], 1, 1)
  test3 <- substr(volc.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      volc.sents[i] <- paste(volc.sents[i], volc.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

volc.sents[bad_spots]
volc.sents <- volc.sents[-c(bad_spots)]
print(length(volc.sents))


volc.title <- rep("underTheVolcano", 7559)
volc.sents.type <- rep("sentence", 7559)
volc.sents.counter<-seq(1, 7559)
volc.sents.id <- paste0("UNDER_THE_VOLCANO", "SENT_", volc.sents.counter)
print(length(volc.sents.id))
volc.sents.matrix <- cbind(volc.title, volc.sents.type, volc.sents.id, volc.sents)
volc.sents.df <- as.data.frame(volc.sents.matrix, stringsAsFactors = FALSE)
colnames(volc.sents.df) <- stock
# okay i think it's good now.
# volc To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", volc.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='underTheVolcano' LIMIT 2")
dbDisconnect(con)

####### did you see the words? 

## 

stock <- c("Title", "Type", "ID", "Unit")
volc <- scan("rawTexts/malcolm-lowry-under-the-volcano.txt",what="character",sep="\n")
volc_start <- 65
volc_end <- which(volc == "Somebody threw a dead dog after him down the ravine.")
volc <- volc[volc_start:volc_end]

volc <- replace_abbreviation(volc)
volc <- gsub('_', '', perl=TRUE, volc)
volc <- gsub('M. Laruelle', 'M Laruelle', perl=TRUE, volc)
print(length(volc))


volc.temp <- volc
volc.temp <- paste(volc.temp, collapse=" ")
volc.temp <-tolower(volc.temp)
# a better regex that is going to maintain contractions. important! 

volc.temp <- unlist(strsplit(volc.temp, "[^\\w’]", perl=TRUE))
volc.not.blanks <- which(volc.temp != "")
volc.words <- volc.temp[volc.not.blanks]
print(length(volc.words))


volc.title <- rep("underTheVolcano", 138612)
volc.words.type <- rep("word", 138612)
volc.words.counter <- seq(1, 138612)
volc.words.id <- paste0("UNDER_THE_VOLCANO_", "WORD_", volc.words.counter)

volc.words.matrix <- cbind(volc.title, volc.words.type, volc.words.id, volc.words)

volc.words.df <- as.data.frame(volc.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(volc.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", volc.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='underTheVolcano' LIMIT 10")
dbDisconnect(con)

## paras. 

volc.paragraphs <- read.csv("Python_Scripts/checkCorpus/VOLC_paras.csv", stringsAsFactors = FALSE)
volc.paragraphs <- volc.paragraphs[-c(1:49,162, 2144:2148),]

colnames(volc.paragraphs) <- c("arb", "para")
volc.paragraphs <- volc.paragraphs %>%
  transmute(paragraph = gsub('M. Laruelle', 'M Laruelle', perl=TRUE, para))

colnames(volc.paragraphs)
volc.paragraphs <- as.data.frame(volc.paragraphs[-c(1,257,420,917, 1111,1265, 1956),], stringsAsFactors=FALSE)
colnames(volc.paragraphs) <- c("para")
colnames(volc.paragraphs)
colnames(volc.paragraphs) <- c("para")


volc.paragraphs <- volc.paragraphs %>%
  transmute(paragraph = str_replace_all(para, "[\n]", " "))
print(length(volc.paragraphs$paragraph))

volc.title <- rep("underTheVolcano", 2086)
volc.paras.type <- rep("paragraph", 2086)
volc.paras.counter<-seq(1, 2086)
volc.paras.id <- paste0("UNDER_THE_VOLCANO_", "PARAGRAPH_", volc.paras.counter)
volc.paras.matrix <- cbind(volc.title, volc.paras.type, volc.paras.id, volc.paragraphs)
volc.paras.df <- as.data.frame(volc.paras.matrix, stringsAsFactors = FALSE)
colnames(volc.paras.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", volc.paras.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='underTheVolcano' LIMIT 2")
dbDisconnect(con)

### done.