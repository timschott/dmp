setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
rm(list=ls())
rayn <- scan("rawTexts/detective/js-fletcher-the-rayner-slade-amalgamation.txt",what="character",sep="\n")
rayn.start <- which(rayn=="About eleven o'clock on the night of Monday, May 12, 1914, Marshall Allerdyke, a bachelor of forty, a man of great mental and physical activity, well known in Bradford as a highly successful manufacturer of dress goods, alighted at the Central Station in that city from an express which had just arrived from Manchester, where he had spent the day on business. He had scarcely set foot on the platform when he was confronted by his chauffeur, a young man in a neat dark-green livery, who took his master's travelling rug in one hand, while with the other he held out an envelope.")
rayn.fin <- which(rayn=="***END OF THE PROJECT GUTENBERG EBOOK THE RAYNER-SLADE AMALGAMATION ***")
rayn <- rayn[rayn.start:rayn.fin-1]

spots <- grep('[A-Z]{2,}[^a-z]', rayn)
rayn[spots]
rayn <- rayn[-c(spots[-c(2,3, 8, 11, 16:20, 46, 49,52,69)])]

rayn.paragraphs <- as.data.frame(rayn, stringsAsFactors=FALSE)

colnames(rayn.paragraphs) <- c("paras")
rayn.paragraphs <- rayn.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

colnames(rayn.paragraphs) <- c("paras")


rayn.paragraphs<- rayn.paragraphs %>%
  transmute(paragraphs=gsub("\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

rayn.paragraphs <- rayn.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

rayn.paragraphs <- rayn.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

rayn.paragraphs <- rayn.paragraphs %>% 
  filter(paragraphs!="")

rayn.paragraphs <- rayn.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(rayn.paragraphs)

rayn.paragraphs <- rayn.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

rayn.paragraphs <- rayn.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(rayn.paragraphs$paragraphs))

rayn.paragraphs <- rayn.paragraphs %>% 
  filter(paragraphs!="")

rayn.paragraphs <- rayn.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(rayn.paragraphs$paragraphs))

rayn.title <- rep("theRaynerSladeAmalgamation", 1717)
rayn.para.type <- rep("paragraph",1717)
rayn.para.counter<-seq(1, 1717)
rayn.para.id <- paste0("THE_RAYNER_SLADE_AMALGAMATION_", "PARAGRAPH_", rayn.para.counter)
rayn.label <- rep("0", 1717)
print(length(rayn.para.id))

rayn.para.matrix <- cbind(rayn.title, rayn.para.type, rayn.para.id, rayn.paragraphs, rayn.label)
rayn.para.df <- as.data.frame(rayn.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(rayn.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", rayn.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theRaynerSladeAmalgamation' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Type='paragraph' OR Type= 'sentence' AND Title='theScarhavenKeep'")
dbDisconnect(con)

# sents. 
rayn <- rayn.paragraphs$paragraphs

first_bite <- rayn[1:1717]

rayn.sents.first <- paste0(first_bite, collapse = "\n")
rayn.sents.first <- unlist(tokenize_sentences(rayn.sents.first))

rayn.sents <- c(rayn.sents.first)
rayn.sents.df <- as.data.frame(rayn.sents, stringsAsFactors = FALSE)

print(length(rayn.sents.df$rayn.sents))


bad_spots <-c(0)
for(i in seq(1:length(rayn.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(rayn.sents[i], nchar(rayn.sents[i]), nchar(rayn.sents[i]))
  test2 <- substr(rayn.sents[i+1], 1, 1)
  test3 <- substr(rayn.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      rayn.sents[i] <- paste(rayn.sents[i], rayn.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
# rayn.sents[bad_spots]
rayn.sents <- rayn.sents[-c(bad_spots)]

print(length(rayn.sents))
rayn.sents <- rayn.sents[rayn.sents!=""]

bad_spots <-c(0)
for(i in seq(1:length(rayn.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(rayn.sents[i], nchar(rayn.sents[i])-1, nchar(rayn.sents[i]))
  test2 <- substr(rayn.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      rayn.sents[i] <- paste(rayn.sents[i], rayn.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
rayn.sents[bad_spots]
rayn.sents <- rayn.sents[-c(bad_spots)]
print(length(rayn.sents))
rayn.title <- rep("theRaynerSladeAmalgamation", 5081)
rayn.sents.type <- rep("sentence", 5081)
rayn.sents.counter<-seq(1, 5081)
rayn.sents.id <- paste0("THE_RAYNER_SLADE_AMALGAMATION_", "SENT_", rayn.sents.counter)
rayn.label <- rep("0", 5081)
print(length(rayn.sents.id))

rayn.sents.matrix <- cbind(rayn.title, rayn.sents.type, rayn.sents.id, rayn.sents, rayn.label)
rayn.sents.df <- as.data.frame(rayn.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(rayn.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", rayn.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theRaynerSladeAmalgamation' LIMIT 2")
dbDisconnect(con)

# words . 
rayn.temp <- rayn
rayn.temp <- paste(rayn.temp, collapse=" ")
rayn.temp <-tolower(rayn.temp)
# a better regex that is going to maintain contractions. important! 

rayn.temp <- unlist(strsplit(rayn.temp, "[^\\w']", perl=TRUE))
rayn.not.blanks <- which(rayn.temp != "")
rayn.words <- rayn.temp[rayn.not.blanks]
print(length(rayn.words))

rayn.words<- rayn.words[which(rayn.words!="^'")]
rayn.words<- rayn.words[which(rayn.words!="'")]
print(length(rayn.words))

rayn.words.df <- as.data.frame(rayn.words, stringsAsFactors = FALSE)

rayn.title <- rep("theRaynerSladeAmalgamation", 79730)
rayn.words.type <- rep("word", 79730)
rayn.words.counter <- seq(1, 79730)
rayn.words.id <- paste0("THE_RAYNER_SLADE_AMALGAMATION_", "WORD_", rayn.words.counter)
rayn.label<- rep("0", 79730)
rayn.words.matrix <- cbind(rayn.title, rayn.words.type, rayn.words.id, rayn.words, rayn.label)

rayn.words.df <- as.data.frame(rayn.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(rayn.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", rayn.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theRaynerSladeAmalgamation' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
