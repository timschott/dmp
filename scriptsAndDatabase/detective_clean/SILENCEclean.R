setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

silence <- scan("rawTexts/detective/harrington-strong-brand-of-silence.txt",what="character",sep="\n")
silence.start <- which(silence =="Now the fog was clearing and the mist was lifting, and the bright sunshine was struggling to penetrate the billows of damp vapor and touch with its glory the things of the world beneath. In the lower harbor there still was a chorus of sirens and foghorns, as craft of almost every description made way toward the metropolis or out toward the open sea.")
silence.fin <- which(silence =="THE END")
silence <- silence[silence.start:silence.fin-1]

spots <- grep('[A-Z]{2,}[^a-z]', silence)
silence[spots]
silence <- silence[-spots]

silence.paragraphs <- as.data.frame(silence, stringsAsFactors=FALSE)
colnames(silence.paragraphs) <- c("paras")

silence.paragraphs<- silence.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

silence.paragraphs <- silence.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

silence.paragraphs <- silence.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

silence.paragraphs <- silence.paragraphs %>% 
  filter(paragraphs!="")

silence.paragraphs <- silence.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(silence.paragraphs)

silence.paragraphs <- silence.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

silence.paragraphs <- silence.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(silence.paragraphs$paragraphs))

silence.paragraphs <- silence.paragraphs %>% 
  filter(paragraphs!="")

silence.paragraphs <- silence.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(silence.paragraphs$paragraphs))

silence.title <- rep("theBrandOfSilence", 2067)
silence.para.type <- rep("paragraph",2067)
silence.para.counter<-seq(1, 2067)
silence.para.id <- paste0("THE_BRAND_OF_SILENCE_", "PARAGRAPH_", silence.para.counter)
silence.label <- rep("0", 2067)
print(length(silence.para.id))

silence.para.matrix <- cbind(silence.title, silence.para.type, silence.para.id, silence.paragraphs, silence.label)
silence.para.df <- as.data.frame(silence.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(silence.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", silence.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theBrandOfSilence' LIMIT 2")
dbDisconnect(con)

# sents.

silence <- silence.paragraphs$paragraphs

first_bite <- silence[1:2067]

silence.sents.first <- paste0(first_bite, collapse = "\n")
silence.sents.first <- unlist(tokenize_sentences(silence.sents.first))

silence.sents <- c(silence.sents.first)
silence.sents.df <- as.data.frame(silence.sents, stringsAsFactors = FALSE)

print(length(silence.sents.df$silence.sents))

silence.sents[999:1051]

bad_spots <-c(0)
for(i in seq(1:length(silence.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(silence.sents[i], nchar(silence.sents[i]), nchar(silence.sents[i]))
  test2 <- substr(silence.sents[i+1], 1, 1)
  test3 <- substr(silence.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      silence.sents[i] <- paste(silence.sents[i], silence.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
silence.sents[bad_spots]
silence.sents <- silence.sents[-c(bad_spots)]

silence.sents <- silence.sents[silence.sents!=""]
print(length(silence.sents))

silence.title <- rep("theBrandOfSilence", 4746)
silence.sents.type <- rep("sentence", 4746)
silence.sents.counter<-seq(1, 4746)
silence.sents.id <- paste0("THE_BRAND_OF_SILENCE_", "SENT_", silence.sents.counter)
silence.label <- rep("0", 4746)
print(length(silence.sents.id))

silence.sents.matrix <- cbind(silence.title, silence.sents.type, silence.sents.id, silence.sents, silence.label)
silence.sents.df <- as.data.frame(silence.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(silence.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", silence.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theBrandOfSilence' LIMIT 2")
dbDisconnect(con)

# words. 
silence.temp <- silence
silence.temp <- paste(silence.temp, collapse=" ")
silence.temp <-tolower(silence.temp)
# a better regex that is going to maintain contractions. important! 

silence.temp <- unlist(strsplit(silence.temp, "[^\\w']", perl=TRUE))
silence.not.blanks <- which(silence.temp != "")
silence.words <- silence.temp[silence.not.blanks]
print(length(silence.words))
silence.words<- silence.words[which(silence.words!="^'")]
silence.words<- silence.words[which(silence.words!="'")]
print(length(silence.words))
silence.words.df <- as.data.frame(silence.words, stringsAsFactors = FALSE)


silence.words.df <- as.data.frame(silence.words, stringsAsFactors = FALSE)

silence.title <- rep("theBrandOfSilence", 59094)
silence.words.type <- rep("word", 59094)
silence.words.counter <- seq(1, 59094)
silence.words.id <- paste0("THE_BRAND_OF_SILENCE_", "WORD_", silence.words.counter)
silence.label<- rep("0", 59094)
silence.words.matrix <- cbind(silence.title, silence.words.type, silence.words.id, silence.words, silence.label)

silence.words.df <- as.data.frame(silence.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(silence.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", silence.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theBrandOfSilence' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
