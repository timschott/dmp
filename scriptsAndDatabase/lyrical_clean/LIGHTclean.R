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

# mOOre wolf.
stock <- c("Title", "Type", "ID", "Unit", "Label")
light <- scan("rawTexts/lyrical/virginia-woolf-to-the-lighthouse.txt",what="character", sep="\n")
light.start <- 37
light.end<- which(light=="I have had my vision.")
light <- light[light.start:light.end]

# need to grep out "\" --> PSYCH (love, future tim.)
chaps<- grep('[0-9+]', light)
chaps <- chaps[-c(5)]
light <- light[-c(chaps)]
# more. 
light <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)|_', '', perl=TRUE, light)
light <- gsub('\"', "'", perl=TRUE, light)

light <- gsub('Mrs.', 'Mrs', perl=TRUE, light)
light <- gsub('Mr\\.', 'Mr', perl=TRUE, light)

light.temp <- which(light != "")
light <- light[light.temp]

print(length(light))


first_bite <- light[1:2499]
second_bite<- light[2500:5714]

light.sents.first <- paste0(first_bite, collapse = "\n")
light.sents.first <- unlist(tokenize_sentences(light.sents.first))

light.sents.second <- paste0(second_bite, collapse = "\n")
light.sents.second <- unlist(tokenize_sentences(light.sents.second))

light.sents <- c(light.sents.first, light.sents.second)

light.sents.df <- as.data.frame(light.sents, stringsAsFactors = FALSE)

# nothing. 

bad_spots <-c(0)
for(i in seq(1:length(light.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(light.sents[i], nchar(light.sents[i]), nchar(light.sents[i]))
  test2 <- substr(light.sents[i+1], 1, 1)
  test3 <- substr(light.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      light.sents[i] <- paste(light.sents[i], light.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

light.sents <- light.sents[-c(bad_spots)]
print(length(light.sents))

bad_spots <-c(0)
for(i in seq(1:length(light.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(light.sents[i], nchar(light.sents[i])-1, nchar(light.sents[i]))
  test2 <- substr(light.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      light.sents[i] <- paste(light.sents[i], light.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
light.sents[bad_spots]
light.sents <- light.sents[-c(bad_spots)]
print(length(light.sents))
light.title <- rep("toTheLighthouse", 3401)
light.sents.type <- rep("sentence", 3401)
light.sents.counter<-seq(1, 3401)
light.sents.id <- paste0("TO_THE_LIGHTHOUSE_", "SENT_", light.sents.counter)
light.label <- rep("1", 3401)
print(length(light.sents.id))
light.sents.matrix <- cbind(light.title, light.sents.type, light.sents.id, light.sents, light.label)
light.sents.df <- as.data.frame(light.sents.matrix, stringsAsFactors = FALSE)

colnames(light.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
# dbExecute(con, "DELETE FROM textTable WHERE Type='sentence' AND Title='toTheLighthouse'")
dbWriteTable(con, "textTable", light.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='toTheLighthouse' LIMIT 2")
dbDisconnect(con)

########### words...

stock <- c("Title", "Type", "ID", "Unit")
light <- scan("rawTexts/virginia-woolf-to-the-lighthouse.txt",what="character", sep="\n")
light.start <- 37
light.end<- which(light=="I have had my vision.")
light <- light[light.start:light.end]

# need to grep out "\"
chaps<- grep('[0-9+]', light)
chaps <- chaps[-c(5)]
light <- light[-c(chaps)]
# more. 
light <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)|_', '', perl=TRUE, light)
light <- gsub('\"', '', perl=TRUE, light)

light <- gsub('Mrs.', 'Mrs', perl=TRUE, light)
light <- gsub('Mr.', 'Mr', perl=TRUE, light)

light.temp <- which(light != "")
light <- light[light.temp]

light.temp <- light
light.temp <- paste(light.temp, collapse=" ")
light.temp <-tolower(light.temp)
# a better regex that is going to maintain contractions. important! 

light.temp <- unlist(strsplit(light.temp, "[^\\w']", perl=TRUE))
light.not.blanks <- which(light.temp != "")
light.words <- light.temp[light.not.blanks]
print(length(light.words))

# 69905 words. 

light.title <- rep("toTheLighthouse", 69905)
light.words.type <- rep("word", 69905)
light.words.counter <- seq(1, 69905)
light.words.id <- paste0("TO_THE_LIGHTHOUSE_", "WORD_", light.words.counter)
# the name of the game is light-words (thanks J Dilla)
light.words.matrix <- cbind(light.title, light.words.type, light.words.id, light.words)

light.words.df <- as.data.frame(light.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(light.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", light.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='toTheLighthouse' LIMIT 10")
dbDisconnect(con)

## paras..
light.paragraphs <- read.csv("Python_Scripts/checkCorpus/LIGHTparas.csv", stringsAsFactors = FALSE)

light.paragraphs <- light.paragraphs[-c(1:14,552:556),]
# bouncing gsubs
colnames(light.paragraphs) <- c("arb", "paras")
light.paragraphs <- light.paragraphs %>%
  transmute(paragraph = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)|_', '', perl=TRUE, paras))
light.paragraphs <- light.paragraphs %>%
  transmute(paras = gsub('\"', "'", perl=TRUE, paragraph))
light.paragraphs <- light.paragraphs %>%
  transmute(paragraph = gsub('[0-9]+', '', perl=TRUE, paras))
light.paragraphs <- light.paragraphs %>%
  transmute(paras = gsub('Mr\\.', 'Mr', perl=TRUE, paragraph))
light.paragraphs <- light.paragraphs %>%
  transmute(paragraph = gsub('Mrs\\.', 'Mrs', perl=TRUE, paras))
light.paragraphs <- light.paragraphs %>% filter(paragraph!="")
light.paragraphs <- light.paragraphs %>%
  transmute(paras = gsub('\n', ' ', perl=TRUE, paragraph))

light.title <- rep("toTheLighthouse", 495)
light.para.type <- rep("paragraph", 495)
light.para.counter<-seq(1, 495)
light.para.id <- paste0("TO_THE_LIGHTHOUSE_", "PARAGRAPH_", light.para.counter)
light.label<- rep("1", 495)
print(length(light.para.id))
light.para.matrix <- cbind(light.title, light.para.type, light.para.id, light.paragraphs, light.label)
light.para.df <- as.data.frame(light.para.matrix, stringsAsFactors = FALSE)
colnames(light.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", light.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='toTheLighthouse' LIMIT 2")
dbDisconnect(con)

# light done. 