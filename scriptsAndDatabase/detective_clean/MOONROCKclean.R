setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

moon <- scan("rawTexts/detective/arthur-j-rees-the-moon-rock.txt",what="character",sep="\n")
moon.start <- which(moon =="The voice of the clergyman intoned the last sad hope of humanity, the final prayer was said, and the mourners turned away, leaving Mrs. Turmoon to take her rest in a bleak Cornish churchyard among strangers, far from the place of her birth and kindred.")
moon.fin <- which(moon=="His eyes, dwelling on the door of the inner room, revealed the direction of his thought.")
moon <- moon[moon.start:moon.fin]

moon[grep("Chapter.X{0,3}(IX|IV|V?I{0,3}).", moon)]

moon.paragraphs <- as.data.frame(moon, stringsAsFactors=FALSE)
colnames(moon.paragraphs) <- c("paras")

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs))

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "", paras))

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paras=  gsub("Chapter.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs))

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paragraphs=  gsub("CHAPTER I", "", paras))

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paras=  gsub("\\*", "", paragraphs))

moon.paragraphs <- moon.paragraphs %>% 
  transmute(paragraphs = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paras))

moon.paragraphs <- moon.paragraphs %>% 
  filter(paragraphs!="")

moon.paragraphs <- moon.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(moon.paragraphs$paragraphs))

moon.title <- rep("theMoonRock", 2216)
moon.para.type <- rep("paragraph",2216)
moon.para.counter<-seq(1, 2216)
moon.para.id <- paste0("THE_MOON_ROCK_", "PARAGRAPH_", moon.para.counter)
moon.label <- rep("0", 2216)
print(length(moon.para.id))

moon.para.matrix <- cbind(moon.title, moon.para.type, moon.para.id, moon.paragraphs, moon.label)
moon.para.df <- as.data.frame(moon.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(moon.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", moon.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theMoonRock' LIMIT 2")
dbDisconnect(con)

# sents.
colnames(moon.paragraphs)
moon <- moon.paragraphs$paragraphs

first_bite <- moon[1:2216]

moon.sents.first <- paste0(first_bite, collapse = "\n")
moon.sents.first <- unlist(tokenize_sentences(moon.sents.first))

moon.sents <- c(moon.sents.first)
moon.sents.df <- as.data.frame(moon.sents, stringsAsFactors = FALSE)

print(length(moon.sents.df$moon.sents))


bad_spots <-c(0)
for(i in seq(1:length(moon.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a camoonal letter... but for eg ha! ha! ha! don't combine
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
moon.sents[bad_spots]
moon.sents <- moon.sents[-c(bad_spots)]

bad_spots <-c(0)
for(i in seq(1:length(moon.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(moon.sents[i], nchar(moon.sents[i])-1, nchar(moon.sents[i]))
  test2 <- substr(moon.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?”', '!”') && test2==tolower(test2)){
      #print(i)
      moon.sents[i] <- paste(moon.sents[i], moon.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

moon.sents[bad_spots]
moon.sents <- moon.sents[-c(bad_spots)]

moon.sents <- moon.sents[moon.sents!=""]
print(length(moon.sents))

moon.title <- rep("theMoonRock", 7184)
moon.sents.type <- rep("sentence", 7184)
moon.sents.counter<-seq(1, 7184)
moon.sents.id <- paste0("THE_MOON_ROCK_", "SENT_", moon.sents.counter)
moon.label <- rep("0", 7184)
print(length(moon.sents.id))

moon.sents.matrix <- cbind(moon.title, moon.sents.type, moon.sents.id, moon.sents, moon.label)
moon.sents.df <- as.data.frame(moon.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(moon.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", moon.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theMoonRock' LIMIT 2")
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
moon.words<- moon.words[which(moon.words!="’")]
print(length(moon.words))
moon.words.df <- as.data.frame(moon.words, stringsAsFactors = FALSE)

moon.title <- rep("theMoonRock", 107804)
moon.words.type <- rep("word", 107804)
moon.words.counter <- seq(1, 107804)
moon.words.id <- paste0("THE_MOON_ROCK_", "WORD_", moon.words.counter)
moon.label<- rep("0", 107804)
moon.words.matrix <- cbind(moon.title, moon.words.type, moon.words.id, moon.words, moon.label)

moon.words.df <- as.data.frame(moon.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(moon.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", moon.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theMoonRock' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
