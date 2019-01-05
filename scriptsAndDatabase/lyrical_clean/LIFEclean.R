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
# paras first. like pale fire. 

stock <- c("Title", "Type", "ID", "Unit", "Label")
life <- scan("rawTexts/lyrical/j-m-coetzee-michael-k.txt",what="character", sep="\n")
life.end<- which(life=="that they recognized who it was.")
life <- life[5:785]
chaps <- grep("^[0-9]+$", life)
life <- life[-chaps]
grep("^TWO$", life)
life <- life[-547]
grep("Mrs.", life)
life <- gsub("Mr\\.", "Mr", perl=TRUE, life)
life <- replace_abbreviation(life)

# print(length(life))
life <- life[life!=""]
# print(length(life))
life.paragraphs <- life
life.title <- rep("lifeAndTimesOfMichaelK", 762)
life.para.type <- rep("paragraph", 762)
life.para.counter<-seq(1, 762)
life.label <- rep("1", 762)
life.para.id <- paste0("LIFE_AND_TIMES_OF_MICHAEL_K_", "PARAGRAPH_", life.para.counter)
# print(length(life.para.id))
life.para.matrix <- cbind(life.title, life.para.type, life.para.id, life.paragraphs, life.label)
life.para.df <- as.data.frame(life.para.matrix, stringsAsFactors = FALSE)
colnames(life.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", life.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='lifeAndTimesOfMichaelK' LIMIT 150")
# the saddest query there ever was.
 # dbExecute(con, "DELETE FROM textTable WHERE Title='gravitysRainbow' OR Title='absalomAbsalom' OR Title='bloodMeridian' OR Title= 'somethingHappened' OR Title= 'wideSargassoSea' OR Title= 'lifeAndTimesOfMichaelK' OR Title='mrsDalloway' OR Title= 'theShriekingPit' OR Title= 'theMoonstone'")
dbDisconnect(con)

# sents. 

first_bite <- life[1:762]


life.sents.first <- paste0(first_bite, collapse = "\n")
life.sents.first <- unlist(tokenize_sentences(life.sents.first))


life.sents <- c(life.sents.first)

life.sents.df <- as.data.frame(life.sents, stringsAsFactors = FALSE)

#### 


bad_spots <-c(0)
for(i in seq(1:length(life.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(life.sents[i], nchar(life.sents[i]), nchar(life.sents[i]))
  test2 <- substr(life.sents[i+1], 1, 1)
  test3 <- substr(life.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      life.sents[i] <- paste(life.sents[i], life.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

life.sents <- life.sents[-c(bad_spots)]
print(length(life.sents))


bad_spots <-c(0)
for(i in seq(1:length(life.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(life.sents[i], nchar(life.sents[i])-1, nchar(life.sents[i]))
  test2 <- substr(life.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      life.sents[i] <- paste(life.sents[i], life.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

life.sents[bad_spots]
life.sents <- life.sents[-c(bad_spots)]
print(length(life.sents))

# cool. 

life.sents[1234:1244]
life.sents[grep("Mr", life.sents)]
life.title <- rep("lifeAndTimesOfMichaelK", 4253)
life.sents.type <- rep("sentence", 4253)
life.sents.counter<-seq(1, 4253)
life.label <- rep("1", 4253)
life.sents.id <- paste0("LIFE_AND_TIMES_OF_MICHAEL_K_", "SENT_", life.sents.counter)

print(length(life.sents.id))
life.sents.matrix <- cbind(life.title, life.sents.type, life.sents.id, life.sents, life.label)
life.sents.df <- as.data.frame(life.sents.matrix, stringsAsFactors = FALSE)
colnames(life.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", life.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='lifeAndTimesOfMichaelK' LIMIT 2")
dbDisconnect(con)

## words. 

life.temp <- life
life.temp <- paste(life.temp, collapse=" ")
life.temp <-tolower(life.temp)
# a better regex that is going to maintain contractions. important! 

life.temp <- unlist(strsplit(life.temp, "[^\\w']", perl=TRUE))
life.not.blanks <- which(life.temp != "")
life.words <- life.temp[life.not.blanks]
print(length(life.words))
grep("^'", life.words)
life.words<- gsub("^'",'',perl=TRUE, life.words)
life.words <- life.words[which(life.words !="")]
print(length(life.words))

#db commit. 
life.title <- rep("lifeAndTimesOfMichaelK", 66354)
life.words.type <- rep("word", 66354)
life.words.counter <- seq(1, 66354)
life.words.id <- paste0("LIFE_AND_TIMES_OF_MICHAEL_K_", "WORD_", life.words.counter)
life.label <- rep("1", 66354)
life.words.matrix <- cbind(life.title, life.words.type, life.words.id, life.words, life.label)

life.words.df <- as.data.frame(life.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(life.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", life.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='lifeAndTimesOfMichaelK' LIMIT 10")
dbDisconnect(con)



