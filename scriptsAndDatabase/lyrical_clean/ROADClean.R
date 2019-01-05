setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)


stock <- c("Title", "Type", "ID", "Unit", "Label")

###NEXT, the Road####

theRoad <- scan("rawTexts/lyrical/cormac-mccarthy-the-road.txt",what="character", sep="\n")
#get rid of all numbers
theRoad <- gsub("[0-9]", '', theRoad)
road.not.blanks <- which(theRoad != "")
theRoad <- theRoad[road.not.blanks]

#sentences

#\r\n[a-z]

#keep a bad list and then get rid of those
bad<-c(0)
for(i in seq(1:length(theRoad))){
  test <- substr(theRoad[i], 1, 1)
  if(test == tolower(test)){
    theRoad[i-1] <- paste(theRoad[i-1], theRoad[i])
    bad<-append(bad, i)
  }
}
theRoad <- theRoad[-bad]
theRoad <- theRoad[-c(1:6)]
theRoad <- theRoad[-c(2469:2484)]
theRoad<- theRoad[-c(2468)]
length(theRoad)
theRoad.paragraphs <- theRoad
theRoad.title <- rep("theRoad", 2467)
theRoad.paragraphs.type <- rep("paragraph", 2467)
theRoad.paragraphs.counter <- seq(1, 2467)
theRoad.label <- rep("1", 2467)
theRoad.paragraphs.id <- paste0("THE_ROAD_", "PARAGRAPH_", theRoad.paragraphs.counter)

theRoad.paragraphs.matrix <- cbind(theRoad.title, theRoad.paragraphs.type, theRoad.paragraphs.id, theRoad.paragraphs, theRoad.label)
theRoad.paragraphs.df <- as.data.frame(theRoad.paragraphs.matrix)
colnames(theRoad.paragraphs.df) <- c("Title", "Type", "ID", "Unit", "Label")

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
### Columns. 
# we should no longer need to set the column names
dbWriteTable(con, "textTable", theRoad.paragraphs.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theRoad' LIMIT 2")
dbDisconnect(con)
## add to DB.

#### CLEANED, ADDED ROAD PARAGRAPHS. NOW LET'S DO WORDS AND SENTS.

theRoad.sentences <- theRoad

# need to deal with the following:
# Papa are we going to die? he said. 
# the last string of i is a ? and the first string of the next is lower case . .. 

first_half <- theRoad.sentences[1:1350]
second_half<- theRoad.sentences[-(1:1350)]

theRoad.sents.first <- paste0(first_half, collapse = "\n")
theRoad.sents.first <- unlist(tokenize_sentences(theRoad.sents.first))

theRoad.sents.second <- paste0(second_half, collapse = "\n")
theRoad.sents.second <- unlist(tokenize_sentences(theRoad.sents.second))

theRoad.sentences <- c(theRoad.sents.first, theRoad.sents.second)
test <- c("Hello my name is tim?")
substr(test, nchar(test), nchar(test))

bad_spots<-c(0)
for(i in seq(1:length(theRoad.sentences))){
  test <- substr(theRoad.sentences[i], 1, 1)
  if(test == tolower(test)){
    print(test)
    theRoad.sentences[i-1] <- paste(theRoad.sentences[i-1], theRoad.sentences[i])
    bad_spots<-append(bad_spots, i)
  }
}
theRoad.sentences <- theRoad.sentences[-bad_spots]

# lets add to db. 
theRoad.title <- rep("theRoad", 6533)
theRoad.sents.type <- rep("sentence", 6533)
theRoad.sents.counter <- seq(1, 6533)
theRoad.sents.id <- paste0("THE_ROAD_", "SENTENCE_", seq(1,6533))

theRoad.sents.matrix <- cbind(theRoad.title, theRoad.sents.type, theRoad.sents.id, theRoad.sentences)
theRoad.sents.df <- as.data.frame(theRoad.sents.matrix)
colnames(theRoad.sents.df) <- c("Title", "Type", "ID", "Unit")
# add to db 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
### Columns. 
# we should no longer need to set the column names
dbWriteTable(con, "textTable", theRoad.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theRoad' LIMIT 2")
dbDisconnect(con)

## the road, words. 
theRoad.temp <- theRoad
theRoad.temp <- paste(theRoad.temp, collapse=" ")
theRoad.temp <-tolower(theRoad.temp)
# a better regex that is going to maintain contractions. important! 
theRoad.temp <- unlist(strsplit(theRoad.temp, "[^\\w']", perl=T))
road.not.blanks <- which(theRoad.temp != "")
theRoad.words <- theRoad.temp[road.not.blanks]

theRoad.title <- rep("theRoad", 58702)
theRoad.words.type <- rep("word", 58702)
theRoad.words.id <- paste0("THE_ROAD_", "WORD_", seq(1,58702))

# make the matrix. 

theRoad.words.matrix <- cbind(theRoad.title, theRoad.words.type, theRoad.words.id, theRoad.words)
theRoad.words.df <- as.data.frame(theRoad.words.matrix)
colnames(theRoad.words.df) <- stock
# add to db 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
### Columns. 
# we should no longer need to set the column names
dbWriteTable(con, "textTable", theRoad.words.df, append=TRUE, row.names=FALSE)
#dbExecute(con, "DELETE from textTable WHERE Type='words'")

dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='word' AND Title='theRoad' LIMIT 2")
dbDisconnect(con)