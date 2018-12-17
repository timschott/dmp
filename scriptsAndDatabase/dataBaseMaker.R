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
##### Let's try it with the tokenizer package.
#### SPLITTING INTO SENTENCES
# manually throw a \n at the end of every line
stock <- c("Title", "Type", "ID", "Unit")

#output <- unlist(tokenize_sentences(test_sentence))
# Mr. Mrs. 
heartOfDarkness <- scan("rawTexts/conrad-heart-of-darkness.txt",what="character",sep="\n")

heartOfDarkness.start<- which(heartOfDarkness == "I")
heartOfDarkness.end <- which(heartOfDarkness == "sky--seemed to lead into the heart of an immense darkness.")

heartOfDarkness<-heartOfDarkness[heartOfDarkness.start: heartOfDarkness.end]

#get rid of volume markers. 
heartOfDarkness.sents<-heartOfDarkness[-(1)]
heartOfDarkness.sents<-heartOfDarkness.sents[-(1160)]
heartOfDarkness.sents<-heartOfDarkness.sents[-(2128)]

heartOfDarkness.sents <- gsub('Mr\\.', 'Mr', heartOfDarkness.sents)

#paste is dumb with big inputs so break in half to be more manageable

#break near middle at full sentence. 
first_half <- heartOfDarkness.sents[1:1543]
second_half<- heartOfDarkness.sents[-(1:1543)]

heartOfDarkness.sents.first <- paste0(first_half, collapse = "\n")
heartOfDarkness.sents.first <- unlist(tokenize_sentences(heartOfDarkness.sents.first))

heartOfDarkness.sents.second <- paste0(second_half, collapse = "\n")
heartOfDarkness.sents.second <- unlist(tokenize_sentences(heartOfDarkness.sents.second))

#recombine
heartOfDarkness.sents <- c(heartOfDarkness.sents.first,heartOfDarkness.sents.second)

heartOfDarkness.sents <- gsub('([\\])', '', heartOfDarkness.sents)

#Mrs doesn't actually occur but well keep the pattern. heartOfDarkness.sents <- gsub('Mrs\\.', 'Mrs', heartOfDarkness.sents)

heartOfDarkness.title <- rep("heartOfDarkness", 2451)
heartOfDarkness.sents.type <- rep("sentence", 2451)

# now put those into a matrix 

heartOfDarkness.sents.counter <- seq(1, 2451)

# paste together HOD_SENT_COUNTER

heartOfDarkness.sents.id <- paste0("HOD_", "SENT_", heartOfDarkness.sents.counter)

heartOfDarkness.sents.matrix <- cbind(heartOfDarkness.title, heartOfDarkness.sents.type, heartOfDarkness.sents.id, heartOfDarkness.sents)

heartOfDarkness.sents.df <- as.data.frame(heartOfDarkness.sents.matrix)
colnames(heartOfDarkness.sents.df) <- c("Title", "Type", "ID", "Unit")

#write to database. 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
### Columns. 
# we should no longer need to set the column names
dbWriteTable(con, "textTable", heartOfDarkness.sents.df[0, ])
dbWriteTable(con, "textTable", heartOfDarkness.sents.df, append=TRUE, row.names=FALSE)

#dbGetQuery(con, "SELECT * FROM textTable LIMIT 10")

dbListTables(con)
summary(con)

dbDisconnect(con)

#### into words. 
heartOfDarkness <- scan("rawTexts/conrad-heart-of-darkness.txt",what="character",sep="\n")
heartOfDarkness.start<- which(heartOfDarkness == "I")
heartOfDarkness.end <- which(heartOfDarkness == "sky--seemed to lead into the heart of an immense darkness.")

heartOfDarkness<-heartOfDarkness[heartOfDarkness.start: heartOfDarkness.end]

## Grep out VOLUME markers

heartOfDarkness.temp<-heartOfDarkness[-(1)]
heartOfDarkness.temp<-heartOfDarkness.temp[-(1160)]
heartOfDarkness.temp<-heartOfDarkness.temp[-(2128)]
heartOfDarknessCount <-sum(sapply(heartOfDarkness.temp, str_count, "\\w+"))

## Replace Contractions
heartOfDarkness.temp<-replace_contraction(heartOfDarkness.temp)

#### WORDS? 

heartOfDarkness.temp <- paste(heartOfDarkness.temp, collapse=" ")
heartOfDarkness.temp <-tolower(heartOfDarkness.temp)
heartOfDarkness.temp <- strsplit(heartOfDarkness.temp, "\\W")

heartOfDarkness.temp <-unlist(heartOfDarkness.temp)

darkness.not.blanks <- which(heartOfDarkness.temp != "")

heartOfDarknessWords <- heartOfDarkness.temp[darkness.not.blanks]

heartOfDarknessWords[1:10]

# and do the same thing in adding it to the data base. 

heartOfDarkness.title <- rep("heartOfDarkness", 39085)
heartOfDarkness.words.type <- rep("word", 39085)
heartOfDarkness.words.counter <- seq(1, 39085)
heartOfDarkness.words.id <- paste0("HOD_", "WORD_", heartOfDarkness.words.counter)

heartOfDarkness.words.matrix <- cbind(heartOfDarkness.title, heartOfDarkness.words.type, heartOfDarkness.words.id, heartOfDarknessWords)

heartOfDarkness.words.df <- as.data.frame(heartOfDarkness.words.matrix)

#writeLines(heartOfDarknessWords, "heartOfDarknessWords.txt")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(heartOfDarkness.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", heartOfDarkness.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' LIMIT 10")
dbDisconnect(con)

### now add the paragraphs.. from python 
#### should just put the python file in this directory and then i can use the output 
## without having to manually update via copy paste. 

heartOfDarkness.paragraphs <- read.csv("paras/HOD_paras.csv", stringsAsFactors = FALSE)
# get rid of junk
heartOfDarkness.paragraphs <- heartOfDarkness.paragraphs[-c(1:12, 75, 113, 212:292),]

colnames(heartOfDarkness.paragraphs) <- c("arbitrary", "para")

heartOfDarkness.paragraphs <- heartOfDarkness.paragraphs %>%
  mutate(para = str_replace_all(para, "[\n]", " ")) %>%
  select(para)

heartOfDarkness.title <- rep("heartOfDarkness", 197)
heartOfDarkness.paragraphs.type <- rep("paragraph", 197)
heartOfDarkness.paragraphs.counter <- seq(1, 197)
heartOfDarkness.paragraphs.id <- paste0("HOD_", "PARAGRAPH_", heartOfDarkness.paragraphs.counter)

heartOfDarkness.paragraphs.matrix <- cbind(heartOfDarkness.title, heartOfDarkness.paragraphs.type, heartOfDarkness.paragraphs.id, heartOfDarkness.paragraphs$para)
heartOfDarkness.paragraphs.df <- as.data.frame(heartOfDarkness.paragraphs.matrix)
colnames(heartOfDarkness.paragraphs.df) <- c("Title", "Type", "ID", "Unit")

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
### Columns. 
# we should no longer need to set the column names
dbWriteTable(con, "textTable", heartOfDarkness.paragraphs.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' LIMIT 2")
summary(con)
dbDisconnect(con)

###NEXT, the Road####

theRoad <- scan("rawTexts/cormac-mccarthy-the-road.txt",what="character", sep="\n")
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

theRoad.paragraphs <- theRoad
theRoad.title <- rep("theRoad", 2467)
theRoad.paragraphs.type <- rep("paragraph", 2467)
theRoad.paragraphs.counter <- seq(1, 2467)

theRoad.paragraphs.id <- paste0("THE_ROAD_", "PARAGRAPH_", theRoad.paragraphs.counter)

theRoad.paragraphs.matrix <- cbind(theRoad.title, theRoad.paragraphs.type, theRoad.paragraphs.id, theRoad.paragraphs)
theRoad.paragraphs.df <- as.data.frame(theRoad.paragraphs.matrix)
colnames(theRoad.paragraphs.df) <- c("Title", "Type", "ID", "Unit")

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

# yay added 2. 
# onto the next. 
# gatsby! 

gatsby <- scan("rawTexts/fscott-fitzergald-the-great-gatsby.txt",what="character", sep="\n")
# clean headmatter
gatsby.start <- which(gatsby=="Chapter 1")
gatsby.end<- which(gatsby=="the past.")
# no head matter 
gatsby<-gatsby[gatsby.start:gatsby.end]
# get rid of chapter markers 
gatsby<- gsub('Chapter [0-9]', "", gatsby)
#https://rdrr.io/cran/qdap/man/replace_abbreviation.html
gatsby <- replace_abbreviation(gatsby)
gatsby <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, gatsby)
# split 
first_half <- gatsby[1:2501]
second_half<- gatsby[-(1:2501)]

gatsby.sents.first <- paste0(first_half, collapse = "\n")
gatsby.sents.first <- unlist(tokenize_sentences(gatsby.sents.first))

gatsby.sents.second <- paste0(second_half, collapse = "\n")
gatsby.sents.second <- unlist(tokenize_sentences(gatsby.sents.second))

gatsby.sents <- c(gatsby.sents.first, gatsby.sents.second)
gatsby.sents <- gsub('\"', '' , gatsby.sents, fixed=TRUE)

# back to good. 
gatsby.title <- rep("theGreatGatsby", 3413)
gatsby.sents.type <- rep("sentence", 3413)
gatsby.sents.id <- paste0("THE_GREAT_GATSBY_", "SENTENCE_", seq(1,3413))

# make matrix. 

gatsby.sents.matrix <- cbind(gatsby.title, gatsby.sents.type, gatsby.sents.id, gatsby.sents)
gatsby.sents.df <- as.data.frame(gatsby.sents.matrix)
colnames(gatsby.sents.df) <- stock


con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", gatsby.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theGreatGatsby' LIMIT 2")
dbDisconnect(con)

# gatsby para's
# should be easier. 
# read in from python. 

