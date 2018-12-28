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

stock <- c("Title", "Type", "ID", "Unit")
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

# inspect here the punctuation funny business. 

bad_spots<-c(0)

for(i in seq(1:length(gatsby.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(gatsby.sents[i], nchar(gatsby.sents[i]), nchar(gatsby.sents[i]))
  test2 <- substr(gatsby.sents[i+1], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2)){
    gatsby.sents[i] <- paste(gatsby.sents[i], gatsby.sents[i+1])
    bad_spots<-append(bad_spots, i+1)
  }
}
gatsby.sents[bad_spots]
gatsby.sents <- gatsby.sents[-bad_spots]
print(length(gatsby.sents))
gatsby.temp <- as.data.frame(gatsby.sents, stringsAsFactors = FALSE)
# back to good. 
gatsby.title <- rep("theGreatGatsby", 3289)
gatsby.sents.type <- rep("sentence", 3289)
gatsby.sents.id <- paste0("THE_GREAT_GATSBY_", "SENTENCE_", seq(1,3289))

# make matrix. 

gatsby.sents.matrix <- cbind(gatsby.title, gatsby.sents.type, gatsby.sents.id, gatsby.sents)
gatsby.sents.df <- as.data.frame(gatsby.sents.matrix)
colnames(gatsby.sents.df) <- stock


con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
#dbExecute(con, "DELETE * from textTable WHERE Type='sentence' AND Title ='theGreatGatsby'")

dbWriteTable(con, "textTable", gatsby.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theGreatGatsby' LIMIT 2")
dbDisconnect(con)

# gatsby para's
# should be easier. 
# read in from python. 

g.paragraphs <- read.csv("../Python_Scripts/checkCorpus/GATBSY_paras.csv", stringsAsFactors = FALSE)
g.paragraphs <- g.paragraphs[-c(1:20, 174, 225, 309, 473, 646, 805, 942, 1353, 1366,1619:1674),]

g.paragraphs$X0 <- replace_abbreviation(g.paragraphs$X0)
g.paragraphs$X0 <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, g.paragraphs$X0)

colnames(g.paragraphs) <- c("arbitrary", "para")

g.title <- rep("theGreatGatsby", 1589)
g.paragraphs.type <- rep("paragraph", 1589)
g.paragraphs.counter <- seq(1, 1589)
g.paragraphs.id <- paste0("THE_GREAT_GATSBY_", "PARAGRAPH_", g.paragraphs.counter)

g.paragraphs.matrix <- cbind(g.title, g.paragraphs.type, g.paragraphs.id, g.paragraphs$para)
g.paragraphs.df <- as.data.frame(g.paragraphs.matrix, stringsAsFactors = FALSE)
colnames(g.paragraphs.df) <- c("Title", "Type", "ID", "Unit")

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
### Columns. 
# we should no longer need to set the column names
dbWriteTable(con, "textTable", g.paragraphs.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theGreatGatsby' LIMIT 2")
summary(con)
dbDisconnect(con)

## need to do the words 


## gatsby, words. 
g.temp <- gatsby
g.temp <- paste(g.temp, collapse=" ")
g.temp <-tolower(g.temp)
# a better regex that is going to maintain contractions. important! 
g.temp <- unlist(strsplit(g.temp, "[^\\w']", perl=T))
g.not.blanks <- which(g.temp != "")
g.words <- g.temp[g.not.blanks]

g.title <- rep("theGreatGatsby", 48818)
g.words.type <- rep("word", 48818)
g.words.id <- paste0("THE_GREAT_GATSBY", "SENTENCE_", seq(1,48818))

# make the matrix. 

g.words.matrix <- cbind(g.title, g.words.type, g.words.id, g.words)
g.words.df <- as.data.frame(g.words.matrix, stringsAsFactors=FALSE)
colnames(g.words.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
### Columns. 
# we should no longer need to set the column names
dbWriteTable(con, "textTable", g.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='word' AND Title='theGreatGatsby' LIMIT 10")

