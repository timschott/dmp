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

## Billy Budd
## Delete the Rest. 

stock <- c("Title", "Type", "ID", "Unit")
bud <- scan("rawTexts/herman-melville-billy-budd.txt",what="character",sep="\n")
bud.start <- which(bud=="In the time before steamships, or then more frequently than now, a")
bud.fin <- which(bud=="I am sleepy, and the oozy weeds about me twist.")
bud<-bud[bud.start:bud.fin]
# chapter 1 - 29

bud <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE,bud)
bud <- bud[-c(grep('Chapter [0-9]+', bud, perl = TRUE))]
bud <- gsub('Mrs.', 'Mrs', perl=TRUE,bud)
bud <- gsub('MRS.', 'MRS', perl=TRUE,bud)
bud <- gsub('Mr.', 'Mr', perl=TRUE,bud)
bud <- gsub('\\\\', '',bud)
bud <- gsub('Camo\xebns', 'CamoÎns', perl=TRUE, bud)
bud <- replace_abbreviation(bud)


print(length(bud))


first_bite <- bud[1:2690]

bud.sents.first <- paste0(first_bite, collapse = "\n")
bud.sents.first <- unlist(tokenize_sentences(bud.sents.first))

bud.sents <- c(bud.sents.first)
print(length(bud.sents))

bud.sents <-bud.sents[bud.sents!=""]
print(length(bud.sents))

bud.sents.df <- as.data.frame(bud.sents, stringsAsFactors = FALSE)
bud.sents[465] <-paste0(bud.sents[465], bud.sents[466])
bud.sents[466] <-""
bad_spots<-c(0)

## standalone, you need the third condition. 
for(i in seq(1:length(bud.sents))){
  #if the sentence ends with a punctuation mark and the next sentence starts with a lowercase, combine them
  test <- substr(bud.sents[i], nchar(bud.sents[i]), nchar(bud.sents[i]))
  test2 <- substr(bud.sents[i+1], 1, 1)
  test3 <- substr(bud.sents[i], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
    bud.sents[i] <- paste(bud.sents[i], bud.sents[i+1])
    # print(bud.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}
bad_spots <- bad_spots[-c(1)]
bud.sents <- bud.sents[-bad_spots]
print(length(bud.sents))
bud.sents.df <- as.data.frame(bud.sents, stringsAsFactors = FALSE)
print(length(bud.sents))
bud.sents <- bud.sents[-c(460)]

bud.title <- rep("billyBudd", 1137)
bud.sents.type <- rep("sentence", 1137)
bud.sents.counter<-seq(1, 1137)
bud.sents.id <- paste0("BILLY_BUDD_", "SENT_", bud.sents.counter)
print(length(bud.sents.id))
bud.sents.matrix <- cbind(bud.title, bud.sents.type, bud.sents.id, bud.sents)
bud.sents.df <- as.data.frame(bud.sents.matrix, stringsAsFactors = FALSE)
colnames(bud.sents.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", bud.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='billyBudd' LIMIT 2")

dbDisconnect(con)

# words. 

stock <- c("Title", "Type", "ID", "Unit")
bud <- scan("rawTexts/herman-melville-billy-budd.txt",what="character",sep="\n")
bud.start <- which(bud=="In the time before steamships, or then more frequently than now, a")
bud.fin <- which(bud=="I am sleepy, and the oozy weeds about me twist.")
bud<-bud[bud.start:bud.fin]
# chapter 1 - 29

bud <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE,bud)
bud <- bud[-c(grep('Chapter [0-9]+', bud, perl = TRUE))]
bud <- gsub('Mrs.', 'Mrs', perl=TRUE,bud)
bud <- gsub('MRS.', 'MRS', perl=TRUE,bud)
bud <- gsub('Mr.', 'Mr', perl=TRUE,bud)
bud <- gsub('\\\\', '',bud)
bud <- gsub('Camo\xebns', 'CamoÎns', perl=TRUE, bud)
bud <- replace_abbreviation(bud)


print(length(bud))

bud.temp <- bud
bud.temp <- paste(bud.temp, collapse=" ")
bud.temp <-tolower(bud.temp)
# a better regex that is going to maintain contractions. important! 

bud.temp <- unlist(strsplit(bud.temp, "[^\\w']", perl=TRUE))
bud.not.blanks <- which(bud.temp != "")
bud.words <- bud.temp[bud.not.blanks]
print(length(bud.words))


bud.title <- rep("billyBudd", 30743)
bud.words.type <- rep("word", 30743)
bud.words.counter <- seq(1, 30743)
bud.words.id <- paste0("BILLY_BUDD_", "WORD_", bud.words.counter)

bud.words.matrix <- cbind(bud.title, bud.words.type, bud.words.id, bud.words)

bud.words.df <- as.data.frame(bud.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(bud.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", bud.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='billyBudd' LIMIT 10")
dbDisconnect(con)

# paras.
