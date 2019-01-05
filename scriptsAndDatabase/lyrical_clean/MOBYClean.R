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

stock <- c("Title", "Type", "ID", "Unit", "Label")

################
## MOBY DICK ##

moby <- scan("rawTexts/lyrical/herman-melville-moby-dick.txt",what="character",sep="\n")

moby.start<- which(moby == "Call me Ishmael. Some years ago—never mind how long precisely—having")
moby.end <- which(moby == "children, only found another orphan.")

moby<- moby[moby.start: moby.end]
moby <- replace_abbreviation(moby)
moby <- gsub('_', '', perl=TRUE, moby)

# taking inititave and grepping out chapter markers. 

moby<-  gsub('CHAPTER [0-9]+..*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', "", perl=TRUE, moby)
moby.not.blanks <- which(moby != "")
moby <- moby[moby.not.blanks]

length(moby)

# moby is 18951 lines. so really big. so have to split it a lot. 
first_bite <- moby[1:2499]
second_bite<- moby[2500:4999]
third_bite <- moby[5000:7499]
fourth_bite<- moby[7500:9999]
fifth_bite <- moby[10000:12499]
sixth_bite<- moby[15000:17499]
seventh_bite<- moby[17500:18951]

moby.sents.first <- paste0(first_bite, collapse = "\n")
moby.sents.first <- unlist(tokenize_sentences(moby.sents.first))

moby.sents.second <- paste0(second_bite, collapse = "\n")
moby.sents.second <- unlist(tokenize_sentences(moby.sents.second))

moby.sents.third <- paste0(third_bite, collapse = "\n")
moby.sents.third <- unlist(tokenize_sentences(moby.sents.third))

moby.sents.fourth <- paste0(fourth_bite, collapse = "\n")
moby.sents.fourth <- unlist(tokenize_sentences(moby.sents.fourth))

moby.sents.fifth <- paste0(fifth_bite, collapse = "\n")
moby.sents.fifth <- unlist(tokenize_sentences(moby.sents.fifth))

moby.sents.sixth <- paste0(sixth_bite, collapse = "\n")
moby.sents.sixth <- unlist(tokenize_sentences(moby.sents.sixth))

moby.sents.seventh <- paste0(seventh_bite, collapse = "\n")
moby.sents.seventh <- unlist(tokenize_sentences(moby.sents.seventh))

moby.sents <- c(moby.sents.first, moby.sents.second, moby.sents.third, moby.sents.fourth, moby.sents.fifth, moby.sents.sixth, moby.sents.seventh)
# moby.sents <- gsub('\"', '' , moby.sents, fixed=TRUE)
grep('\"', moby.sents)
# need to get rid of folio etc business from the Cetalogy chapter
moby.sents <- gsub('\\([A-z]+\\),.CHAPTER.[A-z]{1,}\\.', "", perl=TRUE,moby.sents)

# now we need to join sentences that end in punctuation and begin with a capital letter 

# test <- c("Hello my name is tim?")
# substr(test, nchar(test), nchar(test))

bad_spots<-c(0)

# first_boy <- c("Hello, my name is tim!")
# second_boy <- c(" he bellowed.")
# substr(moby.sents[1], nchar(moby.sents[1])-1, nchar(moby.sents[1]))
# substr(moby.sents[2], 1, 1)

for(i in seq(1:length(moby.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(moby.sents[i], nchar(moby.sents[i]), nchar(moby.sents[i]))
  test2 <- substr(moby.sents[i+1], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2)){
    moby.sents[i] <- paste(moby.sents[i], moby.sents[i+1])
    # print(moby.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}

moby.sents <- moby.sents[-bad_spots]
bad_spots <-c(0)
moby.sents[1004]
for(i in seq(1:length(moby.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(moby.sents[i], nchar(moby.sents[i])-1, nchar(moby.sents[i]))
  test2 <- substr(moby.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?”', '!”') && test2==tolower(test2)){
      #print(i)
      moby.sents[i] <- paste(moby.sents[i], moby.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
#moby.sents[bad_spots]
moby.sents <- moby.sents[-bad_spots]

moby.sents.df <- as.data.frame(moby.sents, stringsAsFactors = FALSE)

write.csv(moby.sents.df, 'mobysents.csv')
### need to check if this is correct. going to confirm with Great Gatsby.

moby.title <- rep("mobyDick", 7381)
moby.sents.type <- rep("sentence", 7381)
moby.sents.counter<-seq(1, 7381)
moby.sents.id <- paste0("MOBY_", "SENT_", moby.sents.counter)
print(length(moby.sents.id))
moby.sents.matrix <- cbind(moby.title, moby.sents.type, moby.sents.id, moby.sents)
moby.sents.df <- as.data.frame(moby.sents.matrix, stringsAsFactors = FALSE)
colnames(moby.sents.df) <- stock
# okay i think it's good now.
# Moby To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", moby.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='mobyDick' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Title='mobyDick'")

dbDisconnect(con)
#paras read in, clean, commit to db; 
## PARAS
moby.paragraphs <- read.csv("Python_Scripts/checkCorpus/MOBY_paras.csv", stringsAsFactors = FALSE)
moby.paragraphs <- moby.paragraphs[-c(1:537, 3129:3183),]
colnames(moby.paragraphs) <- c("arb", "para")
moby.paragraphs <- moby.paragraphs %>%
  mutate(paragraph = gsub('CHAPTER [0-9]+..*', "", perl=TRUE, para))
moby.paragraphs <- moby.paragraphs %>% 
  transmute(para = gsub('\\([A-z]+\\),.CHAPTER.[A-z]{1,}\\.', "", perl=TRUE,paragraph))

moby.paragraphs <- as.data.frame(moby.paragraphs[-which(moby.paragraphs$para==""),], stringsAsFactors = FALSE)

colnames(moby.paragraphs) <- c("para")

moby.paragraphs <- moby.paragraphs %>%
  transmute(paragraph = str_replace_all(para, "[\n]", " "))
moby.paragraphs <- moby.paragraphs %>%
  transmute(para = str_replace_all(paragraph, "_", ""))

moby.paragraphs <- as.data.frame(moby.paragraphs[-c(2456),], stringsAsFactors = FALSE)
colnames(moby.paragraphs) <- c("para")

# need to get rid of _
# need to join lowercase start with uppercase start

bad<-c(0)
for(i in seq(1:length(moby.paragraphs$para))){
  test <- substr(moby.paragraphs$para[i], 1, 1)
  if(test %in% c(LETTERS, letters) && test == tolower(test)){
    moby.paragraphs$para[i-1] <- paste(moby.paragraphs$para[i-1], moby.paragraphs$para[i])
    bad<-append(bad, i)
    print(i)
  }
}
moby.paragraphs <- as.data.frame(moby.paragraphs[-c(bad),], stringsAsFactors = FALSE)
colnames(moby.paragraphs) <- c("para")
moby.paragraphs <- as.data.frame(moby.paragraphs[-c(577, 586),], stringsAsFactors = FALSE)
colnames(moby.paragraphs) <- c("para")

moby.paragraphs <- moby.paragraphs %>%
  mutate(paragraph = gsub('BOOK.I{1,}\\.', "", perl=TRUE, para))

moby.paragraphs <- as.data.frame(moby.paragraphs$paragraph)
colnames(moby.paragraphs) <- c("para")
moby.paragraphs <- as.data.frame(moby.paragraphs[-c(590, 1071),], stringsAsFactors = FALSE)
colnames(moby.paragraphs) <- c("para")

moby.title <- rep("mobyDick", 2434)
moby.paras.type <- rep("paragraph", 2434)
moby.paras.counter<-seq(1, 2434)
moby.label <- rep("1", 2434)
moby.paras.id <- paste0("MOBY_DICK_", "PARAGRAPH_", moby.paras.counter)
moby.paras.matrix <- cbind(moby.title, moby.paras.type, moby.paras.id, moby.paragraphs, moby.label)
moby.paras.df <- as.data.frame(moby.paras.matrix, stringsAsFactors = FALSE)
colnames(moby.paras.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", moby.paras.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='mobyDick' LIMIT 2")
dbDisconnect(con)
# scoop into db. 
#words.. pretty easy. clean, scoop into db.

moby <- scan("rawTexts/herman-melville-moby-dick.txt",what="character",sep="\n")

moby.start<- which(moby == "Call me Ishmael. Some years ago—never mind how long precisely—having")
moby.end <- which(moby == "children, only found another orphan.")

moby<- moby[moby.start: moby.end]
moby <- replace_abbreviation(moby)
moby <- gsub('_', '', perl=TRUE, moby)

# taking inititave and grepping out chapter markers. 

moby<-  gsub('CHAPTER [0-9]+..*', "", perl=TRUE, moby)
moby <- gsub('\\([A-z]+\\),.CHAPTER.[A-z]{1,}\\.\|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', "", perl=TRUE,moby)
moby <- gsub('BOOK.I{1,}\\.', "", perl=TRUE, moby)
# moby <- gsub("[0-9]", '', moby)
moby.not.blanks <- which(moby != "")
moby <- moby[moby.not.blanks]

moby.temp <- moby
moby.temp <- paste(moby.temp, collapse=" ")
moby.temp <-tolower(moby.temp)
# a better regex that is going to maintain contractions. important! 
moby.temp <- unlist(strsplit(moby.temp, "[^\\w']", perl=T))
moby.not.blanks <- which(moby.temp != "")
moby.words <- moby.temp[moby.not.blanks]

# lots of words! 
print(length(moby.words))
# 214062


moby.title <- rep("mobyDick", 214183)
moby.words.type <- rep("word", 214183)
moby.words.counter <- seq(1, 214183)
moby.words.id <- paste0("MOBY_DICK_", "WORD_", moby.words.counter)

moby.words.matrix <- cbind(moby.title, moby.words.type, moby.words.id, moby.words)

moby.words.df <- as.data.frame(moby.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(moby.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", moby.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='mobyDick' LIMIT 10")
dbDisconnect(con)

