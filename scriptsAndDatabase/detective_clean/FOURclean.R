setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

four <- scan("rawTexts/detective/arthur-conan-doyle-the-sign-of-four.txt",what="character",sep="\n")

four.start <- which(four=="Sherlock Holmes took his bottle from the corner of the mantel-piece and his hypodermic syringe from its neat morocco case. With his long, white, nervous fingers he adjusted the delicate needle, and rolled back his left shirt-cuff. For some little time his eyes rested thoughtfully upon the sinewy forearm and wrist all dotted and scarred with innumerable puncture-marks. Finally he thrust the sharp point home, pressed down the tiny piston, and sank back into the velvet-lined arm-chair with a long sigh of satisfaction.")
four.end <- which(four == "\"For me,\" said Sherlock Holmes, \"there still remains the cocaine-bottle.\" And he stretched his long white hand up for it.")
four<-four[four.start:four.end]
print(length(four))

four[chapters <- grep('Chapter.X{0,3}(IX|IV|V?I{0,3}).', four)]
chapters <- c(chapters, chapters+1)
four <- four[-c(chapters)]
four.paragraphs <- as.data.frame(four, stringsAsFactors=FALSE)
colnames(four.paragraphs) <- c("paras")
four.paragraphs <- four.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

colnames(four.paragraphs) <- c("paras")

four.paragraphs <- four.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

four.paragraphs <- four.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs))

four.paragraphs <- four.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "", paras))

four.paragraphs <- four.paragraphs %>% 
  transmute(paras=  gsub("\\*", "", paragraphs))

four.paragraphs <- four.paragraphs %>% 
  transmute(paragraphs = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paras))

four.paragraphs <- four.paragraphs %>% 
  filter(paragraphs!="")

four.paragraphs <- four.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(four.paragraphs$paragraphs))

four.title <- rep("theSignOfFour", 772)
four.para.type <- rep("paragraph",772)
four.para.counter<-seq(1, 772)
four.para.id <- paste0("THE_SIGN_OF_FOUR_", "PARAGRAPH_", four.para.counter)
four.label <- rep("0", 772)
print(length(four.para.id))

four.para.matrix <- cbind(four.title, four.para.type, four.para.id, four.paragraphs, four.label)
four.para.df <- as.data.frame(four.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(four.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", four.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theSignOfFour' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Type='paragraph' AND Title='thefourodilMystery'")
# dbExecute(con, "DELETE FROM textTable WHERE Type='sentence' AND Title='thefourodilMystery'")

dbDisconnect(con)


four <- four.paragraphs$paragraphs

first_bite <- four[1:772]

four.sents.first <- paste0(first_bite, collapse = "\n")
four.sents.first <- unlist(tokenize_sentences(four.sents.first))

four.sents <- c(four.sents.first)
four.sents.df <- as.data.frame(four.sents, stringsAsFactors = FALSE)

print(length(four.sents.df$four.sents))
bad_spots <-c(0)
for(i in seq(1:length(four.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(four.sents[i], nchar(four.sents[i])-1, nchar(four.sents[i]))
  test2 <- substr(four.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      four.sents[i] <- paste(four.sents[i], four.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
four.sents[bad_spots]
four.sents <- four.sents[-c(bad_spots)]

four.sents <- four.sents[four.sents!=""]
print(length(four.sents))
bad_spots <-c(0)
#### lowercase closure no punc.
for(i in seq(1:length(four.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # but if the sequence starts with a capital letter, eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(four.sents[i], nchar(four.sents[i]), nchar(four.sents[i]))
  test2 <- substr(four.sents[i+1], 1, 1)
  test3 <- substr(four.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      four.sents[i] <- paste(four.sents[i], four.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

four.sents[bad_spots]
four.sents <- four.sents[-c(bad_spots)]
print(length(four.sents))
four.sents.df <- as.data.frame(four.sents, stringsAsFactors = FALSE)

four.title <- rep("theSignOfFour", 2847)
four.sents.type <- rep("sentence", 2847)
four.sents.counter<-seq(1, 2847)
four.sents.id <- paste0("THE_SIGN_OF_FOUR_", "SENT_", four.sents.counter)
four.label <- rep("0", 2847)
print(length(four.sents.id))

four.sents.matrix <- cbind(four.title, four.sents.type, four.sents.id, four.sents, four.label)
four.sents.df <- as.data.frame(four.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(four.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", four.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theSignOfFour' LIMIT 10")
dbDisconnect(con)

# words. 
four.temp <- four
four.temp <- paste(four.temp, collapse=" ")
four.temp <-tolower(four.temp)
# a better regex that is going to maintain contractions. important! 

four.temp <- unlist(strsplit(four.temp, "[^\\w']", perl=TRUE))
four.not.blanks <- which(four.temp != "")
four.words <- four.temp[four.not.blanks]
print(length(four.words))

four.words<- four.words[which(four.words!="'")]
print(length(four.words))

# sick! 

four.title <- rep("theSignOfFour", 43445)
four.words.type <- rep("word", 43445)
four.words.counter <- seq(1, 43445)
four.words.id <- paste0("THE_SIGN_OF_FOUR_", "WORD_", four.words.counter)
four.label<- rep("0", 43445)
four.words.matrix <- cbind(four.title, four.words.type, four.words.id, four.words, four.label)

four.words.df <- as.data.frame(four.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(four.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", four.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theSignOfFour' LIMIT 10")
dbGetQuery(con, "SELECT DISTINCT title FROM textTable WHERE Label='0'")
dbDisconnect(con)
