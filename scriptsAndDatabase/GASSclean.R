setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("stringi")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
# library(rJava)
# library("openNLPdata")

# gass. going to put all his stories into one unit. can separate them into each story later
# if i think that's necessary but in the db, they'll all live together. 

stock <- c("Title", "Type", "ID", "Unit")
gas <- scan("rawTexts/william-h-gass-in-the-heart-of-the-heart-of-the-country.txt",what="character",sep="\n")
gas.start <-which(gas=="Few of the stories one has it in one’s self to speak get spoken, because the heart rarely confesses to intelligence its deeper needs; and few of the stories one has at the top of one’s head to tell get told, because the mind does not always possess the voice for them. Even when the voice is there, and the tongue is limber as if with liquor or with love, where is that sensitive, admiring, other pair of ears?")
gas.fin <-which(gas=="It is the week of Christmas and the stores, to accommodate the rush they hope for, are remaining open in the evening. You can see snow falling in the cones of the street lamps. The roads are filling—undisturbed. Strings of red and green lights droop over the principal highway, and the water tower wears a star. The windows of the stores have been bedizened. Shamelessly they beckon. But I am alone, leaning against a pole—no . . . there is no one in sight. They’re all at home, perhaps by their instruments, tuning in on their evenings, and like Ramona, tirelessly playing and replaying themselves. There’s a speaker perched in the tower, and through the boughs of falling snow and over the vacant streets, it drapes the twisted and metallic strains of a tune that can barely be distinguished—yes, I believe it’s one of the jolly ones, it’s “Joy to the World.” There’s no one to hear the music but myself, and though I’m listening, I’m no longer certain. Perhaps the record’s playing something else.")
gas<-gas[gas.start:gas.fin]

# okay, time to clean. 
# leave in the story titles. but get rid of chap markers. 

gas <- gsub('Part One|Part Two|Part Three|_|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)|For Joanne, Oliver, and Allan', '', perl=TRUE,gas)
gas <- gas[-c(grep('^[1-9]$', gas, perl = TRUE))]
gas <- gsub('Mrs.', 'Mrs', perl=TRUE,gas)
gas <- gsub('MRS.', 'MRS', perl=TRUE,gas)
gas <- gsub('Mr.', 'Mr', perl=TRUE,gas)

## sents
print(length(gas))

first_bite <- gas[1:1263]

gas.sents.first <- paste0(first_bite, collapse = "\n")
gas.sents.first <- unlist(tokenize_sentences(gas.sents.first))

gas.sents <- c(gas.sents.first)

gas.sents.df <- as.data.frame(gas.sents, stringsAsFactors = FALSE)

# fix.


bad_spots<-c(0)

## standalone, you need the third condition. 
for(i in seq(1:length(gas.sents))){
  #if the sentence ends with a punctuation mark and the next sentence starts with a lowercase, combine them
  test <- substr(gas.sents[i], nchar(gas.sents[i]), nchar(gas.sents[i]))
  test2 <- substr(gas.sents[i+1], 1, 1)
  test3 <- substr(gas.sents[i], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
    gas.sents[i] <- paste(gas.sents[i], gas.sents[i+1])
    # print(gas.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}
bad_spots <- bad_spots[-c(1)]
gas.sents <- gas.sents[-bad_spots]
print(length(gas.sents))

# with quote? 

bad_spots <-c(0)
for(i in seq(1:length(gas.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(gas.sents[i], nchar(gas.sents[i])-1, nchar(gas.sents[i]))
  test2 <- substr(gas.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?”", "!”") && test2==tolower(test2)){
      gas.sents[i] <- paste(gas.sents[i], gas.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

gas.sents[bad_spots]
gas.sents <- gas.sents[-c(bad_spots)]
print(length(gas.sents))

gas.title <- rep("heartOfTheCountry", 5904)
gas.sents.type <- rep("sentence", 5904)
gas.sents.counter<-seq(1, 5904)
gas.sents.id <- paste0("HEART_OF_THE_COUNTRY_", "SENT_", gas.sents.counter)
print(length(gas.sents.id))
gas.sents.matrix <- cbind(gas.title, gas.sents.type, gas.sents.id, gas.sents)
gas.sents.df <- as.data.frame(gas.sents.matrix, stringsAsFactors = FALSE)
colnames(gas.sents.df) <- stock
# okay i think it's good now.
# gas To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", gas.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='heartOfTheCountry' LIMIT 2")

dbDisconnect(con)

### words. 


stock <- c("Title", "Type", "ID", "Unit")
gas <- scan("rawTexts/william-h-gass-in-the-heart-of-the-heart-of-the-country.txt",what="character",sep="\n")
gas.start <-which(gas=="Few of the stories one has it in one’s self to speak get spoken, because the heart rarely confesses to intelligence its deeper needs; and few of the stories one has at the top of one’s head to tell get told, because the mind does not always possess the voice for them. Even when the voice is there, and the tongue is limber as if with liquor or with love, where is that sensitive, admiring, other pair of ears?")
gas.fin <-which(gas=="It is the week of Christmas and the stores, to accommodate the rush they hope for, are remaining open in the evening. You can see snow falling in the cones of the street lamps. The roads are filling—undisturbed. Strings of red and green lights droop over the principal highway, and the water tower wears a star. The windows of the stores have been bedizened. Shamelessly they beckon. But I am alone, leaning against a pole—no . . . there is no one in sight. They’re all at home, perhaps by their instruments, tuning in on their evenings, and like Ramona, tirelessly playing and replaying themselves. There’s a speaker perched in the tower, and through the boughs of falling snow and over the vacant streets, it drapes the twisted and metallic strains of a tune that can barely be distinguished—yes, I believe it’s one of the jolly ones, it’s “Joy to the World.” There’s no one to hear the music but myself, and though I’m listening, I’m no longer certain. Perhaps the record’s playing something else.")
gas<-gas[gas.start:gas.fin]
# okay, time to clean. 
# leave in the story titles. but get rid of chap markers. 

gas <- gsub('Part One|Part Two|Part Three|_|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)|For Joanne, Oliver, and Allan', '', perl=TRUE,gas)
gas <- gas[-c(grep('^[1-9]$', gas, perl = TRUE))]
gas <- gsub('Mrs.', 'Mrs', perl=TRUE,gas)
gas <- gsub('MRS.', 'MRS', perl=TRUE,gas)
gas <- gsub('Mr.', 'Mr', perl=TRUE,gas)

gas.not.blanks <- which(gas != "")
gas <- gas[gas.not.blanks]

gas.temp <- gas
gas.temp <- paste(gas.temp, collapse=" ")
gas.temp <-tolower(gas.temp)
# a better regex that is going to maintain contractions. important! 

gas.temp <- unlist(strsplit(gas.temp, "[^\\w’]", perl=TRUE))
gas.not.blanks <- which(gas.temp != "")
gas.words <- gas.temp[gas.not.blanks]

gas.title <- rep("heartOfTheCountry", 72073)
gas.words.type <- rep("word", 72073)
gas.words.counter <- seq(1, 72073)
gas.words.id <- paste0("HEART_OF_THE_COUNTRY_", "WORD_", gas.words.counter)

gas.words.matrix <- cbind(gas.title, gas.words.type, gas.words.id, gas.words)

gas.words.df <- as.data.frame(gas.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(gas.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", gas.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='heartOfTheCountry' LIMIT 10")
dbDisconnect(con)

# paras. 

gas.paragraphs <- read.csv("Python_Scripts/checkCorpus/COUNTRYparas.csv", stringsAsFactors = FALSE)
gas.paragraphs <- as.data.frame(unlist(strsplit(gas.paragraphs$X0[9:40], "\n\n", perl=TRUE)), stringsAsFactors=FALSE)
colnames(gas.paragraphs) <- c("paragraph")

gas.paragraphs <- gas.paragraphs %>%
  transmute(paras = gsub('Part One|Part Two|Part Three|_|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)|For Joanne, Oliver, and Allan', '', perl=TRUE, paragraph))

gas.paragraphs <- gas.paragraphs %>%
  transmute(paragraph = gsub('Mrs.', 'Mrs', perl=TRUE, paras))

gas.paragraphs <- gas.paragraphs %>%
  transmute(paras = gsub('Mr.', 'Mr', perl=TRUE, paragraph))

gas.paragraphs <- gas.paragraphs %>%
  transmute(paragraph = gsub('MRS.', 'MRS', perl=TRUE, paras))

gas.paragraphs <- gas.paragraphs %>%
  transmute(paras = gsub('^[1-9]$', '', perl=TRUE, paragraph))
gas.paragraphs <- gas.paragraphs %>% filter(paras!="")
gas.paragraphs <- as.data.frame(gas.paragraphs[-c(102:103,238,364,824,856, 875,921,949,982,1034),])
# okay good.

gas.title <- rep("heartOfTheCountry", 1260)
gas.para.type <- rep("paragraph", 1260)
gas.para.counter<-seq(1, 1260)
gas.para.id <- paste0("HEART_OF_THE_COUNTRY_", "PARAGRAPH_", gas.para.counter)
print(length(gas.para.id))
gas.para.matrix <- cbind(gas.title, gas.para.type, gas.para.id, gas.paragraphs)
gas.para.df <- as.data.frame(gas.para.matrix, stringsAsFactors = FALSE)
colnames(gas.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", gas.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='heartOfTheCountry' LIMIT 2")
dbDisconnect(con)

# done.