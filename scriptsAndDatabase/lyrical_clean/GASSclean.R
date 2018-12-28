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

# the pedersen kid
stock <- c("Title", "Type", "ID", "Unit")
gas <- scan("rawTexts/william-h-gass-in-the-heart-of-the-heart-of-the-country.txt",what="character",sep="\n")
gas.start<-which(gas=="Big Hans yelled, so I came out. The barn was dark, but the sun burned on the snow. Hans was carrying something from the crib. I yelled, but Big Hans didn’t hear. He was in the house with what he had before I reached the steps.")
gas.fin<-which(gas=="It was pleasant not to have to stamp the snow off my boots, and the fire was speaking pleasantly and the kettle was sounding softly. There was no need for me to grieve. I had been the brave one and now I was free. The snow would keep me. I would bury pa and the Pedersens and Hans and even ma if I wanted to bother. I hadn’t wanted to come but now I didn’t mind. The kid and me, we’d done brave things well worth remembering. The way that fellow had come so mysteriously through the snow and done us such a glorious turn—well it made me think how I was told to feel in church. The winter time had finally got them all, and I really did hope that the kid was as warm as I was now, warm inside and out, burning up, inside and out, with joy.")
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

first_bite <- gas[1:793]

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

gas.sents <-gas.sents[gas.sents!=""]
print(length(gas.sents))
gas.title <- rep("thePedersenKid", 2622)
gas.sents.type <- rep("sentence", 2622)
gas.sents.counter<-seq(1, 2622)
gas.sents.id <- paste0("THE_PEDERSEN_KID_", "SENT_", gas.sents.counter)
print(length(gas.sents.id))
gas.sents.matrix <- cbind(gas.title, gas.sents.type, gas.sents.id, gas.sents)
gas.sents.df <- as.data.frame(gas.sents.matrix, stringsAsFactors = FALSE)
colnames(gas.sents.df) <- stock
# okay i think it's good now.
# gas To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", gas.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='thePedersenKid' LIMIT 2")

dbDisconnect(con)

### words. 

stock <- c("Title", "Type", "ID", "Unit")
gas <- scan("rawTexts/william-h-gass-in-the-heart-of-the-heart-of-the-country.txt",what="character",sep="\n")
gas.start<-which(gas=="Big Hans yelled, so I came out. The barn was dark, but the sun burned on the snow. Hans was carrying something from the crib. I yelled, but Big Hans didn’t hear. He was in the house with what he had before I reached the steps.")
gas.fin<-which(gas=="It was pleasant not to have to stamp the snow off my boots, and the fire was speaking pleasantly and the kettle was sounding softly. There was no need for me to grieve. I had been the brave one and now I was free. The snow would keep me. I would bury pa and the Pedersens and Hans and even ma if I wanted to bother. I hadn’t wanted to come but now I didn’t mind. The kid and me, we’d done brave things well worth remembering. The way that fellow had come so mysteriously through the snow and done us such a glorious turn—well it made me think how I was told to feel in church. The winter time had finally got them all, and I really did hope that the kid was as warm as I was now, warm inside and out, burning up, inside and out, with joy.")
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
print(length(gas.words))
gas.title <- rep("thePedersenKid", 23702)
gas.words.type <- rep("word", 23702)
gas.words.counter <- seq(1, 23702)
gas.words.id <- paste0("THE_PEDERSEN_KID_", "WORD_", gas.words.counter)

gas.words.matrix <- cbind(gas.title, gas.words.type, gas.words.id, gas.words)

gas.words.df <- as.data.frame(gas.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(gas.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", gas.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='thePedersenKid' LIMIT 10")
dbDisconnect(con)

# paras. 
gas.paragraphs <- read.csv("Python_Scripts/checkCorpus/COUNTRYparas.csv", stringsAsFactors = FALSE)
gas.paragraphs <- as.data.frame(unlist(strsplit(gas.paragraphs$X0[24:26], "\n\n", perl=TRUE)), stringsAsFactors=FALSE)
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
gas.paragraphs <- gas.paragraphs %>% filter(paras!="\n")
gas.paragraphs <- gas.paragraphs %>%
  transmute(paragraph = gsub('\n[1-9]', '', perl=TRUE, paras))
gas.paragraphs <- gas.paragraphs %>% filter(paragraph!="")
gas.paragraphs <- as.data.frame(gas.paragraphs[-c(1:2,408,719),], stringsAsFactors = FALSE)
print(length(gas.paragraphs$`gas.paragraphs[-c(1:2, 408, 719), ]`))
colnames(gas.paragraphs) <- c("paras")
# okay good.

gas.title <- rep("thePedersenKid", 789)
gas.para.type <- rep("paragraph", 789)
gas.para.counter<-seq(1, 789)
gas.para.id <- paste0("THE_PEDERSEN_KID_", "PARAGRAPH_", gas.para.counter)
print(length(gas.para.id))
gas.para.matrix <- cbind(gas.title, gas.para.type, gas.para.id, gas.paragraphs)
gas.para.df <- as.data.frame(gas.para.matrix, stringsAsFactors = FALSE)
colnames(gas.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", gas.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='thePedersenKid' LIMIT 2")
dbDisconnect(con)

# done.

