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

# blood meridian.

blood <- scan("rawTexts/lyrical/cormac-mccarthy-blood-meridian.txt",what="character",sep="\n")
stock <- c("Title", "Type", "ID", "Unit", "Label")

blood.start<- which(blood == "See the child. He is pale and thin, he wears a thin and ragged linen shirt. He stokes the scullery fire. Outside lie dark turned fields with rags of snow and darker woods beyond that harbor yet a few last wolves. His folk are known for hewers of wood and drawers of water but in truth his father has been a schoolmaster. He lies in drink, he quotes from poets whose names are now lost. The boy crouches by the fire and watches him.")
blood.end <- which(blood == "In the dawn there is a man progressing over the plain by means of holes which he is making in the ground. He uses an implement with two handles and he chucks it into the hole and he enkindles the stone in the hole with his steel hole by hole striking the fire out of the rock which God has put there. On the plain behind him are the wanderers in search of bones and those who do not search and they move haltingly in the light like mechanisms whose movements are monitored with escapement and pallet so that they appear restrained by a prudence or reflectiveness which has no inner reality and they cross in their progress one by one that track of holes that runs to the rim of the visible ground and which seems less the pursuit of some continuance than the verification of a principle, a validation of sequence and causality as if each round and perfect hole owed its existence to the one before it there on that prairie upon which are the bones and the gatherers of bones and those who do not gather. He strikes fire in the hole and draws out his steel. Then they all move on again.")

blood <- blood[blood.start:blood.end]

blood <- replace_abbreviation(blood)
grep("Mr.", blood)

blood.paragraphs <- as.data.frame(blood, stringsAsFactors=FALSE)
colnames(blood.paragraphs) <- c("paras")
blood.paragraphs <- blood.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))
blood.paragraphs <- blood.paragraphs %>% 
  transmute(paras=  replace_abbreviation(paragraphs))
print(length(blood.paragraphs$paras))
blood.paragraphs <- blood.paragraphs %>% filter(paras!="")
print(length(blood.paragraphs$paras))

# good.
blood.title <- rep("bloodMeridian", 2640)
blood.para.type <- rep("paragraph", 2640)
blood.para.counter<-seq(1, 2640)
blood.label <- rep("1", 2640)
blood.para.id <- paste0("BLOOD_MERIDIAN_", "PARAGRAPH_", blood.para.counter)
print(length(blood.para.id))

blood.para.matrix <- cbind(blood.title, blood.para.type, blood.para.id, blood.paragraphs, blood.label)
blood.para.df <- as.data.frame(blood.para.matrix, stringsAsFactors = FALSE)
colnames(blood.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", blood.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='bloodMeridian' LIMIT 2")
dbDisconnect(con)


# sick. sents!

first_bite <- blood[1:2640]

blood.sents.first <- paste0(first_bite, collapse = "\n")
blood.sents.first <- unlist(tokenize_sentences(blood.sents.first))

blood.sents <- c(blood.sents.first)
blood.sents.df <- as.data.frame(blood.sents, stringsAsFactors = FALSE)

bad_spots <-c(0)
for(i in seq(1:length(blood.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(blood.sents[i], nchar(blood.sents[i]), nchar(blood.sents[i]))
  test2 <- substr(blood.sents[i+1], 1, 1)
  test3 <- substr(blood.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      blood.sents[i] <- paste(blood.sents[i], blood.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
blood.sents[bad_spots]
blood.sents <- blood.sents[-c(bad_spots)]

# sick.

print(length(blood.sents))

blood.title <- rep("bloodMeridian", 7552)
blood.sents.type <- rep("sentence", 7552)
blood.sents.counter<-seq(1, 7552)
blood.sents.id <- paste0("BLOOD_MERIDIAN_", "SENT_", blood.sents.counter)
blood.label <- rep("1", 7552)
print(length(blood.sents.id))

blood.sents.matrix <- cbind(blood.title, blood.sents.type, blood.sents.id, blood.sents, blood.label)
blood.sents.df <- as.data.frame(blood.sents.matrix, stringsAsFactors = FALSE)
colnames(blood.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", blood.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='bloodMeridian' LIMIT 2")
dbDisconnect(con)

# words.

blood.temp <- blood
blood.temp <- paste(blood.temp, collapse=" ")
blood.temp <-tolower(blood.temp)
# a better regex that is going to maintain contractions. important! 

blood.temp <- unlist(strsplit(blood.temp, "[^\\w']", perl=TRUE))
blood.not.blanks <- which(blood.temp != "")
blood.words <- blood.temp[blood.not.blanks]
print(length(blood.words))
blood.words<- blood.words[which(blood.words!="'")]
print(length(blood.words))

blood.title <- rep("bloodMeridian", 115200)
blood.words.type <- rep("word", 115200)
blood.words.counter <- seq(1, 115200)
blood.label <- rep("1", 115200)
blood.words.id <- paste0("BLOOD_MERIDIAN_", "WORD_", blood.words.counter)

blood.words.matrix <- cbind(blood.title, blood.words.type, blood.words.id, blood.words, blood.label)

blood.words.df <- as.data.frame(blood.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(blood.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", blood.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='bloodMeridian' LIMIT 10")
dbDisconnect(con)

# blood done.