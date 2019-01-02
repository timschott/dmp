setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

ash <- scan("rawTexts/detective/mrs-charles-bryce-the-ashiel-mystery.txt",what="character",sep="\n")
ash.start <- which(ash=="When Sir Arthur Byrne fell ill, after three summers at his post in the little consulate that overlooked the lonely waters of the Black Sea, he applied for sick leave. Having obtained it, he hurried home to scatter guineas in Harley Street; for he felt all the uneasy doubts as to his future which a strong man who has never in his life known what it is to have a headache is apt to experience at the first symptom that all is not well. Outwardly, he pretended to make light of the matter.")
ash.fin <- which(ash=="THE END")
ash <- ash[ash.start:ash.fin-1]

ash[grep("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", ash)]
ash.paragraphs <- as.data.frame(ash, stringsAsFactors=FALSE)
colnames(ash.paragraphs) <- c("paras")

ash.paragraphs<- ash.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

ash.paragraphs <- ash.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

ash.paragraphs <- ash.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

ash.paragraphs <- ash.paragraphs %>% 
  transmute(paras=  gsub("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs) )

ash.paragraphs <- ash.paragraphs %>% 
  filter(paras!="")

ash.paragraphs <- ash.paragraphs %>% 
  filter(paras!="  ")
colnames(ash.paragraphs)

ash.paragraphs <- ash.paragraphs %>% 
  transmute(paragraphs = replace_abbreviation(paras))

ash.paragraphs <- ash.paragraphs %>% 
  transmute(paras=  gsub("MR\\.", "Mr", paragraphs))

print(length(ash.paragraphs$paras))

ash.paragraphs <- ash.paragraphs %>% 
  filter(paras!="")

ash.paragraphs <- ash.paragraphs %>% 
  filter(paras!="  ")
print(length(ash.paragraphs$paras))

ash.title <- rep("theAshielMystery", 1620)
ash.para.type <- rep("paragraph",1620)
ash.para.counter<-seq(1, 1620)
ash.para.id <- paste0("THE_ASHIEL_MYSTERY_", "PARAGRAPH_", ash.para.counter)
ash.label <- rep("0", 1620)
print(length(ash.para.id))

ash.para.matrix <- cbind(ash.title, ash.para.type, ash.para.id, ash.paragraphs, ash.label)
ash.para.df <- as.data.frame(ash.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(ash.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", ash.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theAshielMystery' LIMIT 2")
dbDisconnect(con)

# sents
ash <- ash.paragraphs$paras

first_bite <- ash[1:1620]

ash.sents.first <- paste0(first_bite, collapse = "\n")
ash.sents.first <- unlist(tokenize_sentences(ash.sents.first))

ash.sents <- c(ash.sents.first)
ash.sents.df <- as.data.frame(ash.sents, stringsAsFactors = FALSE)

print(length(ash.sents.df$ash.sents))

bad_spots <-c(0)
for(i in seq(1:length(ash.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(ash.sents[i], nchar(ash.sents[i]), nchar(ash.sents[i]))
  test2 <- substr(ash.sents[i+1], 1, 1)
  test3 <- substr(ash.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      ash.sents[i] <- paste(ash.sents[i], ash.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
ash.sents[bad_spots]
ash.sents <- ash.sents[-c(bad_spots)]
ash.sents <- ash.sents[ash.sents!=""]
print(length(ash.sents))

ash.title <- rep("theAshielMystery", 4511)
ash.sents.type <- rep("sentence", 4511)
ash.sents.counter<-seq(1, 4511)
ash.sents.id <- paste0("THE_ASHIEL_MYSTERY_", "SENT_", ash.sents.counter)
ash.label <- rep("0", 4511)
print(length(ash.sents.id))

ash.sents.matrix <- cbind(ash.title, ash.sents.type, ash.sents.id, ash.sents, ash.label)
ash.sents.df <- as.data.frame(ash.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(ash.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", ash.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theAshielMystery' LIMIT 2")
dbDisconnect(con)

# words
ash.temp <- ash
ash.temp <- paste(ash.temp, collapse=" ")
ash.temp <-tolower(ash.temp)
# a better regex that is going to maintain contractions. important! 

ash.temp <- unlist(strsplit(ash.temp, "[^\\w']", perl=TRUE))
ash.not.blanks <- which(ash.temp != "")
ash.words <- ash.temp[ash.not.blanks]
print(length(ash.words))
ash.words<- ash.words[which(ash.words!="^'")]
ash.words<- ash.words[which(ash.words!="'")]
print(length(ash.words))

ash.words.df <- as.data.frame(ash.words, stringsAsFactors = FALSE)
ash.words<- ash.words[which(ash.words!="''")]
print(length(ash.words))

ash.title <- rep("theAshielMystery", 87401)
ash.words.type <- rep("word", 87401)
ash.words.counter <- seq(1, 87401)
ash.words.id <- paste0("THE_ASHIEL_MYSTERY_", "WORD_", ash.words.counter)
ash.label<- rep("0", 87401)
ash.words.matrix <- cbind(ash.title, ash.words.type, ash.words.id, ash.words, ash.label)

ash.words.df <- as.data.frame(ash.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(ash.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", ash.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theAshielMystery' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
