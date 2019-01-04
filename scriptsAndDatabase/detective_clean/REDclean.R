setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

red <- scan("rawTexts/detective/raustin-freedman-the-red-thumb-mark.txt",what="character",sep="\n")
red.start <- which(red=="\"Conflagratam An° 1677. Fabricatam An° 1698. Richardo Powell Armiger Thesaurar.\" The words, set in four panels, which formed a frieze beneath the pediment of a fine brick portico, summarised the history of one of the tall houses at the upper end of King's Bench Walk and as I, somewhat absently, read over the inscription, my attention was divided between admiration of the exquisitely finished carved brickwork and the quiet dignity of the building, and an effort to reconstitute the dead and gone Richard Powell, and the stirring times in which he played his part.")
red.fin <- which(red=="She laid her hand in mine for a moment with a gentle pressure and then drew it away; and so we passed through into the cloisters.")
red <- red[red.start:red.fin]

spots <- grep('[A-Z]{2,}[^a-z]', red)
red[spots]
red <- red[-spots]

red.paragraphs <- as.data.frame(red, stringsAsFactors=FALSE)
colnames(red.paragraphs) <- c("paras")
red.paragraphs <- red.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

colnames(red.paragraphs) <- c("paras")

red.paragraphs<- red.paragraphs %>%
  transmute(paragraphs=gsub("\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

red.paragraphs <- red.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

red.paragraphs <- red.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

red.paragraphs <- red.paragraphs %>% 
  filter(paragraphs!="")

red.paragraphs <- red.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(red.paragraphs)

red.paragraphs <- red.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

red.paragraphs <- red.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(red.paragraphs$paragraphs))

red.paragraphs <- red.paragraphs %>% 
  filter(paragraphs!="")

red.paragraphs <- red.paragraphs %>% 
  filter(paragraphs!="  ")

print(length(red.paragraphs$paragraphs))

red.title <- rep("theRedThumbMark", 1747)
red.para.type <- rep("paragraph",1747)
red.para.counter<-seq(1, 1747)
red.para.id <- paste0("THE_RED_THUMB_MARK_", "PARAGRAPH_", red.para.counter)
red.label <- rep("0", 1747)
print(length(red.para.id))

red.para.matrix <- cbind(red.title, red.para.type, red.para.id, red.paragraphs, red.label)
red.para.df <- as.data.frame(red.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(red.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", red.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theRedThumbMark' LIMIT 2")
dbDisconnect(con)

# dbExecute(con, "DELETE FROM textTable WHERE Type='paragraph' OR Type= 'sentence' AND Title='theRedThumbMark'")

red <- red.paragraphs$paragraphs

first_bite <- red[1:1747]

red.sents.first <- paste0(first_bite, collapse = "\n")
red.sents.first <- unlist(tokenize_sentences(red.sents.first))

red.sents <- c(red.sents.first)
red.sents.df <- as.data.frame(red.sents, stringsAsFactors = FALSE)

print(length(red.sents.df$red.sents))


bad_spots <-c(0)
for(i in seq(1:length(red.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(red.sents[i], nchar(red.sents[i]), nchar(red.sents[i]))
  test2 <- substr(red.sents[i+1], 1, 1)
  test3 <- substr(red.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      red.sents[i] <- paste(red.sents[i], red.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
red.sents[bad_spots]
red.sents <- red.sents[-c(bad_spots)]

red.sents.df <- as.data.frame(red.sents, stringsAsFactors = FALSE)
red.sents <- red.sents[red.sents!=""]
print(length(red.sents))

bad_spots <-c(0)
for(i in seq(1:length(red.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(red.sents[i], nchar(red.sents[i])-1, nchar(red.sents[i]))
  test2 <- substr(red.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      red.sents[i] <- paste(red.sents[i], red.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
red.sents[bad_spots]
red.sents <- red.sents[-c(bad_spots)]
print(length(red.sents))

red.title <- rep("theRedThumbMark", 3474)
red.sents.type <- rep("sentence", 3474)
red.sents.counter<-seq(1, 3474)
red.sents.id <- paste0("THE_RED_THUMB_MARK_", "SENT_", red.sents.counter)
red.label <- rep("0", 3474)
print(length(red.sents.id))

red.sents.matrix <- cbind(red.title, red.sents.type, red.sents.id, red.sents, red.label)
red.sents.df <- as.data.frame(red.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(red.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", red.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theRedThumbMark' LIMIT 2")
dbDisconnect(con)

# words.
red.temp <- red
red.temp <- paste(red.temp, collapse=" ")
red.temp <-tolower(red.temp)
# a better regex that is going to maintain contractions. important! 

red.temp <- unlist(strsplit(red.temp, "[^\\w']", perl=TRUE))
red.not.blanks <- which(red.temp != "")
red.words <- red.temp[red.not.blanks]
print(length(red.words))

red.words<- red.words[which(red.words!="^'")]
red.words<- red.words[which(red.words!="'")]
print(length(red.words))


red.title <- rep("theRedThumbMark", 70836)
red.words.type <- rep("word", 70836)
red.words.counter <- seq(1, 70836)
red.words.id <- paste0("THE_RED_THUMB_MARK_", "WORD_", red.words.counter)
red.label<- rep("0", 74609)
red.words.matrix <- cbind(red.title, red.words.type, red.words.id, red.words, red.label)

red.words.df <- as.data.frame(red.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(red.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", red.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theRedThumbMark' LIMIT 10")
dbDisconnect(con)

# red thumb, done.



