setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

keep <- scan("rawTexts/detective/js-fletcher-the-scarhaven-keep.txt",what="character",sep="\n")
keep.start <- which(keep=="Jerramy, thirty years' stage-door keeper at the Theatre Royal, Norcaster, had come to regard each successive Monday morning as a time for the renewal of old acquaintance. For at any rate forty-six weeks of the fifty-two, theatrical companies came and went at Norcaster with unfailing regularity. The company which presented itself for patronage in the first week of April in one year was almost certain to present itself again in the corresponding week of the next year. Sometimes new faces came with it, but as a rule the same old favourites showed themselves for a good many years in succession. And every actor and actress who came to Norcaster knew Jerramy. He was the first official person encountered on entering upon the business of the week. He it was who handed out the little bundles of letters and papers, who exchanged the first greetings, of whom one could make useful inquiries, who always knew exactly what advice to give about lodgings and landladies. From noon onwards of Mondays, when the newcomers began to arrive at the theatre for the customary one o'clock call for rehearsal, Jerramy was invariably employed in hearing that he didn't look a day older, and was as blooming as ever, and sure to last another thirty years, and his reception always culminated in a hearty handshake and genial greeting from the great man of the company, who, of course, after the fashion of magnates, always turned up at the end of the irregular procession, and was not seldom late for the fixture which he himself had made.")
keep.fin <- which(keep=="\"Chatfield!\" he said musingly. \"Chatfield! sublimely ungrateful that he isn't in Dartmoor.\"")
keep <- keep[keep.start:keep.fin]
print(length(keep))
spots <- grep('[A-Z]{2,}[^a-z]', keep)
keep[spots]
# which(spots %in% test)
keep <- keep[-c(spots[-c(13, 18, 35, 38, 39, 40, 41, 66, 67)])]
print(length(keep))

keep.paragraphs <- as.data.frame(keep, stringsAsFactors=FALSE)
colnames(keep.paragraphs) <- c("paras")
keep.paragraphs <- keep.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

colnames(keep.paragraphs) <- c("paras")

keep.paragraphs<- keep.paragraphs %>%
  transmute(paragraphs=gsub("\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

keep.paragraphs <- keep.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

keep.paragraphs <- keep.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

keep.paragraphs <- keep.paragraphs %>% 
  filter(paragraphs!="")

keep.paragraphs <- keep.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(keep.paragraphs)

keep.paragraphs <- keep.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

keep.paragraphs <- keep.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(keep.paragraphs$paragraphs))

keep.paragraphs <- keep.paragraphs %>% 
  filter(paragraphs!="")

keep.paragraphs <- keep.paragraphs %>% 
  filter(paragraphs!="  ")

print(length(keep.paragraphs$paragraphs))

keep.title <- rep("theScarhavenKeep", 1644)
keep.para.type <- rep("paragraph",1644)
keep.para.counter<-seq(1, 1644)
keep.para.id <- paste0("THE_SCARHAVEN_KEEP_", "PARAGRAPH_", keep.para.counter)
keep.label <- rep("0", 1644)
print(length(keep.para.id))

keep.para.matrix <- cbind(keep.title, keep.para.type, keep.para.id, keep.paragraphs, keep.label)
keep.para.df <- as.data.frame(keep.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(keep.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", keep.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theScarhavenKeep' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Type='paragraph' OR Type= 'sentence' AND Title='theScarhavenKeep'")

dbDisconnect(con)

# sents. 
keep <- keep.paragraphs$paragraphs

first_bite <- keep[1:1644]

keep.sents.first <- paste0(first_bite, collapse = "\n")
keep.sents.first <- unlist(tokenize_sentences(keep.sents.first))

keep.sents <- c(keep.sents.first)
keep.sents.df <- as.data.frame(keep.sents, stringsAsFactors = FALSE)

print(length(keep.sents.df$keep.sents))

bad_spots <-c(0)
for(i in seq(1:length(keep.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(keep.sents[i], nchar(keep.sents[i]), nchar(keep.sents[i]))
  test2 <- substr(keep.sents[i+1], 1, 1)
  test3 <- substr(keep.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      keep.sents[i] <- paste(keep.sents[i], keep.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
# keep.sents[bad_spots]
keep.sents <- keep.sents[-c(bad_spots)]

print(length(keep.sents))

bad_spots <-c(0)
for(i in seq(1:length(keep.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(keep.sents[i], nchar(keep.sents[i])-1, nchar(keep.sents[i]))
  test2 <- substr(keep.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      keep.sents[i] <- paste(keep.sents[i], keep.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
keep.sents[bad_spots]
keep.sents <- keep.sents[-c(bad_spots)]
keep.sents.df <- as.data.frame(keep.sents, stringsAsFactors = FALSE)
keep.sents <- keep.sents[keep.sents!=""]
print(length(keep.sents))

keep.title <- rep("theScarhavenKeep", 4831)
keep.sents.type <- rep("sentence", 4831)
keep.sents.counter<-seq(1, 4831)
keep.sents.id <- paste0("THE_SCARHAVEN_KEEP_", "SENT_", keep.sents.counter)
keep.label <- rep("0", 4831)
print(length(keep.sents.id))

keep.sents.matrix <- cbind(keep.title, keep.sents.type, keep.sents.id, keep.sents, keep.label)
keep.sents.df <- as.data.frame(keep.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(keep.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", keep.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theScarhavenKeep' LIMIT 2")
dbDisconnect(con)

# words. 

keep.temp <- keep
keep.temp <- paste(keep.temp, collapse=" ")
keep.temp <-tolower(keep.temp)
# a better regex that is going to maintain contractions. important! 

keep.temp <- unlist(strsplit(keep.temp, "[^\\w']", perl=TRUE))
keep.not.blanks <- which(keep.temp != "")
keep.words <- keep.temp[keep.not.blanks]
print(length(keep.words))

keep.words<- keep.words[which(keep.words!="^'")]
keep.words<- keep.words[which(keep.words!="'")]
print(length(keep.words))
keep.words[65275] <- "we'd"
keep.words[65276] <- ""
keep.words<- keep.words[which(keep.words!="")]

keep.title <- rep("theScarhavenKeep", 74609)
keep.words.type <- rep("word", 74609)
keep.words.counter <- seq(1, 74609)
keep.words.id <- paste0("THE_SCARHAVEN_KEEP_", "WORD_", keep.words.counter)
keep.label<- rep("0", 74609)
keep.words.matrix <- cbind(keep.title, keep.words.type, keep.words.id, keep.words, keep.label)

keep.words.df <- as.data.frame(keep.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(keep.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", keep.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theScarhavenKeep' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)

# keep done.