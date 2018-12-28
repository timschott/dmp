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

# absalom absalom

ab <- scan("rawTexts/william-faulkner-absalom-absalom.txt",what="character",sep="\n")
stock <- c("Title", "Type", "ID", "Unit")

ab.start <-which(ab=="FROM a little after two o'clock until almost sundown of the long still hot weary dead September afternoon they sat in what Miss Coldfield still called the office because her father had called it that—a dim hot airless room with the blinds all closed and fastened for forty-three summers because when she was a girl someone had believed that light and moving air carried heat and that dark was always cooler, and which (as the sun shone fuller and fuller on that side of the house) became latticed with yellow slashes full of dust motes which Quentin thought of as being flecks of the dead old dried paint itself blown inward from the scaling blinds as wind might have blown them.")
ab.end <- which(ab=="I don't hate it! I don't hate it!")

ab <- ab[ab.start:ab.end]

ab[1] <- gsub("FROM", "From", ab[1])

# get rid of chap.
chaps <- grep("^—[0-9]—", ab)
ab <- ab[-chaps]

ab.paragraphs <- as.data.frame(ab, stringsAsFactors=FALSE)
colnames(ab.paragraphs) <- c("paras")
ab.paragraphs <- ab.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr.", "Mr", paras))
ab.paragraphs <- ab.paragraphs %>% 
  transmute(paras=  replace_abbreviation(paragraphs))
print(length(ab.paragraphs$paras))
ab.paragraphs <- ab.paragraphs %>% filter(paras!="")
print(length(ab.paragraphs$paras))
# cool.
ab.title <- rep("absalomAbsalom", 569)
ab.para.type <- rep("paragraph", 569)
ab.para.counter<-seq(1, 569)
ab.para.id <- paste0("ABSALOM_ABSALOM_", "PARAGRAPH_", ab.para.counter)
print(length(ab.para.id))

ab.para.matrix <- cbind(ab.title, ab.para.type, ab.para.id, ab.paragraphs)
ab.para.df <- as.data.frame(ab.para.matrix, stringsAsFactors = FALSE)
colnames(ab.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", ab.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='absalomAbsalom' LIMIT 2")
dbDisconnect(con)

# sents.
first_bite <- ab[1:569]

ab.sents.first <- paste0(first_bite, collapse = "\n")
ab.sents.first <- unlist(tokenize_sentences(ab.sents.first))

ab.sents <- c(ab.sents.first)
ab.sents.df <- as.data.frame(ab.sents, stringsAsFactors = FALSE)


bad_spots <-c(0)
for(i in seq(1:length(ab.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(ab.sents[i], nchar(ab.sents[i]), nchar(ab.sents[i]))
  test2 <- substr(ab.sents[i+1], 1, 1)
  test3 <- substr(ab.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      ab.sents[i] <- paste(ab.sents[i], ab.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
ab.sents[bad_spots]
ab.sents <- ab.sents[-c(bad_spots)]

# sick.

ab.sents.df <- as.data.frame(ab.sents, stringsAsFactors = FALSE)

# sew tg
print(length(ab.sents))
ab.title <- rep("absalomAbsalom", 2986)
ab.sents.type <- rep("sentence", 2986)
ab.sents.counter<-seq(1, 2986)
ab.sents.id <- paste0("ABSALOM_ABSALOM_", "SENT_", ab.sents.counter)
print(length(ab.sents.id))

ab.sents.matrix <- cbind(ab.title, ab.sents.type, ab.sents.id, ab.sents)
ab.sents.df <- as.data.frame(ab.sents.matrix, stringsAsFactors = FALSE)
colnames(ab.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", ab.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='absalomAbsalom' LIMIT 2")
dbDisconnect(con)

# okay words.

ab.temp <- ab
ab.temp <- paste(ab.temp, collapse=" ")
ab.temp <-tolower(ab.temp)
# a better regex that is going to maintain contractions. important! 

ab.temp <- unlist(strsplit(ab.temp, "[^\\w']", perl=TRUE))
ab.not.blanks <- which(ab.temp != "")
ab.words <- ab.temp[ab.not.blanks]
print(length(ab.words))
ab.words<- ab.words[which(ab.words!="'")]
print(length(ab.words))

# okay cool.
ab.title <- rep("absalomAbsalom", 132668)
ab.words.type <- rep("word", 132668)
ab.words.counter <- seq(1, 132668)
ab.words.id <- paste0("ABSALOM_ABSALOM_", "WORD_", ab.words.counter)

ab.words.matrix <- cbind(ab.title, ab.words.type, ab.words.id, ab.words)
grep("'", ab.words.df$ab.words)
ab.words.df <- as.data.frame(ab.words.matrix, stringsAsFactors = FALSE)

ab.words.df <- ab.words.df %>%
  mutate(word = gsub("'", "", perl=TRUE, ab.words)) %>%
  select(ab.title, ab.words.type, ab.words.id, word)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(ab.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", ab.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='absalomAbsalom' LIMIT 10")
dbDisconnect(con)

#abso donzo. 