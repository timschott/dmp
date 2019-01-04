setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

lock <- scan("rawTexts/detective/arthur-conan-doyle-a-study-in-scarlet.txt",what="character",sep="\n")
lock.start <- which(lock=="IN the year 1878 I took my degree of Doctor of Medicine of the University of London, and proceeded to Netley to go through the course prescribed for surgeons in the army. Having completed my studies there, I was duly attached to the Fifth Northumberland Fusiliers as Assistant Surgeon. The regiment was stationed in India at the time, and before I could join it, the second Afghan war had broken out. On landing at Bombay, I learned that my corps had advanced through the passes, and was already deep in the enemy’s country. I followed, however, with many other officers who were in the same situation as myself, and succeeded in reaching Candahar in safety, where I found my regiment, and at once entered upon my new duties.")
lock.fin <- which(lock=="“Never mind,” I answered, “I have all the facts in my journal, and the public shall know them. In the meantime you must make yourself contented by the consciousness of success, like the Roman miser—")
lock<- lock[lock.start: lock.fin]

print(length(lock))
lock[1] <- "In the year 1878 I took my degree of Doctor of Medicine of the University of London, and proceeded to Netley to go through the course prescribed for surgeons in the army. Having completed my studies there, I was duly attached to the Fifth Northumberland Fusiliers as Assistant Surgeon. The regiment was stationed in India at the time, and before I could join it, the second Afghan war had broken out. On landing at Bombay, I learned that my corps had advanced through the passes, and was already deep in the enemy’s country. I followed, however, with many other officers who were in the same situation as myself, and succeeded in reaching Candahar in safety, where I found my regiment, and at once entered upon my new duties."

spots <- grep('[A-Z]{2,}[^a-z]', lock)
lock[spots]
lock <- lock[-c(spots[-c(2,5:8, 10:11,13,15,17:18,21,23,25,
                         27,29:31,33,34,36)])]

lock.paragraphs <- as.data.frame(lock, stringsAsFactors=FALSE)
colnames(lock.paragraphs) <- c("paras")

lock.paragraphs<- lock.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

lock.paragraphs <- lock.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

lock.paragraphs <- lock.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

lock.paragraphs <- lock.paragraphs %>% 
  filter(paragraphs!="")

lock.paragraphs <- lock.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(lock.paragraphs)

lock.paragraphs <- lock.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

lock.paragraphs <- lock.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(lock.paragraphs$paragraphs))

lock.paragraphs <- lock.paragraphs %>% 
  filter(paragraphs!="")

lock.paragraphs <- lock.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(lock.paragraphs$paragraphs))

lock.title <- rep("aStudyInScarlet", 811)
lock.para.type <- rep("paragraph",811)
lock.para.counter<-seq(1, 811)
lock.para.id <- paste0("A_STUDY_IN_SCARLET_", "PARAGRAPH_", lock.para.counter)
lock.label <- rep("0", 811)
print(length(lock.para.id))

lock.para.matrix <- cbind(lock.title, lock.para.type, lock.para.id, lock.paragraphs, lock.label)
lock.para.df <- as.data.frame(lock.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(lock.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", lock.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='aStudyInScarlet' LIMIT 2")
dbDisconnect(con)

# sents. 
lock <- lock.paragraphs$paragraphs

first_bite <- lock[1:811]

lock.sents.first <- paste0(first_bite, collapse = "\n")
lock.sents.first <- unlist(tokenize_sentences(lock.sents.first))

lock.sents <- c(lock.sents.first)
lock.sents.df <- as.data.frame(lock.sents, stringsAsFactors = FALSE)

print(length(lock.sents.df$lock.sents))

bad_spots <-c(0)
for(i in seq(1:length(lock.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(lock.sents[i], nchar(lock.sents[i]), nchar(lock.sents[i]))
  test2 <- substr(lock.sents[i+1], 1, 1)
  test3 <- substr(lock.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      lock.sents[i] <- paste(lock.sents[i], lock.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
# lock.sents[bad_spots]
lock.sents <- lock.sents[-c(bad_spots)]

bad_spots <-c(0)
for(i in seq(1:length(lock.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(lock.sents[i], nchar(lock.sents[i])-1, nchar(lock.sents[i]))
  test2 <- substr(lock.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?”", "!”") && test2==tolower(test2)){
      lock.sents[i] <- paste(lock.sents[i], lock.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

lock.sents[bad_spots]
lock.sents <- lock.sents[-c(bad_spots)]
print(length(lock.sents))

print(length(lock.sents))
lock.sents <- lock.sents[lock.sents!=""]
lock.sents.df <- as.data.frame(lock.sents, stringsAsFactors = FALSE)

lock.title <- rep("aStudyInScarlet", 2616)
lock.sents.type <- rep("sentence", 2616)
lock.sents.counter<-seq(1, 2616)
lock.sents.id <- paste0("A_STUDY_IN_SCARLET_", "SENT_", lock.sents.counter)
lock.label <- rep("0", 2616)
print(length(lock.sents.id))

lock.sents.matrix <- cbind(lock.title, lock.sents.type, lock.sents.id, lock.sents, lock.label)
lock.sents.df <- as.data.frame(lock.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(lock.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", lock.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='aStudyInScarlet' LIMIT 2")
dbDisconnect(con)

lock.temp <- lock
lock.temp <- paste(lock.temp, collapse=" ")
lock.temp <-tolower(lock.temp)
# a better regex that is going to maintain contractions. important! 

lock.temp <- unlist(strsplit(lock.temp, "[^\\w']", perl=TRUE))
lock.not.blanks <- which(lock.temp != "")
lock.words <- lock.temp[lock.not.blanks]
print(length(lock.words))

lock.words<- lock.words[which(lock.words!="^”")]
lock.words<- lock.words[which(lock.words!="”")]
print(length(lock.words))


lock.words.df <- as.data.frame(lock.words, stringsAsFactors = FALSE)

lock.title <- rep("aStudyInScarlet", 43901)
lock.words.type <- rep("word", 43901)
lock.words.counter <- seq(1, 43901)
lock.words.id <- paste0("A_STUDY_IN_SCARLET_", "WORD_", lock.words.counter)
lock.label<- rep("0", 43901)
lock.words.matrix <- cbind(lock.title, lock.words.type, lock.words.id, lock.words, lock.label)

lock.words.df <- as.data.frame(lock.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(lock.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", lock.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='aStudyInScarlet' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
