setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

# just do the paragraphs first then distill down. 

blue.paragraphs <- read.csv("Python_Scripts/checkCorpus/blue_paras.csv", stringsAsFactors = FALSE)
blue.paragraphs$X0[20]

blue.paragraphs <- blue.paragraphs[-c(1:54,1183:1186),]
colnames(blue.paragraphs) <- c("arb", "paragraphs")


blue.paragraphs <- blue.paragraphs %>% 
  transmute(paras=  gsub("\n", " ", paragraphs) )

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "'", paras))

colnames(blue.paragraphs) <- c("paras")

blue.paragraphs<- blue.paragraphs %>%
  transmute(paragraphs=gsub("\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paras=  gsub("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs) )

blue.paragraphs <- blue.paragraphs %>% 
  filter(paras!="")

blue.paragraphs <- blue.paragraphs %>% 
  filter(paras!="  ")
colnames(blue.paragraphs)

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paragraphs = replace_abbreviation(paras))

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paras=  gsub("MR\\.", "Mr", paragraphs))

print(length(blue.paragraphs$paras))

blue.paragraphs <- blue.paragraphs %>% 
  filter(paras!="")

blue.paragraphs <- blue.paragraphs %>% 
  filter(paras!="  ")
blue.paragraphs <- blue.paragraphs %>% 
  transmute(paragraphs=  gsub("_", "", paras))
# Remove 
spots <- grep("^--", blue.paragraphs$paragraphs)
blue.paragraphs <- as.data.frame(blue.paragraphs[-c(spots),], stringsAsFactors = FALSE)
colnames(blue.paragraphs) <- c("paragraphs")
blue.paragraphs$paragraphs[555:568]

print(length(blue.paragraphs$paragraphs))

blue.title <- rep("theLadyInBlue", 1091)
blue.para.type <- rep("paragraph",1091)
blue.para.counter<-seq(1, 1091)
blue.para.id <- paste0("THE_LADY_IN_BLUE_", "PARAGRAPH_", blue.para.counter)
blue.label <- rep("0", 1091)
print(length(blue.para.id))

blue.para.matrix <- cbind(blue.title, blue.para.type, blue.para.id, blue.paragraphs, blue.label)
blue.para.df <- as.data.frame(blue.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(blue.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", blue.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theLadyInBlue' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Type='paragraph' OR Type= 'sentence' AND Title='theLadyInblue'")

dbDisconnect(con)

# blue sents. 
blue <- blue.para.df$Unit
first_bite <- blue[1:1091]
first_bite <- gsub("No\\.", "No", perl=TRUE, first_bite)

blue.sents.first <- paste0(first_bite, collapse = "\n")
blue.sents.first <- unlist(tokenize_sentences(blue.sents.first))

blue.sents <- c(blue.sents.first)
blue.sents.df <- as.data.frame(blue.sents, stringsAsFactors = FALSE)

print(length(blue.sents.df$blue.sents))

bad_spots <-c(0)
for(i in seq(1:length(blue.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(blue.sents[i], nchar(blue.sents[i]), nchar(blue.sents[i]))
  test2 <- substr(blue.sents[i+1], 1, 1)
  test3 <- substr(blue.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      blue.sents[i] <- paste(blue.sents[i], blue.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
blue.sents[bad_spots]
blue.sents <- blue.sents[-c(bad_spots)]
blue.sents <- blue.sents[blue.sents!=""]
print(length(blue.sents))
bad_spots <-c(0)

for(i in seq(1:length(blue.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(blue.sents[i], nchar(blue.sents[i])-1, nchar(blue.sents[i]))
  test2 <- substr(blue.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      blue.sents[i] <- paste(blue.sents[i], blue.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
blue.sents[bad_spots]
blue.sents <- blue.sents[-c(bad_spots)]
print(length(blue.sents))

blue.title <- rep("theLadyInBlue", 4659)
blue.sents.type <- rep("sentence", 4659)
blue.sents.counter<-seq(1, 4659)
blue.sents.id <- paste0("THE_LADY_IN_BLUE", "SENT_", blue.sents.counter)
blue.label <- rep("0", 4659)
print(length(blue.sents.id))

blue.sents.matrix <- cbind(blue.title, blue.sents.type, blue.sents.id, blue.sents, blue.label)
blue.sents.df <- as.data.frame(blue.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(blue.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", blue.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theLadyInBlue' LIMIT 2")
dbDisconnect(con)
# words.
blue.temp <- blue
blue.temp <- paste(blue.temp, collapse=" ")
blue.temp <-tolower(blue.temp)
# a better regex that is going to maintain contractions. important! 

blue.temp <- unlist(strsplit(blue.temp, "[^\\w']", perl=TRUE))
blue.not.blanks <- which(blue.temp != "")
blue.words <- blue.temp[blue.not.blanks]
print(length(blue.words))
blue.words<- blue.words[which(blue.words!="^'")]
blue.words<- blue.words[which(blue.words!="'")]
blue.words<- blue.words[which(blue.words!="''")]
print(length(blue.words))
blue.words.df <- as.data.frame(blue.words, stringsAsFactors = FALSE)

blue.title <- rep("theLadyInblue", 78625)
blue.words.type <- rep("word", 78625)
blue.words.counter <- seq(1, 78625)
blue.words.id <- paste0("THE_LADY_IN_blue_", "WORD_", blue.words.counter)
blue.label<- rep("0", 78625)
blue.words.matrix <- cbind(blue.title, blue.words.type, blue.words.id, blue.words, blue.label)

blue.words.df <- as.data.frame(blue.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(blue.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", blue.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theLadyInblue' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='1'")
dbDisconnect(con)

