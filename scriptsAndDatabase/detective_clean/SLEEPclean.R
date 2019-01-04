setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)


sleep <- scan("rawTexts/detective/raymond-chandler-the-big-sleep.txt",what="character",sep="\n")
sleep.start <- which(sleep=="IT WAS ABOUT ELEVEN O’Csleep in the morning, mid October, with the sun not shining and a look of hard wet rain in the clearness of the foothills. I was wearing my powder-blue suit, with dark blue shirt, tie and display handkerchief, black brogues, black wool socks with dark blue csleeps on them. I was neat, clean, shaved and sober, and I didn’t care who knew it. I was everything the well-dressed private detective ought to be. I was calling on four million dollars.")
sleep.fin <- which(sleep=="On the way downtown I stopped at a bar and had a couple of double Scotches. They didn’t do me any good. All they did was make me think of Silver-Wig, and I never saw her again.")
sleep<- sleep[2: sleep.fin]
spots <- grep("^[0-9]+", sleep)
# spots
sleep <- sleep[-c(spots)]

sleep.paragraphs <- as.data.frame(sleep, stringsAsFactors=FALSE)
colnames(sleep.paragraphs) <- c("paras")
sleep.paragraphs<- sleep.paragraphs %>%
  transmute(paragraphs=gsub("\"", "'", perl=TRUE, paras))
colnames(sleep.paragraphs) <- c("paras")

sleep.paragraphs<- sleep.paragraphs %>%
  transmute(paragraphs=gsub("\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

sleep.paragraphs <- sleep.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

sleep.paragraphs <- sleep.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

sleep.paragraphs <- sleep.paragraphs %>% 
  filter(paragraphs!="")

sleep.paragraphs <- sleep.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(sleep.paragraphs)

sleep.paragraphs <- sleep.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

sleep.paragraphs <- sleep.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(sleep.paragraphs$paragraphs))

sleep.paragraphs <- sleep.paragraphs %>% 
  filter(paragraphs!="")

sleep.paragraphs <- sleep.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(sleep.paragraphs$paragraphs))

sleep.title <- rep("theBigSleep", 1979)
sleep.para.type <- rep("paragraph",1979)
sleep.para.counter<-seq(1, 1979)
sleep.para.id <- paste0("THE_BIG_SLEEP_", "PARAGRAPH_", sleep.para.counter)
sleep.label <- rep(0,1979)
print(length(sleep.para.id))

sleep.para.matrix <- cbind(sleep.title, sleep.para.type, sleep.para.id, sleep.paragraphs, sleep.label)
sleep.para.df <- as.data.frame(sleep.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(sleep.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", sleep.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theBigSleep' LIMIT 2")
dbDisconnect(con)

# sents. 

sleep <- sleep.paragraphs$paragraphs

first_bite <- sleep[1:1979]

sleep.sents.first <- paste0(first_bite, collapse = "\n")
sleep.sents.first <- unlist(tokenize_sentences(sleep.sents.first))

sleep.sents <- c(sleep.sents.first)
sleep.sents.df <- as.data.frame(sleep.sents, stringsAsFactors = FALSE)

print(length(sleep.sents.df$sleep.sents))

bad_spots <-c(0)
for(i in seq(1:length(sleep.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(sleep.sents[i], nchar(sleep.sents[i])-1, nchar(sleep.sents[i]))
  test2 <- substr(sleep.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      sleep.sents[i] <- paste(sleep.sents[i], sleep.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

sleep.sents[bad_spots]
sleep.sents <- sleep.sents[-c(bad_spots)]
print(length(sleep.sents))


print(length(sleep.sents))
sleep.sents <- sleep.sents[sleep.sents!=""]
sleep.sents.df <- as.data.frame(sleep.sents, stringsAsFactors = FALSE)

sleep.title <- rep("theBigSleep", 6516)
sleep.sents.type <- rep("sentence", 6516)
sleep.sents.counter<-seq(1, 6516)
sleep.sents.id <- paste0("THE_BIG_SLEEP_", "SENT_", sleep.sents.counter)
sleep.label <- rep("0", 6516)
print(length(sleep.sents.id))

sleep.sents.matrix <- cbind(sleep.title, sleep.sents.type, sleep.sents.id, sleep.sents, sleep.label)
sleep.sents.df <- as.data.frame(sleep.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(sleep.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", sleep.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type='paragraph' AND Title='theBigSleep' LIMIT 2")
dbDisconnect(con)


sleep.temp <- sleep
sleep.temp <- paste(sleep.temp, collapse=" ")
sleep.temp <-tolower(sleep.temp)
# a better regex that is going to maintain contractions. important! 

sleep.temp <- unlist(strsplit(sleep.temp, "[^\\w’]", perl=TRUE))
sleep.not.blanks <- which(sleep.temp != "")
sleep.words <- sleep.temp[sleep.not.blanks]
print(length(sleep.words))

sleep.words<- sleep.words[which(sleep.words!="^’")]
sleep.words<- sleep.words[which(sleep.words!="’")]
print(length(sleep.words))


sleep.words.df <- as.data.frame(sleep.words, stringsAsFactors = FALSE)

sleep.title <- rep("theBigSleep", 66911)
sleep.words.type <- rep("word", 66911)
sleep.words.counter <- seq(1, 66911)
sleep.words.id <- paste0("THE_BIG_SLEEP_", "WORD_", sleep.words.counter)
sleep.label<- rep("0", 66911)
sleep.words.matrix <- cbind(sleep.title, sleep.words.type, sleep.words.id, sleep.words, sleep.label)

sleep.words.df <- as.data.frame(sleep.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(sleep.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", sleep.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theBigSleep' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
