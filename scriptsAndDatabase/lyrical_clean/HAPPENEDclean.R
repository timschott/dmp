setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
# library(rJava)
# library("openNLPdata")

## something happened
stock <- c("Title", "Type", "ID", "Unit", "Label")

hell <- scan("rawTexts/lyrical/joseph-heller-something-happened.txt",what="character", sep="\n")
hell.start <- which(hell=="I get the willies when I see closed doors. Even at work, where I am doing so well now, the sight of a closed door is sometimes enough to make me dread that something horrible is happening behind it, something that is going to affect me adversely; if I am tired and dejected from a night of lies or booze or sex or just plain nerves and insomnia, I can almost smell the disaster mounting invisibly and flooding out toward me through the frosted glass panes. My hands may perspire, and my voice may come out strange. I wonder why.")
hell.end<- which(hell=="Everyone seems pleased with the way I’ve taken command.")
hell <- hell[hell.start:hell.end]
# get rid of chapters; which are simple words in this book.
grep("Nobody knows what I’ve done", hell)
hell <- hell[-29]
hell <- hell[-408]
hell <- hell[-876]
hell <- hell[-1665]
hell <- hell[-3025]
hell <- hell[-4391]
hell <- hell[-4909]
hell <- hell[-5062]

hell.paragraphs <- as.data.frame(hell, stringsAsFactors=FALSE)
colnames(hell.paragraphs) <- c("paras")
hell.paragraphs <- hell.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))
hell.paragraphs <- hell.paragraphs %>% 
  transmute(paras=  replace_abbreviation(paragraphs))
print(length(hell.paragraphs$paras))
hell.paragraphs <- hell.paragraphs %>% filter(paras!="")
print(length(hell.paragraphs$paras))

# alright cool. 

hell.title <- rep("somethingHappened", 5124)
hell.para.type <- rep("paragraph", 5124)
hell.para.counter<-seq(1, 5124)
hell.label <- rep("1", 5124)
hell.para.id <- paste0("SOMETHING_HAPPENED_", "PARAGRAPH_", hell.para.counter)
print(length(hell.para.id))

hell.para.matrix <- cbind(hell.title, hell.para.type, hell.para.id, hell.paragraphs, hell.label)
hell.para.df <- as.data.frame(hell.para.matrix, stringsAsFactors = FALSE)
colnames(hell.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", hell.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='somethingHappened' LIMIT 2")
dbDisconnect(con)

# cool.

hell <- hell.paragraphs$paras
print(length(hell))
hell[1]

first_bite <- hell[1:2499]
second_bite<- hell[2500:5124]

hell.sents.first <- paste0(first_bite, collapse = "\n")
hell.sents.first <- unlist(tokenize_sentences(hell.sents.first))

hell.sents.second <- paste0(second_bite, collapse = "\n")
hell.sents.second <- unlist(tokenize_sentences(hell.sents.second))

hell.sents <- c(hell.sents.first, hell.sents.second)
hell.sents.df <- as.data.frame(hell.sents, stringsAsFactors = FALSE)
# quote loop. 

bad_spots <-c(0)
for(i in seq(1:length(hell.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(hell.sents[i], nchar(hell.sents[i])-1, nchar(hell.sents[i]))
  test2 <- substr(hell.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?”", "!”") && test2==tolower(test2)){
      hell.sents[i] <- paste(hell.sents[i], hell.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]

hell.sents[bad_spots]
hell.sents <- hell.sents[-c(bad_spots)]
print(length(hell.sents))

# no quote. 
bad_spots <-c(0)
for(i in seq(1:length(hell.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(hell.sents[i], nchar(hell.sents[i]), nchar(hell.sents[i]))
  test2 <- substr(hell.sents[i+1], 1, 1)
  test3 <- substr(hell.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      hell.sents[i] <- paste(hell.sents[i], hell.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
hell.sents[bad_spots]
hell.sents <- hell.sents[-c(bad_spots)]
print(length(hell.sents))
hell.sents[7592] <-paste(hell.sents[7592], hell.sents[7593])
hell.sents[7593] <- ""
hell.sents[8706] <- paste(hell.sents[8706], hell.sents[8707])
hell.sents[8707] <- ""
hell.sents[9150:9152]
hell.sents[9151] <- paste(hell.sents[9151], hell.sents[9152])
hell.sents[9152] <- ""
hell.sents <- hell.sents[hell.sents!=""]
print(length(hell.sents))
hell.sents.df <- as.data.frame(hell.sents, stringsAsFactors = FALSE)
           # lots of sents. 
hell.title <- rep("somethingHappened", 14994)
hell.sents.type <- rep("sentence", 14994)
hell.sents.counter<-seq(1, 14994)
hell.label <- rep("1", 14994)
hell.sents.id <- paste0("SOMETHING_HAPPENED_", "SENT_", hell.sents.counter)
print(length(hell.sents.id))

hell.sents.matrix <- cbind(hell.title, hell.sents.type, hell.sents.id, hell.sents, hell.label)
hell.sents.df <- as.data.frame(hell.sents.matrix, stringsAsFactors = FALSE)
colnames(hell.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", hell.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='somethingHappened' LIMIT 2")
dbDisconnect(con)

# once more, with feeling.

# words.

hell.temp <- hell
hell.temp <- paste(hell.temp, collapse=" ")
hell.temp <-tolower(hell.temp)
# a better regex that is going to maintain contractions. important! 

hell.temp <- unlist(strsplit(hell.temp, "[^\\w’]", perl=TRUE))
hell.not.blanks <- which(hell.temp != "")
hell.words <- hell.temp[hell.not.blanks]
print(length(hell.words))

hell.words<- hell.words[which(hell.words!="’")]
print(length(hell.words))
hell.words[1:100]


hell.title <- rep("somethingHappened", 190235)
hell.words.type <- rep("word", 190235)
hell.words.counter <- seq(1, 190235)
hell.label <- rep("1", 190235)
hell.words.id <- paste0("SOMETHING_HAPPENED_", "WORD_", hell.words.counter)

hell.words.matrix <- cbind(hell.title, hell.words.type, hell.words.id, hell.words, hell.label)

hell.words.df <- as.data.frame(hell.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(hell.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
dbWriteTable(con, "textTable", hell.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='somethingHappened' LIMIT 10")
dbDisconnect(con)

# hell done. 
