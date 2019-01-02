setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

# just do the paragraphs first then distill down. 

white.paragraphs <- read.csv("Python_Scripts/checkCorpus/white_paras.csv", stringsAsFactors = FALSE)
white.paragraphs$X0[20]
# gotta sub \n for a space
white.paragraphs <- white.paragraphs[-c(1:21,3145:3149),]
colnames(white.paragraphs) <- c("arb", "paragraphs")
white.paragraphs$paragraphs[86] <- "Her glance of respect was reserved for the black-and-white satin\ntea-frock, which gave the impression that Simone had been imported\nstraight from the London Restaurant, the dansant, together with\nthe music. She also followed the conventions of fwhiteion in such details\nas artificial lips and eyebrows superimposed on the original structure.\nHer glossy black hair was sleeked back into curls, resting on the nape\nof her neck, and her nails were polished vermilion."
white.paragraphs$paras[grep("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", white.paragraphs$paras)]
xe9

white.paragraphs <- white.paragraphs %>% 
  transmute(paras=  gsub("\n", " ", paragraphs) )

white.paragraphs<- white.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

white.paragraphs <- white.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

white.paragraphs <- white.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

white.paragraphs <- white.paragraphs %>% 
  transmute(paras=  gsub("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs) )

white.paragraphs <- white.paragraphs %>% 
  filter(paras!="")

white.paragraphs <- white.paragraphs %>% 
  filter(paras!="  ")
colnames(white.paragraphs)

white.paragraphs <- white.paragraphs %>% 
  transmute(paragraphs = replace_abbreviation(paras))

white.paragraphs <- white.paragraphs %>% 
  transmute(paras=  gsub("MR\\.", "Mr", paragraphs))

print(length(white.paragraphs$paras))

white.paragraphs <- white.paragraphs %>% 
  filter(paras!="")

white.paragraphs <- white.paragraphs %>% 
  filter(paras!="  ")
white.paragraphs <- white.paragraphs %>% 
  transmute(paragraphs=  gsub("_", "", paras))

print(length(white.paragraphs$paragraphs))
white.paragraphs$paragraphs[2000:2005]
white.title <- rep("theSpiralStaircase", 3123)
white.para.type <- rep("paragraph",3123)
white.para.counter<-seq(1, 3123)
white.para.id <- paste0("THE_SPIRAL_STAIRCASE_", "PARAGRAPH_", white.para.counter)
white.label <- rep("0", 3123)
print(length(white.para.id))

white.para.matrix <- cbind(white.title, white.para.type, white.para.id, white.paragraphs, white.label)
white.para.df <- as.data.frame(white.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(white.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", white.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theSpiralStaircase' LIMIT 2")
dbDisconnect(con)

# sents.
white <- white.paragraphs$paragraphs

first_bite <- white[1:3123]

white.sents.first <- paste0(first_bite, collapse = "\n")
white.sents.first <- unlist(tokenize_sentences(white.sents.first))

white.sents <- c(white.sents.first)
white.sents.df <- as.data.frame(white.sents, stringsAsFactors = FALSE)

print(length(white.sents.df$white.sents))


bad_spots <-c(0)
for(i in seq(1:length(white.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(white.sents[i], nchar(white.sents[i]), nchar(white.sents[i]))
  test2 <- substr(white.sents[i+1], 1, 1)
  test3 <- substr(white.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      white.sents[i] <- paste(white.sents[i], white.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
white.sents[bad_spots]
white.sents <- white.sents[-c(bad_spots)]
white.sents <- white.sents[white.sents!=""]
print(length(white.sents))

white.title <- rep("theSpiralStaircase", 6166)
white.sents.type <- rep("sentence", 6166)
white.sents.counter<-seq(1, 6166)
white.sents.id <- paste0("THE_SPIRAL_STAIRCASE_", "SENT_", white.sents.counter)
white.label <- rep("0", 6166)
print(length(white.sents.id))

white.sents.matrix <- cbind(white.title, white.sents.type, white.sents.id, white.sents, white.label)
white.sents.df <- as.data.frame(white.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(white.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", white.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theSpiralStaircase' LIMIT 2")
dbDisconnect(con)
# words. 

white.temp <- white
white.temp <- paste(white.temp, collapse=" ")
white.temp <-tolower(white.temp)
# a better regex that is going to maintain contractions. important! 

white.temp <- unlist(strsplit(white.temp, "[^\\w']", perl=TRUE))
white.not.blanks <- which(white.temp != "")
white.words <- white.temp[white.not.blanks]
print(length(white.words))
white.words<- white.words[which(white.words!="^'")]
white.words<- white.words[which(white.words!="'")]
print(length(white.words))

white.words.df <- as.data.frame(white.words, stringsAsFactors = FALSE)
white.words<- white.words[which(white.words!="''")]
print(length(white.words))

white.title <- rep("theSpiralStaircase", 70956)
white.words.type <- rep("word", 70956)
white.words.counter <- seq(1, 70956)
white.words.id <- paste0("THE_SPIRAL_STAIRCASE_", "WORD_", white.words.counter)
white.label<- rep("0", 70956)
white.words.matrix <- cbind(white.title, white.words.type, white.words.id, white.words, white.label)

white.words.df <- as.data.frame(white.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(white.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", white.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theSpiralStaircase' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
