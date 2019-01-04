setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
rm(list=ls())
green <- scan("rawTexts/detective/anna-katharine-green-the-leavenworth-case.txt",what="character",sep="\n")
green.start <- which(green=="I had been a junior partner in the firm of Veeley, Carr & Raymond, attorneys and counsellors at law, for about a year, when one morning, in the temporary absence of both Mr. Veeley and Mr. Carr, there came into our office a young man whose whole appearance was so indicative of haste and agitation that I involuntarily rose at his approach and impetuously inquired:")
green.stop<- which(green=="And leaving them there, with the light of growing hope and confidence on their faces, we went out again into the night, and so into a dream from which I have never waked, though the shine of her dear eyes have been now the load-star of my life for many happy, happy months.")
green <- green[green.start:green.stop]

spots <- grep('[A-Z]{2,}[^a-z]', green)
green[spots]
green.chaps<-spots[c(1,3,5,7,9,12,14,16,18,26,28,37,40,42,44,
                   46,48,50,52,54,56,62,64,66,68,71,73,75,
                   77,80,84,87,90, 95,97,99,104)]
green.content<-spots[c(2,4,6,8,11,13,15,17,19,27,30,38,41,43,45,
                     47,49,51,53,55,57,63,65,67,69,72,74,76,
                     79,81,85,88,92,96,98,100,105)]
# green.chaps
# green.content

green.remove <- c(39,70,74, 75, 76, 77, 87:92, 331:333, 407:412, 488:492, 518:520,
                  575, 576, 769:774, 847:850, 941:943, 986:994, 1044:1047, 1113:1115,
                  1226:1228, 1288:1291, 1327:1331, 1431:1433, 1493:1495, 1546:1550, 1584:1588,
                  1584:1588, 1672:1674, 1878:1882, 1961:1965, 2058:2060, 2145:2147,
                  2256:2260, 2298: 2300, 2363:2365, 2386: 2388, 2444:2445, 2450:2456,
                  2749:2752, 2782: 2786, 3076:3078, 3103:3108, 3145:3153, 3194:3205)
# hold out. green.bad <- spots[94]
# green.chaps[1]
# green.content[1]
# green[green.chaps[1]:(green.content[1]-1)]

# green[green.content]
# green.content

green <- green[-green.remove]
grep("BOOK IV", green)
green <- green[-c(2647)]
print(length(green))

green.paragraphs <- as.data.frame(green, stringsAsFactors=FALSE)
colnames(green.paragraphs) <- c("paras")

green.paragraphs<- green.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

green.paragraphs <- green.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

green.paragraphs <- green.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

green.paragraphs <- green.paragraphs %>% 
  filter(paragraphs!="")

green.paragraphs <- green.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(green.paragraphs)

green.paragraphs <- green.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

green.paragraphs <- green.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(green.paragraphs$paragraphs))

green.paragraphs <- green.paragraphs %>% 
  filter(paragraphs!="")

green.paragraphs <- green.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(green.paragraphs$paragraphs))

green.title <- rep("theLeavenworthCase", 3066)
green.para.type <- rep("paragraph",3066)
green.para.counter<-seq(1, 3066)
green.para.id <- paste0("THE_LEAVENWORTH_CASE_", "PARAGRAPH_", green.para.counter)
green.label <- rep("0", 3066)
print(length(green.para.id))

green.para.matrix <- cbind(green.title, green.para.type, green.para.id, green.paragraphs, green.label)
green.para.df <- as.data.frame(green.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(green.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", green.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theLeavenworthCase' LIMIT 2")
dbDisconnect(con)

# sents.

green <- green.paragraphs$paragraphs

first_bite <- green[1:3066]

green.sents.first <- paste0(first_bite, collapse = "\n")
green.sents.first <- unlist(tokenize_sentences(green.sents.first))

green.sents <- c(green.sents.first)
green.sents.df <- as.data.frame(green.sents, stringsAsFactors = FALSE)

print(length(green.sents.df$green.sents))


bad_spots <-c(0)
for(i in seq(1:length(green.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(green.sents[i], nchar(green.sents[i]), nchar(green.sents[i]))
  test2 <- substr(green.sents[i+1], 1, 1)
  test3 <- substr(green.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      green.sents[i] <- paste(green.sents[i], green.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
green.sents[bad_spots]
green.sents <- green.sents[-c(bad_spots)]

print(length(green.sents))

bad_spots <-c(0)
for(i in seq(1:length(green.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(green.sents[i], nchar(green.sents[i])-1, nchar(green.sents[i]))
  test2 <- substr(green.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if((test %in% c('?”', '!”') && test2==tolower(test2))){
      #print(i)
      green.sents[i] <- paste(green.sents[i], green.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

green.sents[bad_spots]
green.sents <- green.sents[-c(bad_spots)]
green.sents <- green.sents[green.sents!=""]
print(length(green.sents))

green.sents.df <- as.data.frame(green.sents, stringsAsFactors = FALSE)

green.title <- rep("theLeavenworthCase", 6133)
green.sents.type <- rep("sentence", 6133)
green.sents.counter<-seq(1, 6133)
green.sents.id <- paste0("THE_LEAVENWORTH_CASE_", "SENT_", green.sents.counter)
green.label <- rep("0", 6133)
print(length(green.sents.id))

green.sents.matrix <- cbind(green.title, green.sents.type, green.sents.id, green.sents, green.label)
green.sents.df <- as.data.frame(green.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(green.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", green.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theLeavenworthCase' LIMIT 2")
dbDisconnect(con)

# words

green.temp <- green
green.temp <- paste(green.temp, collapse=" ")
green.temp <-tolower(green.temp)
# a better regex that is going to maintain contractions. important! 

green.temp <- unlist(strsplit(green.temp, "[^\\w’]", perl=TRUE))
green.not.blanks <- which(green.temp != "")
green.words <- green.temp[green.not.blanks]
print(length(green.words))

green.words<- green.words[which(green.words!="^’")]
green.words<- green.words[which(green.words!="’")]
print(length(green.words))

green.words.df <- as.data.frame(green.words, stringsAsFactors = FALSE)

green.title <- rep("theLeavenworthCase", 110218)
green.words.type <- rep("word", 110218)
green.words.counter <- seq(1, 110218)
green.words.id <- paste0("THE_LEAVENWORTH_CASE_", "WORD_", green.words.counter)
green.label<- rep("0", 110218)
green.words.matrix <- cbind(green.title, green.words.type, green.words.id, green.words, green.label)

green.words.df <- as.data.frame(green.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(green.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", green.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theLeavenworthCase' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
