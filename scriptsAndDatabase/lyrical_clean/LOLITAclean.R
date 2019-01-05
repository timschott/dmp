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

stock <- c("Title", "Type", "ID", "Unit", "Label")

lita <- scan("rawTexts/lyrical/vladimir-nabokov-lolita-2.txt",what="character",sep="\n")

lita.start<- which(lita == "Lolita, light of my life, fire of my loins. My sin, my soul. Lo-lee-ta:")
lita.end <- which(lita == "share, my Lolita.")

lita<- lita[lita.start: lita.end]
lita <- replace_abbreviation(lita)
lita <- gsub('_', '', perl=TRUE, lita)

lita <- gsub('^[0-9]+', '', perl=TRUE, lita)
lita <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, lita)
length(lita)
lita.not.blanks <- which(lita != "")
lita <- lita[lita.not.blanks]

first_bite <- lita[1:2499]
second_bite<- lita[2500:4999]
third_bite <- lita[5000:7499]
fourth_bite <- lita[7500:9176]

lita.sents.first <- paste0(first_bite, collapse = "\n")
lita.sents.first <- unlist(tokenize_sentences(lita.sents.first))

lita.sents.second <- paste0(second_bite, collapse = "\n")
lita.sents.second <- unlist(tokenize_sentences(lita.sents.second))

lita.sents.third <- paste0(third_bite, collapse = "\n")
lita.sents.third <- unlist(tokenize_sentences(lita.sents.third))

lita.sents.fourth <- paste0(fourth_bite, collapse = "\n")
lita.sents.fourth <- unlist(tokenize_sentences(lita.sents.fourth))

lita.sents <- c(lita.sents.first, lita.sents.second, lita.sents.third, lita.sents.fourth)
length(lita.sents)
# punctuation loop.

bad_spots <-c(0)
for(i in seq(1:length(lita.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(lita.sents[i], nchar(lita.sents[i]), nchar(lita.sents[i]))
  test2 <- substr(lita.sents[i+1], 1, 1)
  test3 <- substr(lita.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      lita.sents[i] <- paste(lita.sents[i], lita.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
lita.sents <- lita.sents[-c(bad_spots)]
print(length(lita.sents))

bad_spots <-c(0)
for(i in seq(1:length(lita.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(lita.sents[i], nchar(lita.sents[i])-1, nchar(lita.sents[i]))
  test2 <- substr(lita.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      lita.sents[i] <- paste(lita.sents[i], lita.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
lita.sents[bad_spots]
lita.sents <- lita.sents[-c(bad_spots)]
print(length(lita.sents))

# first person fun.
bad_spots <- c(0)
for(i in seq(1:length(lita.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(lita.sents[i], nchar(lita.sents[i]), nchar(lita.sents[i]))
  test2 <- substr(lita.sents[i+1], 1, 1)
  if(test %in% c('?', '!') && test2%in%c('I')){
    lita.sents[i] <- paste(lita.sents[i], lita.sents[i+1])
    # print(lita.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}
bad_spots

lita.sents <- lita.sents[-bad_spots]

print(length(lita.sents))

lita.sents[1300]<- paste(lita.sents[1300],lita.sents[1301],lita.sents[1302])
lita.sents[1301] <- ""
lita.sents[1302] <- ""
lita.sents[3959] <- paste(lita.sents[3959], lita.sents[3960])
lita.sents[3960] <- ""
lita.sents[1768] <- paste(lita.sents[1768], lita.sents[1769])
lita.sents[1769] <- ""
print(length(lita.sents))

lita.title <- rep("lolita", 5169)
lita.sents.type <- rep("sentence", 5169)
lita.sents.counter<-seq(1, 5169)
lita.sents.id <- paste0("LOLITA_", "SENT_", lita.sents.counter)
lita.label <- rep("1", 5169)
print(length(lita.sents.id))
lita.sents.matrix <- cbind(lita.title, lita.sents.type, lita.sents.id, lita.sents, lita.label)
lita.sents.df <- as.data.frame(lita.sents.matrix, stringsAsFactors = FALSE)
colnames(lita.sents.df) <- stock
# okay i think it's good now.
# lita To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", lita.sents.df, append=TRUE, row.names=FALSE)
# dbExecute(con, "DELETE FROM textTable WHERE Type='sentence' AND Title='lolita'")
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='lolita' LIMIT 2")
dbDisconnect(con)

##### words. 
lita <- scan("rawTexts/vladimir-nabokov-lolita-2.txt",what="character",sep="\n")

lita.start<- which(lita == "Lolita, light of my life, fire of my loins. My sin, my soul. Lo-lee-ta:")
lita.end <- which(lita == "share, my Lolita.")

lita<- lita[lita.start: lita.end]
lita <- replace_abbreviation(lita)
lita <- gsub('_', '', perl=TRUE, lita)

lita <- gsub('^[0-9]+', '', perl=TRUE, lita)

lita.not.blanks <- which(lita != "")
lita <- lita[lita.not.blanks]

lita.temp <- lita
lita.temp <- paste(lita.temp, collapse=" ")
lita.temp <-tolower(lita.temp)
# a better regex that is going to maintain contractions. important! 
lita.temp <- unlist(strsplit(lita.temp, "[^\\w']", perl=T))
lita.not.blanks <- which(lita.temp != "")
lita.words <- lita.temp[lita.not.blanks]

# lots of words! 
print(length(lita.words))


lita.title <- rep("lolita", 112193)
lita.words.type <- rep("word", 112193)
lita.words.counter <- seq(1, 112193)
lita.words.id <- paste0("LOLITA_", "WORD_", lita.words.counter)

lita.words.matrix <- cbind(lita.title, lita.words.type, lita.words.id, lita.words)

lita.words.df <- as.data.frame(lita.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(lita.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", lita.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='lolita' LIMIT 9")
dbDisconnect(con)

### LOLITA, PARAGRAPHS.

### LOLITA, PARAGRAPHS.
lita <- scan("rawTexts/lyrical/vladimir-nabokov-lolita-2.txt",what="character", sep="\n")
lita <- read.csv("Python_Scripts/checkCorpus/LOLITA_paras.csv", stringsAsFactors = FALSE)
colnames(lita) <- c("arb", "para")
# lets kill the line number 
lita <- lita %>%
  transmute(paragraphs = gsub('^[0-9]+', '', perl=TRUE, para))

lita <- lita %>%
  transmute(para = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, paragraphs))

lita <- lita %>% filter(para!="")
lita <- lita %>%
  transmute(paragraph = gsub('\n', ' ', perl=TRUE, para))

lita <- lita %>%
  transmute(para = gsub('\"', "'", perl=TRUE, paragraph))

lita <- lita %>%
  transmute(paragraph = gsub('  ', " ", perl=TRUE, para))
print(length(lita$paragraph))

# phew.. 
# verse connections..
1159:1162
lita$paragraph[1158] <- paste(lita$paragraph[1158], lita$paragraph[1159], lita$paragraph[1160], lita$paragraph[1161])
lita$paragraph[1159] <-""
lita$paragraph[1160] <- ""
lita$paragraph[1161] <- ""
lita$paragraph[1163] <- paste(lita$paragraph[1163], lita$paragraph[1164])
lita$paragraph[1164] <- ""
# lita$paragraph[1163:1164]
lita$paragraph[1166] <- paste(lita$paragraph[1166], lita$paragraph[1167],lita$paragraph[1168],lita$paragraph[1168], lita$paragraph[1169], lita$paragraph[1170])
lita$paragraph[1167] <- ""
lita$paragraph[1168] <- ""
lita$paragraph[1169] <- ""
lita$paragraph[1170] <- ""
lita$paragraph[1172] <- paste(lita$paragraph[1172], lita$paragraph[1173], lita$paragraph[1174])
lita$paragraph[1173] <- ""
lita$paragraph[1174] <- ""
# lita$paragraph[1172:1174]
lita$paragraph[1176] <- paste(lita$paragraph[1176], lita$paragraph[1177], lita$paragraph[1178], lita$paragraph[1179])
lita$paragraph[1177] <- ""
lita$paragraph[1178] <- ""
lita$paragraph[1179] <- ""
# lita$paragraph[1176:1179]

lita$paragraph[1181:1195]
lita$paragraph[1181] <- paste(lita$paragraph[1181], lita$paragraph[1182], lita$paragraph[1183],
                              lita$paragraph[1184], lita$paragraph[1185], lita$paragraph[1186],
                              lita$paragraph[1187], lita$paragraph[1188], lita$paragraph[1189],
                              lita$paragraph[1190], lita$paragraph[1191], lita$paragraph[1192],
                              lita$paragraph[1193], lita$paragraph[1194], lita$paragraph[1195])
lita$paragraph[1182] <- ""
lita$paragraph[1183]<- ""
lita$paragraph[1184]<- ""
lita$paragraph[1185]<- ""
lita$paragraph[1186]<- ""
lita$paragraph[1187]<- ""
lita$paragraph[1188]<- ""
lita$paragraph[1189]<- ""
lita$paragraph[1190]<- ""
lita$paragraph[1191]<- ""
lita$paragraph[1192]<- ""
lita$paragraph[1193]<- ""
lita$paragraph[1194]<- ""
lita$paragraph[1195]<- ""
lita <- lita %>% filter(paragraph!="")
length(lita$paragraph)
lita.title <- rep("lolita", 1189)
lita.para.type <- rep("paragraph", 1189)
lita.para.counter<-seq(1, 1189)
lita.label <- rep("1", 1189)
lita.para.id <- paste0("LOLITA_", "PARAGRAPH_", lita.para.counter)
print(length(lita.para.id))
lita.para.matrix <- cbind(lita.title, lita.para.type, lita.para.id, lita, lita.label)
lita.para.df <- as.data.frame(lita.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(lita.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", lita.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='lolita' LIMIT 2")
dbDisconnect(con)

