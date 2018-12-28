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

# gravity's rainbow

stock <- c("Title", "Type", "ID", "Unit")
grav <- scan("rawTexts/thomas-pynchon-gravitys-rainbow.txt",what="character", sep="\n")

# what to sub, what to sub. 
# Capt.; Mr. Mrs;
# gravbrev regex
# 				• • • • • • •
# W.C.s =- W C S
grep("• • • • • • •", grav)
grep("\t", grav)
print(length(grav))
grav[669:671]

grav.paragraphs <- as.data.frame(grav, stringsAsFactors=FALSE)
colnames(grav.paragraphs) <- c("paras")

grav.paragraphs <- grav.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr.", "Mr", paras))

grav.paragraphs <- grav.paragraphs %>% 
  transmute(paras=  gsub("Mrs.", "Mr", paragraphs))

grav.paragraphs <- grav.paragraphs %>% 
  transmute(paragraphs=  gsub("W.C.s.", "W C s", paras))

grav.paragraphs <- grav.paragraphs %>% 
  transmute(pagraphs=  gsub("[\t\t\t\t]", "", paragraphs))

grav.paragraphs <- grav.paragraphs %>% 
  transmute(paras=  gsub("• • • • • • •", "", pagraphs))

grav.paragraphs <- grav.paragraphs %>% 
  transmute(paragraphs=  replace_abbreviation(paras))

grav.paragraphs <- grav.paragraphs %>%
  transmute(paras = gsub("Lt.", "Lt", paragraphs))

grav.paragraphs <- grav.paragraphs %>%
  transmute(paragraphs = gsub("Z.", "Z", paras))

grav.paragraphs <- grav.paragraphs %>%
  transmute(paras = gsub("A.F.", "A F", paragraphs))

grav.paragraphs <- grav.paragraphs %>%
  transmute(paragraphs = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paras))

grav.paragraphs <- grav.paragraphs %>%
  transmute(paras = gsub("d. 1812", "d 1812", paragraphs))

grav.paragraphs <- grav.paragraphs %>%
  transmute(paragraphs = gsub("pg.", "pg", paras))

grav.paragraphs <- grav.paragraphs %>%
  transmute(paras = gsub("Cecil B. De Mille", "Cecil B De Mille", paragraphs))
  
colnames(grav.paragraphs) <- c("paragraphs")

grav.paragraphs <- grav.paragraphs %>% 
  filter(paragraphs!="")
print(length(grav.paragraphs$paragraphs))

# cool...
grav.paragraphs$paragraphs[5000:5010]
print(length(grav.paragraphs$paragraphs))
# paragraphs should be okay..?

grav.title <- rep("gravitysRainbow", 5669)
grav.para.type <- rep("paragraph", 5669)
grav.para.counter<-seq(1, 5669)
grav.para.id <- paste0("GRAVITYS_RAINBOW_", "PARAGRAPH_", grav.para.counter)
print(length(grav.para.id))

grav.para.matrix <- cbind(grav.title, grav.para.type, grav.para.id, grav.paragraphs)
grav.para.df <- as.data.frame(grav.para.matrix, stringsAsFactors = FALSE)
colnames(grav.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", grav.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='gravitysRainbow' LIMIT 2")
dbDisconnect(con)

### sentences 
### tokenize the man...
grav <- grav.paragraphs$paragraphs
first_bite <- grav[1:2499]
second_bite <- grav[2500:5669]

grav.sents.first <- paste0(first_bite, collapse = "\n")
grav.sents.first <- unlist(tokenize_sentences(grav.sents.first))

grav.sents.second <- paste0(second_bite, collapse = "\n")
grav.sents.second <- unlist(tokenize_sentences(grav.sents.second))

grav.sents <- c(grav.sents.first, grav.sents.second)
grav.sents.df <- as.data.frame(grav.sents, stringsAsFactors = FALSE)
print(length(grav.sents.df$grav.sents))

# gsub Lt.
# gsub Z

## no quote loop
## curly quote loop. 

bad_spots <-c(0)
for(i in seq(1:length(grav.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(grav.sents[i], nchar(grav.sents[i]), nchar(grav.sents[i]))
  test2 <- substr(grav.sents[i+1], 1, 1)
  test3 <- substr(grav.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      grav.sents[i] <- paste(grav.sents[i], grav.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
# grav.sents[bad_spots]
grav.sents <- grav.sents[-c(bad_spots)]

bad_spots <-c(0)
for(i in seq(1:length(grav.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(grav.sents[i], nchar(grav.sents[i])-1, nchar(grav.sents[i]))
  test2 <- substr(grav.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?”', '!”') && test2==tolower(test2)){
      #print(i)
      grav.sents[i] <- paste(grav.sents[i], grav.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

# grav.sents[bad_spots]
grav.sents <- grav.sents[-c(bad_spots)]
# print(length(grav.sents))
grav.sents.df <- as.data.frame(grav.sents, stringsAsFactors = FALSE)
# better. 
# let's look. 
# alright. 


print(length(grav.sents))

grav.title <- rep("gravitysRainbow", 18847)
grav.sents.type <- rep("sentence", 18847)
grav.sents.counter<-seq(1, 18847)
grav.sents.id <- paste0("GRAVITYS_RAINBOW_", "SENT_", grav.sents.counter)
print(length(grav.sents.id))

grav.sents.matrix <- cbind(grav.title, grav.sents.type, grav.sents.id, grav.sents)
grav.sents.df <- as.data.frame(grav.sents.matrix, stringsAsFactors = FALSE)
colnames(grav.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", grav.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='gravitysRainbow' LIMIT 2")
dbDisconnect(con)

# words! 
grav.temp <- grav.paragraphs$paragraphs
grav.temp.1 <- paste(grav.temp[1:999], collapse=" ")
grav.temp.2 <- paste(grav.temp[1000:1999], collapse=" ")
grav.temp.3 <- paste(grav.temp[2000:2999], collapse=" ")
grav.temp.4 <- paste(grav.temp[3000:3999], collapse=" ")
grav.temp.5 <- paste(grav.temp[4000:5669], collapse=" ")

grav.temp.1 <-tolower(grav.temp.1)
grav.temp.2 <-tolower(grav.temp.2)
grav.temp.3 <-tolower(grav.temp.3)
grav.temp.4 <-tolower(grav.temp.4)
grav.temp.5 <-tolower(grav.temp.5)

grav.temp.1 <- unlist(strsplit(grav.temp.1, "[^\\w’]", perl=TRUE))
grav.temp.2 <- unlist(strsplit(grav.temp.2, "[^\\w’]", perl=TRUE))
grav.temp.3 <- unlist(strsplit(grav.temp.3, "[^\\w’]", perl=TRUE))
grav.temp.4 <- unlist(strsplit(grav.temp.4, "[^\\w’]", perl=TRUE))
grav.temp.5 <- unlist(strsplit(grav.temp.5, "[^\\w’]", perl=TRUE))

# a better regex that is going to maintain contractions. important! 

grav.not.blanks.1 <- which(grav.temp.1 != "")
grav.not.blanks.2 <- which(grav.temp.2 != "")
grav.not.blanks.3 <- which(grav.temp.3 != "")
grav.not.blanks.4 <- which(grav.temp.4 != "")
grav.not.blanks.5 <- which(grav.temp.5 != "")

grav.words.1 <- grav.temp.1[grav.not.blanks.1]
grav.words.2 <- grav.temp.2[grav.not.blanks.2]
grav.words.3 <- grav.temp.3[grav.not.blanks.3]
grav.words.4 <- grav.temp.4[grav.not.blanks.4]
grav.words.5 <- grav.temp.5[grav.not.blanks.5]
grav.words <- c(grav.words.1,grav.words.2,grav.words.3,grav.words.4,grav.words.5)

print(length(grav.words))
grav.words<- grav.words[which(grav.words!="^’")]
print(length(grav.words))

# sheesh. 333k words.
gravitys.title <- rep("gravitysRainbow", 333370)
gravitys.words.type <- rep("word", 333370)
gravitys.words.counter <- seq(1, 333370)
gravitys.words.id <- paste0("GRAVITYS_RAINBOW_", "WORD_", gravitys.words.counter)

gravitys.words.matrix <- cbind(gravitys.title, gravitys.words.type, gravitys.words.id, grav.words)

gravitys.words.df <- as.data.frame(gravitys.words.matrix, stringsAsFactors = FALSE)
# 37.1 MB!!! "yuge!"
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(gravitys.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", gravitys.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='gravitysRainbow' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word'")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='sentence'")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='paragraph'")
dbExecute(con, "ALTER TABLE textTable ADD 'Label' INT")
dbExecute(con, "UPDATE textTable SET 'Label' = 1")
# 3 million "lyrical". 
dbDisconnect(con)


