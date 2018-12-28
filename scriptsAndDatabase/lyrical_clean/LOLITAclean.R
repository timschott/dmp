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

stock <- c("Title", "Type", "ID", "Unit")

lita <- scan("rawTexts/vladimir-nabokov-lolita-2.txt",what="character",sep="\n")

lita.start<- which(lita == "Lolita, light of my life, fire of my loins. My sin, my soul. Lo-lee-ta:")
lita.end <- which(lita == "share, my Lolita.")

lita<- lita[lita.start: lita.end]
lita <- replace_abbreviation(lita)
lita <- gsub('_', '', perl=TRUE, lita)

lita <- gsub('^[0-9]+', '', perl=TRUE, lita)
lita <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, lita)

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

bad_spots<-c(0)
substr(lita.sents[750], nchar(lita.sents[750]), nchar(lita.sents[750]))

for(i in seq(1:length(lita.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(lita.sents[i], nchar(lita.sents[i]), nchar(lita.sents[i]))
  test2 <- substr(lita.sents[i+1], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2)){
    lita.sents[i] <- paste(lita.sents[i], lita.sents[i+1])
    # print(lita.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}
bad_spots

lita.sents <- lita.sents[-bad_spots]

print(length(lita.sents))
# first person fun.

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

lita.title <- rep("lolita", 5063)
lita.sents.type <- rep("sentence", 5063)
lita.sents.counter<-seq(1, 5063)
lita.sents.id <- paste0("LOLITA_", "SENT_", lita.sents.counter)
print(length(lita.sents.id))
lita.sents.matrix <- cbind(lita.title, lita.sents.type, lita.sents.id, lita.sents)
lita.sents.df <- as.data.frame(lita.sents.matrix, stringsAsFactors = FALSE)
colnames(lita.sents.df) <- stock
# okay i think it's good now.
# lita To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", lita.sents.df, append=TRUE, row.names=FALSE)
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
lita <- scan("rawTexts/vladimir-nabokov-lolita-2.txt",what="character", sep="\n")
# lets kill the line number 
lita <- gsub('^[0-9]+', '', perl=TRUE, lita)
lita <- gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, lita)
lita.not.blanks <- which(lita != "")
lita <- replace_abbreviation(lita)

lita <- lita[lita.not.blanks]
lita <- gsub('\"', '', perl=TRUE, lita)

#tiny <- lita[1:10]
#tiny_test <- paste(tiny, "\n", sep="")
#tiny_test
#print(length(lita))
#test <- paste(lita[7148:7150],"\n", collapse="")
#test2<-paste(test, collapse="\n")
#test3 <- gsub("\n", "", perl=TRUE, test2)

lita_ind <- lita[1:9177]
lita_slash <- paste(lita_ind, "\n", sep="")

# procedure: 
# add \n to end of every line.
# collapse index-- end of index on "\n" 
# append to para vec
# for all in para vec, sub "" for \n .
paras_ind <- grep("     ", lita)
paras <- c('')

for(i in seq(1:length(lita))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  #test <- substr(lita.sents[i], nchar(lita.sents[i]), nchar(lita.sents[i]))
  if(i<1211){
    paragraph <- paste(lita_slash[paras_ind[i]:(paras_ind[i+1]-1)], collapse="\n")
    paras <- append(paras, paragraph)
  }
}
paras <- paras[-c(1)]
first_para <- c("Lolita, light of my life, fire of my loins. My sin, my soul. Lo-lee-ta: the tip  of  the tongue taking a trip of three steps down the palate to tap, at three, on the teeth. Lo. Lee. Ta.")
last_para <- c("Thus, neither of us is alive when the reader opens this book. But while the blood  still  throbs through my writing hand, you are still as much part of blessed matter as I am, and I can still talk to you from here to  Alaska. Be  true  to  your  Dick. Do not let other fellows touch you. Do not talk to strangers. I hope you will love your baby. I hope it will  be  a  boy.  That husband  of  yours, I hope, will always treat you well, because otherwise my specter shall come at him, like black smoke, like a demented giant, and pull him apart nerve by nerve. And do not pity C. Q. One had  to  choose  between him  and  H.H.,  and  one  wanted  H.H. to exist at least a couple of months longer, so as to have him make you live in the minds of later generations. I am thinking of aurochs and angels, the secret of durable pigments, prophetic sonnets, the refuge of art. And this is the only immortality you and  I  may share, my Lolita.")
# okay, so let's just build our own paragraphs...

## okay take out the new lines. double and single. 
paras_out <- gsub('\n\n', ' ', perl=TRUE, paras)
paras <- gsub('\n', '', perl=TRUE, paras_out)
paras_out<- gsub('     ', '', perl=TRUE, paras)
paras <- gsub('  ', ' ', perl=TRUE, paras_out)

# okay finally. 
print(length(paras))

lita.paras <- c(first_para, paras, last_para)

# phew.. 

lita.title <- rep("lolita", 1212)
lita.para.type <- rep("paragraph", 1212)
lita.para.counter<-seq(1, 1212)
lita.para.id <- paste0("LOLITA_", "PARAGRAPH_", lita.para.counter)
print(length(lita.para.id))
lita.para.matrix <- cbind(lita.title, lita.para.type, lita.para.id, lita.paras)
lita.para.df <- as.data.frame(lita.para.matrix, stringsAsFactors = FALSE)
colnames(lita.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", lita.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='lolita' LIMIT 2")
dbDisconnect(con)

