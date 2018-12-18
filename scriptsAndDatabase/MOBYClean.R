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

################
## MOBY DICK ##

moby <- scan("rawTexts/herman-melville-moby-dick.txt",what="character",sep="\n")

moby.start<- which(moby == "Call me Ishmael. Some years ago—never mind how long precisely—having")
moby.end <- which(moby == "children, only found another orphan.")

moby<- moby[moby.start: moby.end]
moby <- replace_abbreviation(moby)
moby <- gsub('_', '', perl=TRUE, moby)

# taking inititave and grepping out chapter markers. 

moby<-  gsub('CHAPTER [0-9]+..*', "", perl=TRUE, moby)
moby <- gsub("[0-9]", '', moby)
moby.not.blanks <- which(moby != "")
moby <- moby[moby.not.blanks]

length(moby)

# moby is 18256 lines. so really big. so have to split it a lot. 
first_bite <- moby[1:2499]
second_bite<- moby[2500:4999]
third_bite <- moby[5000:7499]
fourth_bite<- moby[7500:9999]
fifth_bite <- moby[10000:12499]
sixth_bite<- moby[15000:17499]
seventh_bite<- moby[17500:18256]

moby.sents.first <- paste0(first_bite, collapse = "\n")
moby.sents.first <- unlist(tokenize_sentences(moby.sents.first))

moby.sents.second <- paste0(second_bite, collapse = "\n")
moby.sents.second <- unlist(tokenize_sentences(moby.sents.second))

moby.sents.third <- paste0(third_bite, collapse = "\n")
moby.sents.third <- unlist(tokenize_sentences(moby.sents.third))

moby.sents.fourth <- paste0(fourth_bite, collapse = "\n")
moby.sents.fourth <- unlist(tokenize_sentences(moby.sents.fourth))

moby.sents.fifth <- paste0(fifth_bite, collapse = "\n")
moby.sents.fifth <- unlist(tokenize_sentences(moby.sents.fifth))

moby.sents.sixth <- paste0(sixth_bite, collapse = "\n")
moby.sents.sixth <- unlist(tokenize_sentences(moby.sents.sixth))

moby.sents.seventh <- paste0(seventh_bite, collapse = "\n")
moby.sents.seventh <- unlist(tokenize_sentences(moby.sents.seventh))

moby.sents <- c(moby.sents.first, moby.sents.second, moby.sents.third, moby.sents.fourth, moby.sents.fifth, moby.sents.sixth, moby.sents.seventh)
moby.sents <- gsub('\"', '' , moby.sents, fixed=TRUE)

moby.sents <- gsub('\\([A-z]+\\),.CHAPTER.[A-z]{1,}\\.', "", perl=TRUE,moby.sents)

# now we need to join sentences that end in punctuation and begin with a capital letter 

test <- c("Hello my name is tim?")
substr(test, nchar(test), nchar(test))

bad_spots<-c(0)

first_boy <- c("Hello, my name is tim!")
second_boy <- c(" he bellowed.")
substr(moby.sents[1], nchar(moby.sents[1])-1, nchar(moby.sents[1]))
substr(moby.sents[2], 1, 1)

for(i in seq(1:length(moby.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(moby.sents[i], nchar(moby.sents[i]), nchar(moby.sents[i]))
  test2 <- substr(moby.sents[i+1], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2)){
    moby.sents[i] <- paste(moby.sents[i], moby.sents[i+1])
    print(moby.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}

moby.sents <- moby.sents[-bad_spots]
bad_spots <-c(0)
moby.sents[1004]
for(i in seq(1:length(moby.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(moby.sents[i], nchar(moby.sents[i])-1, nchar(moby.sents[i]))
  test2 <- substr(moby.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?”', '!”') && test2==tolower(test2)){
      print(i)
      moby.sents[i] <- paste(moby.sents[i], moby.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
moby.sents[bad_spots]
moby.sents <- moby.sents[-bad_spots]

moby.sents.df <- as.data.frame(moby.sents, stringsAsFactors = FALSE)

write.csv(moby.sents.df, 'mobysents.csv')
### need to check if this is correct. going to confirm with Great Gatsby.

moby.title <- rep("mobyDick", 8226)
moby.sents.type <- rep("sentence", 8226)
moby.sents.counter<-seq(1, 8226)
moby.sents.id <- paste0("MOBY_", "SENT_", moby.sents.counter)
print(length(moby.sents.id))
moby.sents.matrix <- cbind(moby.title, moby.sents.type, moby.sents.id, moby.sents)
moby.sents.df <- as.data.frame(moby.sents.matrix, stringsAsFactors = FALSE)

# okay i think it's good now.
# Moby To Do: press into sents into DB; 
#paras clean; 
#words clean.