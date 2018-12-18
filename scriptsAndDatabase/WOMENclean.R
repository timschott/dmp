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

#######################
#####Women IN Love Clean

women <- scan("rawTexts/dh-lawrence-women-in-love.txt",what="character",sep="\n")

women.start<- which(women == "Ursula and Gudrun Brangwen sat one morning in the window-bay of their")
women.end <- which(women == "“I don’t believe that,” he answered.")
women <- women[women.start:women.end]
women <- replace_abbreviation(women)
women <- gsub('_', '', perl=TRUE, women)
# https://stackoverflow.com/questions/36612808/regex-match-roman-numerals-from-0-39-only
# no chapters or chapter names 
women <- gsub('CHAPTER.X{0,3}(IX|IV|V?I{0,3}).', '', perl=TRUE, women)
women <- gsub('[A-Z]{2,}', '', perl=TRUE, women)

# okay lets make sentences. 
length(women)

first_bite <- women[1:2499]
second_bite<- women[2500:4999]
third_bite <- women[5000:7499]
fourth_bite<- women[7500:9999]
fifth_bite <- women[10000:12499]
sixth_bite <- women[15000:17431]

women.sents.first <- paste0(first_bite, collapse = "\n")
women.sents.first <- unlist(tokenize_sentences(women.sents.first))

women.sents.second <- paste0(second_bite, collapse = "\n")
women.sents.second <- unlist(tokenize_sentences(women.sents.second))

women.sents.third <- paste0(third_bite, collapse = "\n")
women.sents.third <- unlist(tokenize_sentences(women.sents.third))

women.sents.fourth <- paste0(fourth_bite, collapse = "\n")
women.sents.fourth <- unlist(tokenize_sentences(women.sents.fourth))

women.sents.fifth <- paste0(fifth_bite, collapse = "\n")
women.sents.fifth <- unlist(tokenize_sentences(women.sents.fifth))

women.sents.sixth <- paste0(sixth_bite, collapse = "\n")
women.sents.sixth <- unlist(tokenize_sentences(women.sents.sixth))

women.sents <- c(women.sents.first, women.sents.second, women.sents.third, women.sents.fourth, women.sents.fifth, women.sents.sixth)
women.sents <- gsub('\"', '' , women.sents, fixed=TRUE)

print(length(women.sents))


women.sents.df <- as.data.frame(women.sents, stringsAsFactors = FALSE)

bad_spots <-c(0)
for(i in seq(1:length(women.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(women.sents[i], nchar(women.sents[i])-1, nchar(women.sents[i]))
  test2 <- substr(women.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?”', '!”') && test2==tolower(test2)){
      #print(i)
      women.sents[i] <- paste(women.sents[i], women.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}

women.sents[bad_spots]
women.sents <- women.sents[-c(bad_spots)]
print(length(women.sents))

### need to check if this is correct. going to confirm with Great Gatsby.

women.title <- rep("womenInLove", 12829)
women.sents.type <- rep("sentence", 12829)
women.sents.counter<-seq(1, 12829)
women.sents.id <- paste0("WOMEN_IN_LOVE_", "SENT_", women.sents.counter)
print(length(women.sents.id))
women.sents.matrix <- cbind(women.title, women.sents.type, women.sents.id, women.sents)
women.sents.df <- as.data.frame(women.sents.matrix, stringsAsFactors = FALSE)
colnames(women.sents.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", women.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='womenInLove' LIMIT 2")
dbDisconnect(con)
