### dialOgics 
setwd("~/Documents/7thSemester/dmp/corpus")
library(scales)
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library(stringi)
library("tm")
library(qdap)

big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
detective_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='0'")
detective_para_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='paragraph' AND Label ='0'")

length(detective_sent_df$Unit)
# okay, plenty of sentences.


detective_sent_df$Unit[633:700]
detective_sent_df$Unit[701:777]
detective_sent_df$Unit[5655:5699]

detective_para_df$Unit[39:50]
replace_contraction("'What can I do for you?' he asked.")
replace_contraction("Thomas' sword")
# manually replace I'd I'm I'll I've 

x <- c("ralph's","marco's", "tims'", "I'm, 'tis")
gsub("([a-z])(?:'[a-z])|([a-z])(?:'[a-z])",paste0("\\1","s"),x, perl=TRUE)
# okay
# now to work on tims'
# for possessives ending in s apost.
gsub("([a-z])(?:')|([a-z])(?:'[a-z])","\\1",x, perl=TRUE)

# cool.

# okay so move through the paragraphs, run these gsubs, and then
# we should be good to count tick marks.

test <- detective_para_df$Unit[1999:2299]

test <- gsub("I'm", "Im", test)
test <- gsub("I'll", "Ill", test)
test <- gsub("I've", "Im", test)
test <- gsub("I'd", "Id", test)
test <- gsub("'tis", "tis", test)
test <- gsub("o'clock", "o clock", test)
test <- gsub("([a-z])(?:'[a-z])|([a-z])(?:'[a-z])",paste0("\\1","s"),test, perl=TRUE)
test<-gsub("([a-z])(?:')|([a-z])(?:'[a-z])","\\1",test, perl=TRUE)

test[1:10]
