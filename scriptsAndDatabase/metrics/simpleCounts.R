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

### simple counts.
# ! and ?

big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
detective_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='0'")
detective_titles <- sort(unique(detective_sent_df$Title))
dbDisconnect(con)

# loop and counts.

detective_excl_vec <- c(0)
detective_question_vec <- c(0)

for(i in seq(from =1, to=24)){
  sents <- filter(detective_sent_df, Title==detective_titles[i])
  sents <- sents$Unit
  detective_excl_vec <- append(detective_excl_vec, sum(str_count(sents, "\\!")))
  detective_question_vec <- append(detective_question_vec, sum(str_count(sents, "\\?")))
}
detective_excl_vec <- detective_excl_vec[-c(1)]
detective_question_vec <- detective_question_vec[-c(1)]

# let's do per sentence as not 

# okay replicate with lyrical 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
lyrical_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='1'")
lyrical_titles <- sort(unique(lyrical_sent_df$Title))
dbDisconnect(con)

# loop and counts.

lyrical_excl_vec <- c(0)
lyrical_question_vec <- c(0)

for(i in seq(from =1, to=26)){
  sents <- filter(lyrical_sent_df, Title==lyrical_titles[i])
  sents <- sents$Unit
  lyrical_excl_vec <- append(lyrical_excl_vec, sum(str_count(sents, "\\!")))
  lyrical_question_vec <- append(lyrical_question_vec, sum(str_count(sents, "\\?")))
}
lyrical_titles
# some shockingly low numbers here for mccarthy.
lyrical_excl_vec <- lyrical_excl_vec[-c(1)]
# makes sense that eureka, a weird essay of sorts, has low # of questions 
lyrical_question_vec <- lyrical_question_vec[-c(1)]

question_vec <- c(lyrical_question_vec, detective_question_vec)
exclamation_vec <- c(lyrical_excl_vec, detective_excl_vec)

big_boy <- as.data.frame(cbind(big_boy, question_vec, exclamation_vec), stringsAsFactors = FALSE)
colnames(big_boy)
write.csv(big_boy,'starts.csv')
