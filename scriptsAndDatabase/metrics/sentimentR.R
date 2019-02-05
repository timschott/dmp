### sent work 
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
library('sentimentr')

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
detective_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='0'")
detective_titles <- sort(unique(detective_sent_df$Title))
dbDisconnect(con)

test <- detective_sent_df$Unit[1:25]
s <- sentiment(test)
s_by <- sentiment_by(test)
sum(s$sentiment)/25
test <- get_sentences(detective_sent_df$Unit[1:5000])
sum(sentiment(detective_sent_df$Unit[1:5000]))/5000

# "this is what we'll go to war with"


detective_sentiment_vec <- c(0)

for(i in seq(from =1, to=24)){
  sents <- filter(detective_sent_df, Title==detective_titles[i])
  sents <- sents$Unit
  sentiments <- sentiment_by(sents)
  detective_sentiment_vec <- append(detective_sentiment_vec, sum(sentiments$ave_sentiment/length(sents)))
}
detective_sentiment_vec <- detective_sentiment_vec[-c(1)]

# lyrical

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
lyrical_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='1'")
lyrical_titles <- sort(unique(detective_sent_df$Title))
dbDisconnect(con)

lyrical_sentiment_vec <- c(0)

for(i in seq(from =1, to=26)){
  sents <- filter(lyrical_sent_df, Title==lyrical_titles[i])
  sents <- sents$Unit
  sentiments <- sentiment_by(sents)
  lyrical_sentiment_vec <- append(lyrical_sentiment_vec, sum(sentiments$ave_sentiment/length(sents)))
}
lyrical_sentiment_vec <- lyrical_sentiment_vec[-c(1)]

