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
detective_para_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='paragraph' AND Label ='0'")
detective_titles <- sort(unique(detective_para_df$Title))
dbDisconnect(con)

test <- detective_sent_df$Unit[1:25]
s <- sentiment(test)
s_by <- sentiment_by(test)
sum(s$sentiment)/25


# okay so you could just pipe this through each of my books 
# and sum up the sentiments as you go. 


