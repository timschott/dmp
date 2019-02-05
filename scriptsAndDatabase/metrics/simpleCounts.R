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
# ! and ? and -- [em-dash] and percent of words that are numbers

big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
detective_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='0'")
detective_titles <- sort(unique(detective_sent_df$Title))
dbDisconnect(con)

# loop and counts.

detective_excl_vec <- c(0)
detective_question_vec <- c(0)
detective_dash_vec <- c(0)
detective_dash_vec_2 <- c(0)

for(i in seq(from =1, to=24)){
  sents <- filter(detective_sent_df, Title==detective_titles[i])
  sents <- sents$Unit
  detective_excl_vec <- append(detective_excl_vec, sum(str_count(sents, "\\!")))
  detective_question_vec <- append(detective_question_vec, sum(str_count(sents, "\\?")))
  detective_dash_vec <- append(detective_dash_vec, sum(str_count(sents, "—")))
  if(i==15 || i==12 || i==10 || i==24){
    print(sum(str_count(sents, "--")))
    # weird temp appending 
    detective_dash_vec_2 <- append(detective_dash_vec_2, sum(str_count(sents, "--")))
  }
}
detective_excl_vec <- detective_comma_vec[-c(1)]
detective_question_vec <- detective_question_vec[-c(1)]
detective_dash_vec <- detective_dash_vec[-c(1)]
detective_dash_vec_2 <- detective_dash_vec_2[-c(1)]

detective_dash_vec[10] <- detective_dash_vec_2[1]
detective_dash_vec[12] <- detective_dash_vec_2[2]
detective_dash_vec[15] <- detective_dash_vec_2[3]
detective_dash_vec[24] <- detective_dash_vec_2[4]

room <- filter(detective_sent_df, Title==detective_titles[15])
# use -- for 15 and 12 and 10 and 24
sum(which(grep("--", room$Unit) %in% room$Unit))

test <- c("Hello -- there --", "-- ok", "good --")
str_count(test, "--")

# let's do per sentence as not 

# okay replicate with lyrical 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
lyrical_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='1'")
lyrical_titles <- sort(unique(lyrical_sent_df$Title))
dbDisconnect(con)

# loop and counts.

lyrical_excl_vec <- c(0)
lyrical_question_vec <- c(0)
lyrical_dash_vec <- c(0)
# lyrical_dash_vec_2 <- c(0)

for(i in seq(from =1, to=26)){
  sents <- filter(lyrical_sent_df, Title==lyrical_titles[i])
  sents <- sents$Unit
  lyrical_excl_vec <- append(lyrical_excl_vec, sum(str_count(sents, "\\!")))
  lyrical_question_vec <- append(lyrical_question_vec, sum(str_count(sents, "\\?")))
  lyrical_dash_vec <- append(lyrical_dash_vec, sum(str_count(sents, "—")))
}
lyrical_titles
# some shockingly low numbers here for mccarthy.
lyrical_excl_vec <- lyrical_excl_vec[-c(1)]
# makes sense that eureka, a weird essay of sorts, has low # of questions 
lyrical_question_vec <- lyrical_question_vec[-c(1)]

lyrical_dash_vec