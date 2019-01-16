setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

# This file will be used to acheive "objective" metrics
# in the spirit of Moretti's distant reading and classic Stylometry (a la Josephine Miles)
## commas per sentence

# for sentence in corpus
# count commas
# divide by number of sentences. 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
# pull out lyrical and detective sentences. 
detective_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='0'")
lyrical_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='1'")
dbDisconnect(con)

length(unique(detective_sent_df$Title))
length(unique(lyrical_sent_df$Title))

# 25 titles in each.

# each title
detective_titles <- sort(unique(detective_sent_df$Title))

lyrical_titles <- sort(unique(lyrical_sent_df$Title))

detective_commas <- c(0)
detective_sizes <- c(0)

for(i in seq(1:25)){
  temp <- filter(detective_sent_df, Title==detective_titles[i])
  val <- sum(str_count(temp$Unit, ","))
  len <- length(temp$Unit)
  detective_commas <- append(detective_commas, val)
  detective_sizes <- append(detective_size, len)
}

detective_sent_comma_freq <- detective_commas/detective_sizes
# lyrical loop. 
lyrical_commas <- c(0)
lyrical_sizes <- c(0)

for(i in seq(1:25)){
  temp <- filter(lyrical_sent_df, Title==titles[i])
  val <- sum(str_count(temp$Unit, ","))
  len <- length(temp$Unit)
  lyrical_commas <- append(commas, val)
  lyrical_lengths <- append(lengths, len)
}

lyrical_commas <- lyrical_commas[-c(1)]
lyrical_lengths <- lyrical_lengths[-c(1)]

lyrical_sent_comma_freq <- lyrical_commas/lyrical_sizes

## commas per paragraph
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
myQuery <- dbSendQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='paragraph'")
my_data <- dbFetch(myQuery, n = -1)

dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND title='portraitOfTheArtist' LIMIT 25")


dbClearResult(myQuery)

length(unique(my_data$Title))

titles <- unique(my_data$Title)

commas <- c(0)
lengths <- c(0)

print(length(titles))
pale_fire <- filter(my_data, Title=="paleFire")
pale_count<- sum(str_detect(pale_fire$Unit, ","))
is.na(pale_fire$Unit)
test <- grep(",", pale_fire$Unit)

for(i in seq(1:length(titles))){
  temp <- filter(my_data, Title==titles[i])
  val <- sum(str_count(temp$Unit, ","))
  len <- length(temp$Unit)
  commas <- append(commas, val)
  lengths <- append(lengths, len)
}
print(length(commas))
print(length(lengths))
print(commas)
print(lengths)
commas <- commas[-c(1)]
lengths <- lengths[-c(1)]
commas_per_paragraph <- commas/lengths
dbDisconnect(con)


## words per paragraph



## words per sentence