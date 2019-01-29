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
## a much cleaner metrics script. 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
# pull out lyrical and detective sentences. 
detective_word_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='word' AND Label ='0'")
detective_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='0'")
detective_para_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='paragraph' AND Label ='0'")
dbDisconnect(con)

detective_titles <- sort(unique(detective_word_df$Title))
# angel_words <- filter(detective_word_df, Title==detective_titles[1])
# length(angel_words$Unit)
# the number of commas is fixed. so let's achieve this readout in one, collective loop. 

detective_commas <- c(0)
detective_sent_counts <- c(0)
detective_para_counts <- c(0)
detective_word_counts <- c(0)

for(i in seq(1:24)){
  sents <- filter(detective_sent_df, Title==detective_titles[i])
  paras <- filter(detective_para_df, Title==detective_titles[i])
  words <- filter(detective_word_df, Title==detective_titles[i])
  comma_count <- sum(str_count(sents$Unit, ","))
  detective_commas <- append(detective_commas, comma_count)
  detective_sent_counts <- append(detective_sent_counts, length(sents$Unit))
  detective_para_counts <- append(detective_para_counts, length(paras$Unit))
  detective_word_counts <- append(detective_word_counts, length(words$Unit))
}
detective_commas <- detective_commas[-c(1)] 

detective_sent_counts <- detective_sent_counts[-c(1)] 
detective_para_counts <- detective_para_counts[-c(1)] 
detective_word_counts <- detective_word_counts[-c(1)] 
length(detective_commas)
length(detective_sent_counts)
# commas per sentence, detective. 
detective_sent_comma_freq <- detective_commas/detective_sent_counts
detective_para_comma_freq <- detective_commas/detective_para_counts

# flip - more useful! how many words can I go without seeing a comma?
# gotta be some kind of weighting to apply to this, like....
# average sentence length
detective_words_per_sentence <- (detective_word_counts / detective_sent_counts)
detective_words_per_paragraph <- (detective_word_counts / detective_para_counts)
detective_sent_per_paragraph <- (detective_sent_counts / detective_para_counts)

# pre lim munging
testing <- c("Hello how are you?")
testing2 <- c("Hello are you okay?")

a<- gsub('^[[:punct:] ]|[[:punct:] ]$','',word(testing))
b<-  gsub('^[[:punct:] ]|[[:punct:] ]$','',word(testing2))
a==b

just_scarlet <- filter(detective_sent_df, Title==detective_titles[1])
the_length <- length(just_scarlet$Unit)
length(seq(1:the_length-1))

detective_consecutive_count <- 0
detective_sents_length_less_one <- 0
detective_consecutive_counts <- c(0)
sent_one_word <- ""
sent_two_word <- ""

# start loop 
for(i in seq(1:24)){
  sents <- filter(detective_sent_df, Title==detective_titles[i])
  detective_sents_length_less_one <- length(sents$Unit)-1
  print("at the moment")
  print(detective_titles[i])
  for(j in seq(1:detective_sents_length_less_one)){
    sent_one_word <- gsub('^[[:punct:] ]|[[:punct:] ]$','',word(sents$Unit[j]))
    sent_two_word <- gsub('^[[:punct:] ]|[[:punct:] ]$','',word(sents$Unit[j+1]))
    if (is.null(sent_one_word)==TRUE | is.null(sent_two_word) ==TRUE) {
      next
    }
    if((sent_one_word == sent_two_word)==TRUE) {
      detective_consecutive_count <- detective_consecutive_count+1
    }
    sent_one_word <- ""
    sent_two_word <- ""
  }
  detective_consecutive_counts <- append(detective_consecutive_counts, detective_consecutive_count)
  detective_consecutive_count <- 0
  sents_length_less_one <- 0
}
detective_consecutive_counts <- detective_consecutive_counts[-c(1)]
detective_consecutive_repeat_freq <- detective_consecutive_counts / detective_sent_counts 

# perhaps we could combine these two strains of logic in order
# to create, % of words with identical consecutive syllables... 
# a la Melville.
# syllables per word. sheesh! 
# qdap baby
# x <- c("Hello there delicious", "My name is 89898", "1975", "Edward")
# j <- syllable_sum(x)
# polysyllable_sum(x)
# sum(syllable_sum(x), na.rm=T)
# syllables per sentence is just going to tell us how many 
# words are in a sentence.. syllables per word though 
# is not correlated with anything so let's use that 
# but polysyllables per sentence is actually kind of insightful.
# polysylls / sentence length ? 

# pull out all words, unleash the script.
# note: strip punctuation, numbers. 

# gotta do this per book and record it because it takes forever. 
# detective_syll_counts <- c(0)
# detective_polysyll_counts <- c(0)
# and using a loop did not work.. 
# words <- filter(detective_word_df, Title==detective_titles[25])

# does not work
#testing_no_accents <- iconv(words$Unit,from="UTF-8",to="ASCII//TRANSLIT")
no_accents <- stri_trans_general(words$Unit,"Latin-ASCII")
counts <- combo_syllable_sum(no_accents)
# total sylls
one_count <- counts$syllable.count
# is it a poly (more than 2.)
poly_count <- counts$polysyllable.count
detective_syll_counts <- scan("syllcounts.txt")
detective_polysyll_counts <- scan("polysyllcounts.txt")

#detective_syll_counts <- append(detective_syll_counts, sum(one_count, na.rm=TRUE))
#detective_polysyll_counts <- append(detective_polysyll_counts, sum(poly_count, na.rm=TRUE))
detective_syll_counts <- detective_syll_counts[-c(1)]
detective_polysyll_counts <- detective_polysyll_counts[-c(1)]

# write(detective_syll_counts, "syllcounts.txt")

# write(detective_polysyll_counts, "polysyllcounts.txt")

detective_syll_and_word_freq <- detective_syll_counts / detective_word_counts
detective_polysyll_and_word_freq <- detective_polysyll_counts /detective_word_counts 

detective_syll_and_sent_freq <- detective_syll_counts / detective_sent_counts
detective_polysyll_and_sent_freq <- detective_polysyll_counts /detective_sent_counts 

# TTR / Mean Usage Freq. 

detective_unique_counts <- c(0)

for(i in seq(1:24)){
  words <- filter(detective_word_df, Title==detective_titles[i])
  detective_unique_counts <- append(detective_unique_counts, length(unique(words$Unit)))
}
detective_unique_counts <- detective_unique_counts[-c(1)]
detective_type_token_ratio <- detective_unique_counts/ detective_word_counts
detective_mean_usage_freq <- 1/detective_type_token

######## Lyrical ##########

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
# pull out lyrical and lyrical sentences. 
lyrical_word_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='word' AND Label ='1'")
lyrical_sent_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='sentence' AND Label ='1'")
lyrical_para_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='paragraph' AND Label ='1'")

dbDisconnect(con)

lyrical_titles <- sort(unique(lyrical_word_df$Title))

# the number of commas is fixed. so let's achieve this readout in one, collective loop. 

lyrical_commas <- c(0)
lyrical_sent_counts <- c(0)
lyrical_para_counts <- c(0)
lyrical_word_counts <- c(0)

for(i in seq(1:26)){
  sents <- filter(lyrical_sent_df, Title==lyrical_titles[i])
  paras <- filter(lyrical_para_df, Title==lyrical_titles[i])
  words <- filter(lyrical_word_df, Title==lyrical_titles[i])
  comma_count <- sum(str_count(sents$Unit, ","))  
  lyrical_commas <- append(lyrical_commas, comma_count)
  lyrical_sent_counts <- append(lyrical_sent_counts, length(sents$Unit))
  lyrical_para_counts <- append(lyrical_para_counts, length(paras$Unit))
  lyrical_word_counts <- append(lyrical_word_counts, length(words$Unit))
}
lyrical_commas <- lyrical_commas[-c(1)] 
lyrical_sent_counts <- lyrical_sent_counts[-c(1)] 
lyrical_para_counts <- lyrical_para_counts[-c(1)] 
lyrical_word_counts <- lyrical_word_counts[-c(1)] 

# commas per sentence, lyrical. 
lyrical_sent_comma_freq <- lyrical_commas/lyrical_sent_counts
lyrical_para_comma_freq <- lyrical_commas/lyrical_para_counts

# flip - more useful! how many words can I go without seeing a comma?
# gotta be some kind of weighting to apply to this, like....
# average sentence length
lyrical_words_per_sentence <- (lyrical_word_counts / lyrical_sent_counts)
lyrical_words_per_paragraph <- (lyrical_word_counts / lyrical_para_counts)
lyrical_sents_per_paragraph <- (lyrical_sent_counts / lyrical_para_counts)

# next --> lexical variety (unique words, )
# going to calculate type-token ratio (# total unique / # total tokens)
# mean word frequency (# total tokens / total unique )
lyrical_unique_counts <- c(0)

for(i in seq(1:26)){
  words <- filter(lyrical_word_df, Title==lyrical_titles[i])
  lyrical_unique_counts <- append(lyrical_unique_counts, length(unique(words$Unit)))
}
lyrical_unique_counts <- lyrical_unique_counts[-c(1)]
lyrical_type_token_ratio <- lyrical_unique_counts/ lyrical_word_counts
lyrical_mean_usage_freq <- 1/lyrical_type_token

# consecutive sentences that start with the same word 

lyrical_consecutive_count <- 0
lyrical_sents_length_less_one <- 0
lyrical_consecutive_counts <- c(0)
sent_one_word <- ""
sent_two_word <- ""

# start loop 
for(i in seq(1:26)){
  sents <- filter(lyrical_sent_df, Title==lyrical_titles[i])
  lyrical_sents_length_less_one <- length(sents$Unit)-1
  print("at the moment")
  print(lyrical_titles[i])
  for(j in seq(1:lyrical_sents_length_less_one)){
    sent_one_word <- gsub('^[[:punct:] ]|[[:punct:] ]$','',word(sents$Unit[j]))
    sent_two_word <- gsub('^[[:punct:] ]|[[:punct:] ]$','',word(sents$Unit[j+1]))
    if (is.null(sent_one_word)==TRUE | is.null(sent_two_word) ==TRUE) {
      next
    }
    if((sent_one_word == sent_two_word)==TRUE) {
      lyrical_consecutive_count <- lyrical_consecutive_count+1
    }
    sent_one_word <- ""
    sent_two_word <- ""
  }
  lyrical_consecutive_counts <- append(lyrical_consecutive_counts, lyrical_consecutive_count)
  lyrical_consecutive_count <- 0
  lyrical_sents_length_less_one <- 0
}
lyrical_consecutive_counts <- lyrical_consecutive_counts[-c(1)]
lyrical_consecutive_repeat_freq <- lyrical_consecutive_counts / lyrical_sent_counts 

# lyrical syllables 

lyrical_words <- filter(lyrical_word_df, Title==lyrical_titles[26])

# to do - 25 - 26. 
# lyrical_syll_counts <- c(0)
# lyrical_polysyll_counts <- c(0)
# does not work
#testing_no_accents <- iconv(words$Unit,from="UTF-8",to="ASCII//TRANSLIT")
lyrical_no_accents <- stri_trans_general(lyrical_words$Unit,"Latin-ASCII")
lyrical_counts <- combo_syllable_sum(lyrical_no_accents)
# total sylls
lyrical_one_count <- lyrical_counts$syllable.count
# is it a poly (more than 2.)
lyrical_poly_count <- lyrical_counts$polysyllable.count
lyrical_syll_counts <- scan("lyricalsyllcounts.txt")
lyrical_polysyll_counts <- scan("lyricalpolysyllcounts.txt")

# lyrical_syll_counts <- lyrical_syll_counts[-c(1)]
# lyrical_polysyll_counts <- lyrical_polysyll_counts[-c(1)]
# lyrical_syll_counts <- append(lyrical_syll_counts, sum(lyrical_one_count, na.rm=TRUE))
# lyrical_polysyll_counts <- append(lyrical_polysyll_counts, sum(lyrical_poly_count, na.rm=TRUE))

lyrical_syll_and_word_freq <- lyrical_syll_counts/lyrical_word_counts
lyrical_polysyll_and_word_freq <- lyrical_polysyll_counts/lyrical_word_counts
lyrical_syll_and_sent_freq <- lyrical_syll_counts/lyrical_sent_counts
lyrical_polysyll_and_sent_freq <- lyrical_polysyll_counts/lyrical_sent_counts

# write(lyrical_syll_counts, "lyricalsyllcounts.txt")

# write(lyrical_polysyll_counts, "lyricalpolysyllcounts.txt")

# bundling .

lyrical_labels <- rep("lyrical", 26)
detective_labels <- rep("detective", 24)

lyrical_ones <- rep(1, 26)
detective_zeros <- rep(1, 24)

# bundle. 
titles_vec <- c(lyrical_titles, detective_titles)
labels_vec <- c(lyrical_labels, detective_labels)
numeric_labels_vec <- c(lyrical_ones, detective_zeros)
word_counts_vec <- c(lyrical_word_counts, detective_word_counts)
sent_counts_vec <- c(lyrical_sent_counts, detective_sent_counts)
para_counts_vec <- c(lyrical_para_counts, detective_para_counts)
commas_vec <- c(lyrical_commas, detective_commas)
sent_comma_freq_vec <- c(lyrical_sent_comma_freq, detective_sent_comma_freq)
para_comma_freq_vec <- c(lyrical_para_comma_freq, detective_para_comma_freq)
words_per_sentence_vec <- c(lyrical_words_per_sentence, detective_words_per_sentence)
words_per_paragraph_vec <- c(lyrical_words_per_paragraph, detective_words_per_paragraph)
sents_per_paragraph_vec <- c(lyrical_sents_per_paragraph, detective_sent_per_paragraph)

# repetition
# consecutive counts
# consecutive frequencies 
consecutive_counts_vec <- c(lyrical_consecutive_counts, detective_consecutive_counts)
consecutive_repeat_freq_vec <- c(lyrical_consecutive_repeat_freq, detective_consecutive_repeat_freq)

# syllables 
# one count, poly count

syll_and_word_freq_vec <- as.numeric(c(lyrical_syll_and_word_freq, detective_syll_and_word_freq))
polsyll_and_word_freq_vec <- c(lyrical_polysyll_and_word_freq, detective_polysyll_and_word_freq)

syll_and_sent_freq_vec <- c(lyrical_syll_and_sent_freq, detective_syll_and_sent_freq)
polsyll_and_sent_freq_vec <- c(lyrical_polysyll_and_sent_freq, detective_polysyll_and_sent_freq)

# TTR / Mean Usage 
# unique counts
unique_counts_vec <- c(lyrical_unique_counts, detective_unique_counts)
type_token_ratio_vec <- c(lyrical_type_token_ratio, detective_type_token_ratio)
mean_usage_frequency_vec <- c(lyrical_mean_usage_freq, detective_mean_usage_freq)

# all together. 
big_df <- as.data.frame(cbind.data.frame(titles_vec,labels_vec, word_counts_vec,sent_counts_vec,para_counts_vec,
                        commas_vec,sent_comma_freq_vec,para_comma_freq_vec,words_per_sentence_vec,
                        words_per_paragraph_vec,sents_per_paragraph_vec,consecutive_counts_vec,
                        consecutive_repeat_freq_vec,syll_and_word_freq_vec,polsyll_and_word_freq_vec,
                        syll_and_sent_freq_vec,polsyll_and_sent_freq_vec,unique_counts_vec,
                        type_token_ratio_vec,mean_usage_frequency_vec, numeric_labels_vec), stringsAsFactors = FALSE)

write.csv(big_df,'starts.csv')
