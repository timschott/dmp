setwd("~/Documents/7thSemester/dmp/corpus")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library("corpus")
library("tidytext")
##### Let's try it with the tokenizer package.
song <-  paste0("How many roads must a man walk down\n",
                "Before you call him a man?\n",
                "How many seas must a white dove sail\n",
                "Before she sleeps in the sand?\n",
                "\n",
                "How many times must the cannonballs fly\n",
                "Before they're forever banned?\n",
                "The answer, my friend, is blowin' in the wind.\n",
                "The answer is blowin' in the wind.\n")
#### SPLITTING INTO PARAGRAPHS

library(dplyr)
library(janeaustenr)

d <- data_frame(txt = prideprejudice)
d

heartOfDarkness <- scan("conrad-heart-of-darkness.txt",what="character", sep="\n")

test_df <- data_frame(text = heartOfDarkness)

test_df <- test_df[-c(1:15),]
test_df <- test_df[-c(1160),]
test_df <- test_df[-c(2128),]
test_df <- test_df[-c(3090:length(test_df$text)),]
test_df <- test_df[-c(3089),]

heartOfDarkness_df <- test_df

# that may have actually worked. lets go through the clean way .. 
#heartOfDarkness_para_df <- heartOfDarkness_df %>%
#  unnest_tokens(word, heartOfDarkness_df$text, token = "word")

my_test<- heartOfDarkness_df %>%
  unnest_tokens(paragraph, text, token="paragraphs")

write.csv(my_test, "bad.csv")



text_split(heartOfDarkness, "sentences")
PandP_sentences <- data_frame(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")
  

#### SPLITTING INTO SENTENCES
# manually throw a \n at the end of every line

heartOfDarkness <- scan("conrad-heart-of-darkness.txt",what="character",sep="\n")

heartOfDarkness.start<- which(heartOfDarkness == "I")
heartOfDarkness.end <- which(heartOfDarkness == "sky--seemed to lead into the heart of an immense darkness.")

heartOfDarkness<-heartOfDarkness[heartOfDarkness.start: heartOfDarkness.end]

heartOfDarkness.sents<-heartOfDarkness[-(1)]
heartOfDarkness.sents<-heartOfDarkness.sents[-(1160)]
heartOfDarkness.sents<-heartOfDarkness.sents[-(2128)]

#paste is dumb with big inputs so break in half to be more manageable

#break near middle at full sentence. 
first_half <- heartOfDarkness.sents[1:1543]
second_half<- heartOfDarkness.sents[-(1:1543)]

heartOfDarkness.sents.first <- paste0(first_half, collapse = "\n")
heartOfDarkness.sents.first <- unlist(tokenize_sentences(heartOfDarkness.sents.first))

heartOfDarkness.sents.second <- paste0(second_half, collapse = "\n")
heartOfDarkness.sents.second <- unlist(tokenize_sentences(heartOfDarkness.sents.second))

#recombine

heartOfDarkness.sents <- c(heartOfDarkness.sents.first,heartOfDarkness.sents.second)

####
#### BETTER WAY TO BREAK DOWN INTO WORDS
# Better way to do my original work 

heartOfDarkness <- scan("conrad-heart-of-darkness.txt",what="character",sep="\n")
heartOfDarkness.start<- which(heartOfDarkness == "I")
heartOfDarkness.end <- which(heartOfDarkness == "sky--seemed to lead into the heart of an immense darkness.")

heartOfDarkness<-heartOfDarkness[heartOfDarkness.start: heartOfDarkness.end]

test<-grep("I looked at him, lost in astonishment", heartOfDarkness)


## Grep out VOLUME markers (likely manually)

heartOfDarkness.temp<-heartOfDarkness[-(1)]
heartOfDarkness.temp<-heartOfDarkness.temp[-(1160)]
heartOfDarkness.temp<-heartOfDarkness.temp[-(2128)]
heartOfDarknessCount <-sum(sapply(heartOfDarkness.temp, str_count, "\\w+"))

## Replace Contractions
heartOfDarkness.temp<-replace_contraction(heartOfDarkness.temp)


#### JUST THE WORDS? 

heartOfDarkness.temp <- paste(heartOfDarkness.temp, collapse=" ")
heartOfDarkness.temp <-tolower(heartOfDarkness.temp)
heartOfDarkness.temp <- strsplit(heartOfDarkness.temp, "\\W")

heartOfDarkness.temp <-unlist(heartOfDarkness.temp)

darkness.not.blanks <- which(heartOfDarkness.temp != "")

heartOfDarknessWords <- heartOfDarkness.temp[darkness.not.blanks]

heartOfDarknessWords[1:10]

writeLines(heartOfDarknessWords, "heartOfDarknessWordsTest.txt")



