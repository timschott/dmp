setwd("~/Documents/7thSemester/dmp/corpus")
install.packages("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")

##### Let's try it with the tokenizer package.
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

### BAG OF WORDS CREATION
heartOfDarkness <- scan("conrad-heart-of-darkness.txt",what="character",sep="\n")
heartOfDarkness.start<- which(heartOfDarkness == "I")
heartOfDarkness.end <- which(heartOfDarkness == "sky--seemed to lead into the heart of an immense darkness.")

heartOfDarkness<-heartOfDarkness[heartOfDarkness.start: heartOfDarkness.end]

#try just giving it the text file (instead of it erupting into multiple tiny documents])
heartOfDarkness.tm <- VectorSource(heartOfDarkness)
heartOfDarkness.tm <- Corpus(heartOfDarkness.tm)

heartOfDarkness.cleaning<-tm_map(heartOfDarkness.tm,removeNumbers)
heartOfDarkness.cleaning<-tm_map(heartOfDarkness.cleaning,stripWhitespace)
heartOfDarkness.cleaning<-tm_map(heartOfDarkness.cleaning, content_transformer(tolower))
heartOfDarkness.cleaning<-tm_map(heartOfDarkness.cleaning, removePunctuation)
heartOfDarkness.cleaning<-tm_map(heartOfDarkness.cleaning, removeWords,stopwords("english"))

conrad.dtm<-DocumentTermMatrix(heartOfDarkness.cleaning)
conrad.dtm<- removeSparseTerms(conrad.dtm, 0.95)
dim(conrad.dtm)
str(conrad.dtm)


meta.matrix<-as.matrix(watts.dtm)
meta.dist<-dist(meta.matrix)
dist.matrix<-as.matrix(meta.dist)
meta.scaled<-meta.matrix/rowSums(meta.matrix)
meta.scaled<-meta.scaled*10000 
meta.pca<-prcomp(meta.scaled)




