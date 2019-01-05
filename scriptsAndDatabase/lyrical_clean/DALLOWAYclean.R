setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("stringi")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
# library(rJava)
# library("openNLPdata")

stock <- c("Title", "Type", "ID", "Unit", "Label")
dali <- scan("rawTexts/lyrical/virginia-woolf-mrs-dalloway.txt",what="character",sep="\n")
dali.start <-which(dali == "Mrs. Dalloway said she would buy the flowers herself.")
dali.end <- which(dali == "For there she was.")
dali <- dali[dali.start:dali.end]

dali <- gsub('_', '', perl=TRUE, dali)
dali<-  gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, dali)
dali <- gsub('Mrs\\.', 'Mrs', perl=TRUE, dali)
dali <- gsub('Mr\\.', 'Mr', perl=TRUE, dali)
dali <- gsub('St\\.', 'St', perl=TRUE, dali)
dali <- gsub("t?te-?-t?tes", "tête-à-tête", perl=TRUE, dali)
dali <- gsub("caf?", "café", perl=TRUE,dali)
dali <- gsub('\"', "'", perl=TRUE, dali)
# bad diacritics! 
dali[167] <- "knowledge Fräulein Daniels gave them she could not think. She knew"
dali[3266] <- "somehow right. So she let Hugh eat his soufflé; asked after poor"
dali[3935] <- "inches of a chocolate éclair."
dali[3938] <- "inches of the chocolate éclair, then wiped her fingers, and washed"
dali[3973] <- "éclairs, stricken once, twice, thrice by shocks of suffering. She"
dali[4380] <- "had felt about him, that night in the café when he had come in with"
dali[4878] <- "blotting-paper with Littré's dictionary on top, sitting under the"
dali[4982] <- "about the entrée, was it really made at home? But it was the"
dali[5656] <- "still had a little Emily Brontë he had given her, and he was to"
dali <- replace_abbreviation(dali)
length(dali)

first_bite <- dali[1:2499]
second_bite<- dali[2500:4999]
third_bite <- dali[5000:5871]

dali.sents.first <- paste0(first_bite, collapse = "\n")
dali.sents.first <- unlist(tokenize_sentences(dali.sents.first))

dali.sents.second <- paste0(second_bite, collapse = "\n")
dali.sents.second <- unlist(tokenize_sentences(dali.sents.second))

dali.sents.third <- paste0(third_bite, collapse = "\n")
dali.sents.third <- unlist(tokenize_sentences(dali.sents.third))

dali.sents <- c(dali.sents.first, dali.sents.second, dali.sents.third)
length(dali.sents)

substr(dali.sents[594], nchar(dali.sents[594]), nchar(dali.sents[594]))
bad_spots <-c(0)
for(i in seq(1:length(dali.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(dali.sents[i], nchar(dali.sents[i]), nchar(dali.sents[i]))
  test2 <- substr(dali.sents[i+1], 1, 1)
  test3 <- substr(dali.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      dali.sents[i] <- paste(dali.sents[i], dali.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
dali.sents[bad_spots]
dali.sents <- dali.sents[-c(bad_spots)]
print(length(dali.sents))
bad_spots <-c(0)
for(i in seq(1:length(dali.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(dali.sents[i], nchar(dali.sents[i])-1, nchar(dali.sents[i]))
  test2 <- substr(dali.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      dali.sents[i] <- paste(dali.sents[i], dali.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
dali.sents[bad_spots]
dali.sents <- dali.sents[-c(bad_spots)]
print(length(dali.sents))
dali.sents.df <- as.data.frame(dali.sents, stringsAsFactors = FALSE)
dali.sents[1069] <- paste(dali.sents[1069], dali.sents[1070])
dali.sents[1070] <- ""
dali.sents <- dali.sents[dali.sents!=""]
print(length(dali.sents))

dali.title <- rep("mrsDalloway", 3405)
dali.sents.type <- rep("sentence", 3405)
dali.sents.counter<-seq(1, 3405)
dali.sents.id <- paste0("MRS_DALLOWAY_", "SENT_", dali.sents.counter)
dali.label <- rep("1", 3405)
print(length(dali.sents.id))
dali.sents.matrix <- cbind(dali.title, dali.sents.type, dali.sents.id, dali.sents, dali.label)
dali.sents.df <- as.data.frame(dali.sents.matrix, stringsAsFactors = FALSE)
colnames(dali.sents.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
# dbExecute(con, "DELETE FROM textTable WHERE Type='sentence' AND Title='mrsDalloway'")
dbWriteTable(con, "textTable", dali.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='mrsDalloway' LIMIT 2")
dbDisconnect(con)

#### words. 
stock <- c("Title", "Type", "ID", "Unit", "Label")
dali <- scan("rawTexts/lyrical/virginia-woolf-mrs-dalloway.txt",what="character",sep="\n")
dali.start <-which(dali == "Mrs. Dalloway said she would buy the flowers herself.")
dali.end <- which(dali == "For there she was.")
dali <- dali[dali.start:dali.end]
length(dali)
dali <- gsub('_', '', perl=TRUE, dali)
dali<-  gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, dali)
dali <- gsub('Mrs\\.', 'Mrs', perl=TRUE, dali)
dali <- gsub('Mr\\.', 'Mr', perl=TRUE, dali)
dali <- gsub('St\\.', 'St', perl=TRUE, dali)
dali <- gsub("t?te-?-t?tes", "tête-à-tête", perl=TRUE, dali)
dali <- gsub("caf?", "café", perl=TRUE,dali)
dali <- gsub('\"', '', perl=TRUE, dali)

# bad diacritics! 
dali[167] <- "knowledge Fräulein Daniels gave them she could not think. She knew"
dali[3266] <- "somehow right. So she let Hugh eat his soufflé; asked after poor"
dali[3935] <- "inches of a chocolate éclair."
dali[3938] <- "inches of the chocolate éclair, then wiped her fingers, and washed"
dali[3973] <- "éclairs, stricken once, twice, thrice by shocks of suffering. She"
dali[4380] <- "had felt about him, that night in the café when he had come in with"
dali[4878] <- "blotting-paper with Littré's dictionary on top, sitting under the"
dali[4982] <- "about the entrée, was it really made at home? But it was the"
dali[5656] <- "still had a little Emily Brontë he had given her, and he was to"
dali <- replace_abbreviation(dali)

# splits. 

dali.not.blanks <- which(dali != "")
dali <- dali[dali.not.blanks]

dali.temp <- dali
dali.temp <- paste(dali.temp, collapse=" ")
dali.temp <-tolower(dali.temp)
# a better regex that is going to maintain contractions. important! 

dali.temp <- unlist(strsplit(dali.temp, "[^\\w']", perl=TRUE))
dali.not.blanks <- which(dali.temp != "")
dali.words <- dali.temp[dali.not.blanks]

# data base commit 
dali.title <- rep("mrsDalloway", 64267)
dali.words.type <- rep("word", 64267)
dali.words.counter <- seq(1, 64267)
dali.words.id <- paste0("MRS_DALLOWAY_", "WORD_", dali.words.counter)
dali.label <- rep("1", 64267)
dali.words.matrix <- cbind(dali.title, dali.words.type, dali.words.id, dali.words, dali.label)

dali.words.df <- as.data.frame(dali.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(dali.words.df) <- c("Title", "Type", "ID", "Unit", 'Label')
dbWriteTable(con, "textTable", dali.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='mrsDalloway' LIMIT 10")

dbDisconnect(con)

### paras. 
dali.paragraphs <- read.csv("Python_Scripts/checkCorpus/DALI_paras.csv", stringsAsFactors = FALSE)
dali.paragraphs$X0[20]
# gotta sub \n for a space
dali.paragraphs <- dali.paragraphs[-c(1:18,773:777),]
# subs..
colnames(dali.paragraphs) <- c("arb", "paras")
dali.paragraphs <- dali.paragraphs %>%
  transmute(paragraph = gsub('Mrs\\.', 'Mrs', perl=TRUE, paras))

dali.paragraphs <- dali.paragraphs %>%
  transmute(paras = gsub('Mr\\.', 'Mr', perl=TRUE, paragraph))

dali.paragraphs <- dali.paragraphs %>%
  transmute(paragraph = gsub('St\\.', 'St', perl=TRUE, paras))

dali.paragraphs <- dali.paragraphs %>%
  transmute(paras = gsub('\n', ' ', perl=TRUE, paragraph))

bads <- grep("<", dali.paragraphs$paras)
dali.paragraphs$paras[bads[1]] <- gsub("Fr<e4>ulein", "Fräulein", dali.paragraphs$paras[bads[1]])
dali.paragraphs$paras[bads[2]] <- gsub("t<ea>te-<e0>-t<ea>tes", "tête-à-têtes", dali.paragraphs$paras[bads[2]])
dali.paragraphs$paras[bads[3]] <- gsub("souffl<e9>", "soufflé", dali.paragraphs$paras[bads[3]])
dali.paragraphs$paras[bads[4]] <- gsub("<e9>clair", "éclair", dali.paragraphs$paras[bads[4]])
dali.paragraphs$paras[bads[5]] <- gsub("<e9>clair", "éclair", dali.paragraphs$paras[bads[5]])
dali.paragraphs$paras[bads[6]] <- gsub("<e9>clair", "éclair", dali.paragraphs$paras[bads[6]])
dali.paragraphs$paras[bads[7]] <- gsub("caf?", "café", dali.paragraphs$paras[bads[7]])
dali.paragraphs$paras[bads[8]] <- gsub('Littr<e9>', 'Littré', dali.paragraphs$paras[bads[8]])
dali.paragraphs$paras[bads[9]] <- gsub('entr<e9>e', 'entrée', dali.paragraphs$paras[bads[9]])
dali.paragraphs$paras[bads[10]] <- gsub('Bront<eb>', "Brontë", dali.paragraphs$paras[bads[10]])

dali.paragraphs <- dali.paragraphs %>%
  transmute(paragraph = gsub('St\\.', 'St', perl=TRUE, paras))

dali.paragraphs <- dali.paragraphs %>%
  transmute(paras = gsub('\n', ' ', perl=TRUE, paragraph))

dali.paragraphs <- dali.paragraphs %>%
  transmute(paragraph = gsub('  ', ' ', perl=TRUE, paras))

dali.paragraphs <- dali.paragraphs %>%
  transmute(paras = replace_abbreviation(paragraph))
print(length(dali.paragraphs$paras))


dali.title <- rep("mrsDalloway", 754)
dali.para.type <- rep("paragraph", 754)
dali.para.counter<-seq(1, 754)
dali.para.id <- paste0("MRS_DALLOWAY_", "PARAGRAPH_", dali.para.counter)
dali.label <- rep("1", 754)
print(length(dali.para.id))
dali.para.matrix <- cbind(dali.title, dali.para.type, dali.para.id, dali.paragraphs, dali.label)
dali.para.df <- as.data.frame(dali.para.matrix, stringsAsFactors = FALSE)
colnames(dali.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", dali.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='mrsDalloway' LIMIT 2")
dbDisconnect(con)
## DALI SQUEAKY CLEAN.








