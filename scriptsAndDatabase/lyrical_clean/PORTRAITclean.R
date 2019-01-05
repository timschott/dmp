setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

stock <- c("Title", "Type", "ID", "Unit", "Label")

portrait <- scan("rawTexts/lyrical/james-joyce-portrait-of-the-artist.txt",what="character",sep="\n")

portrait.start<- which(portrait == "Once upon a time and a very good time it was there was a moocow coming")
portrait.end <- which(portrait == "stead.")

portrait<- portrait[portrait.start: portrait.end]
portrait <- replace_abbreviation(portrait)
portrait <- gsub('_', '', perl=TRUE, portrait)

portrait <- gsub('Chapter.X{0,3}(IX|IV|V?I{0,3}).', '', perl=TRUE, portrait)

portrait.not.blanks <- which(portrait != "")
portrait <- portrait[portrait.not.blanks]

length(portrait)

first_bite <- portrait[1:2499]
second_bite<- portrait[2500:4999]
third_bite <- portrait[5000:7700]

portrait.sents.first <- paste0(first_bite, collapse = "\n")
portrait.sents.first <- unlist(tokenize_sentences(portrait.sents.first))

portrait.sents.second <- paste0(second_bite, collapse = "\n")
portrait.sents.second <- unlist(tokenize_sentences(portrait.sents.second))

portrait.sents.third <- paste0(third_bite, collapse = "\n")
portrait.sents.third <- unlist(tokenize_sentences(portrait.sents.third))

portrait.sents <- c(portrait.sents.first, portrait.sents.second, portrait.sents.third)

portrait.sents.df <- as.data.frame(portrait.sents, stringsAsFactors = FALSE)

bad_spots<-c(0)
substr(portrait.sents[750], nchar(portrait.sents[750]), nchar(portrait.sents[750]))

for(i in seq(1:length(portrait.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(portrait.sents[i], nchar(portrait.sents[i]), nchar(portrait.sents[i]))
  test2 <- substr(portrait.sents[i+1], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2)){
    portrait.sents[i] <- paste(portrait.sents[i], portrait.sents[i+1])
    # print(portrait.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}

portrait.sents <- portrait.sents[-bad_spots]

print(length(portrait.sents))

portrait.title <- rep("portraitOfTheArtist", 4452)
portrait.sents.type <- rep("sentence", 4452)
portrait.sents.counter<-seq(1, 4452)
portrait.sents.id <- paste0("PORTRAIT_OF_THE_ARTIST_", "SENT_", portrait.sents.counter)
print(length(portrait.sents.id))
portrait.sents.matrix <- cbind(portrait.title, portrait.sents.type, portrait.sents.id, portrait.sents)
portrait.sents.df <- as.data.frame(portrait.sents.matrix, stringsAsFactors = FALSE)
colnames(portrait.sents.df) <- stock
# okay i think it's good now.
# portrait To Do: press into sents into DB; 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", portrait.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='portraitOfTheArtist' LIMIT 2")
dbDisconnect(con)

#### words ####

stock <- c("Title", "Type", "ID", "Unit")

portrait <- scan("rawTexts/james-joyce-portrait-of-the-artist.txt",what="character",sep="\n")

portrait.start<- which(portrait == "Once upon a time and a very good time it was there was a moocow coming")
portrait.end <- which(portrait == "stead.")

portrait<- portrait[portrait.start: portrait.end]
portrait <- replace_abbreviation(portrait)
portrait <- gsub('_', '', perl=TRUE, portrait)

portrait <- gsub('Chapter.X{0,3}(IX|IV|V?I{0,3}).', '', perl=TRUE, portrait)

portrait.not.blanks <- which(portrait != "")
portrait <- portrait[portrait.not.blanks]


portrait.temp <- portrait
portrait.temp <- paste(portrait.temp, collapse=" ")
portrait.temp <-tolower(portrait.temp)
# a better regex that is going to maintain contractions. important! 
portrait.temp <- unlist(strsplit(portrait.temp, "[^\\w’]", perl=T))
portrait.not.blanks <- which(portrait.temp != "")
portrait.words <- portrait.temp[portrait.not.blanks]

# lots of words! 
print(length(portrait.words))

portrait.title <- rep("portraitOfTheArtist", 84927)
portrait.words.type <- rep("word", 84927)
portrait.words.counter <- seq(1, 84927)
portrait.words.id <- paste0("PORTRAIT_OF_THE_ARTIST", "WORD_", portrait.words.counter)

portrait.words.matrix <- cbind(portrait.title, portrait.words.type, portrait.words.id, portrait.words)

portrait.words.df <- as.data.frame(portrait.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(portrait.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", portrait.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='portraitOfTheArtist' LIMIT 10")
dbDisconnect(con)

####### Paragraphs ###### 

portrait.paragraphs <- read.csv("Python_Scripts/checkCorpus/PORTRAIT_paras.csv", stringsAsFactors = FALSE)

portrait.paragraphs <- portrait.paragraphs[-c(1:19, 2247:2302),]
colnames(portrait.paragraphs) <- c("arb", "paras")

# cleanzo

portrait.paragraphs <- portrait.paragraphs %>%
  transmute(paragraph = gsub('CHAPTER.X{0,3}(IX|IV|V?I{0,3}).|_', '', perl=TRUE, paras))

portrait.paragraphs <- portrait.paragraphs %>%
  transmute(paras = gsub('\n', ' ', perl=TRUE, paragraph))
portrait.paragraphs$paras[1773:1775]

# join stephen's villanelle

portrait.paragraphs$paras[1773] <- paste(portrait.paragraphs$paras[1773], portrait.paragraphs$paras[1774], portrait.paragraphs$paras[1775])
portrait.paragraphs$paras[1774:1775] <-""

grep("ardent ways", portrait.paragraphs$paras)
portrait.paragraphs$paras[1777:1779]
portrait.paragraphs$paras[1777] <- paste(portrait.paragraphs$paras[1777], portrait.paragraphs$paras[1778], portrait.paragraphs$paras[1779])
portrait.paragraphs$paras[1778:1779] <- ""

portrait.paragraphs$paras[1804:1809]
portrait.paragraphs$paras[1804] <- paste(portrait.paragraphs$paras[1804], portrait.paragraphs$paras[1805], portrait.paragraphs$paras[1806], portrait.paragraphs$paras[1807], portrait.paragraphs$paras[1808], portrait.paragraphs$paras[1809], portrait.paragraphs$paras[1810])
portrait.paragraphs$paras[1805:1810] <- ""

portrait.paragraphs$paras[1804] <- "Our broken cries and mournful lays Rise in one eucharistic hymn Are you not weary of ardent ways?"
portrait.paragraphs$paras[1805] <- "While sacrificing hands upraise The chalice flowing to the brim Tell no more of enchanted days."
portrait.paragraphs$paras[1806] <- "He spoke the verses aloud from the first lines till the music and rhythm suffused his mind, turning it to quiet indulgence; then copied them painfully to feel them the better by seeing them; then lay back on his bolster."

# portrait.paragraphs$paras[1819] <- paste(portrait.paragraphs$paras[1819], )

portrait.paragraphs$paras[1819] <- paste(portrait.paragraphs$paras[1819], portrait.paragraphs$paras[1820], portrait.paragraphs$paras[1821])
portrait.paragraphs$paras[1820:1821] <- ""

portrait.paragraphs$paras[1822] <- paste(portrait.paragraphs$paras[1822], portrait.paragraphs$paras[1823], portrait.paragraphs$paras[1824])
portrait.paragraphs$paras[1823:1824] <- ""

portrait.paragraphs$paras[1825] <- paste(portrait.paragraphs$paras[1825],portrait.paragraphs$paras[1826], portrait.paragraphs$paras[1827])
portrait.paragraphs$paras[1826:1827] <-""

portrait.paragraphs$paras[1828] <- paste(portrait.paragraphs$paras[1828], portrait.paragraphs$paras[1829], portrait.paragraphs$paras[1830])
portrait.paragraphs$paras[1829:1830] <-""

portrait.paragraphs$paras[1831] <- paste(portrait.paragraphs$paras[1831], portrait.paragraphs$paras[1832], portrait.paragraphs$paras[1833])
portrait.paragraphs$paras[1832:1833] <- ""

portrait.paragraphs$paras[1834] <- paste(portrait.paragraphs$paras[1834], portrait.paragraphs$paras[1835], portrait.paragraphs$paras[1836], portrait.paragraphs$paras[1837])
portrait.paragraphs$paras[1835:1837] <- ""

portrait.paragraphs <- portrait.paragraphs %>% filter(paras!="")

portrait.title <- rep("portraitOfTheArtist", 2208)
portrait.para.type <- rep("paragraph", 2208)
portrait.para.counter<-seq(1, 2208)
portrait.label <- rep("1", 2208)
portrait.para.id <- paste0("PORTRAIT_OF_THE_ARTIST_", "PARAGRAPH_", portrait.para.counter)
print(length(portrait.para.id))
portrait.para.matrix <- cbind(portrait.title, portrait.para.type, portrait.para.id, portrait.paragraphs, portrait.label)
portrait.para.df <- as.data.frame(portrait.para.matrix, stringsAsFactors = FALSE)
colnames(portrait.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", portrait.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='portraitOfTheArtist' LIMIT 2")
dbDisconnect(con)

# portrait done. 


