setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

hand <- scan("rawTexts/detective/arthur-j-rees-the-hand-in-the-dark.txt",what="character",sep="\n")
hand.start <- which(hand=="Seen in the sad glamour of an English twilight, the old moat-house, emerging from the thin mists which veiled the green flats in which it stood, conveyed the impression of a habitation falling into senility, tired with centuries of existence. Houses grow old like the race of men; the process is not less inevitable, though slower; in both, decay is hastened by events as well as by the passage of Time.")
hand.fin <- which(hand =="But Colwyn felt that it would not be so. As he turned from the room, leaving the living and the dead together, he knew that when the first bitterness of the shock was over, and she was faced again with the consciousness of duty, she would call on her abiding faith to help her to wear, without flinching, the heavy grey garment of life.")
hand<-hand[hand.start:hand.fin]

hand.paragraphs <- as.data.frame(hand, stringsAsFactors=FALSE)
colnames(hand.paragraphs) <- c("paras")

hand.paragraphs <- hand.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

hand.paragraphs <- hand.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs))

hand.paragraphs <- hand.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "", paras))

hand.paragraphs <- hand.paragraphs %>% 
  transmute(paras=  gsub("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs))

hand.paragraphs <- hand.paragraphs %>% 
  transmute(paragraphs=  gsub("CHAPTER I", "", paras))

hand.paragraphs <- hand.paragraphs %>% 
  transmute(paras=  gsub("\\*", "", paragraphs))

hand.paragraphs <- hand.paragraphs %>% 
  transmute(paragraphs = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paras))

hand.paragraphs <- hand.paragraphs %>% 
  filter(paragraphs!="")

hand.paragraphs <- hand.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(hand.paragraphs$paragraphs))

hand.title <- rep("theHandInTheDark", 2205)
hand.para.type <- rep("paragraph",2205)
hand.para.counter<-seq(1, 2205)
hand.para.id <- paste0("THE_HAND_IN_THE_DARK_", "PARAGRAPH_", hand.para.counter)
hand.label <- rep("0", 2205)
print(length(hand.para.id))

hand.para.matrix <- cbind(hand.title, hand.para.type, hand.para.id, hand.paragraphs, hand.label)
hand.para.df <- as.data.frame(hand.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(hand.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", hand.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theHandInTheDark' LIMIT 2")
dbDisconnect(con)

# sents 
hand <- hand.paragraphs$paragraphs

first_bite <- hand[1:2205]

hand.sents.first <- paste0(first_bite, collapse = "\n")
hand.sents.first <- unlist(tokenize_sentences(hand.sents.first))

hand.sents <- c(hand.sents.first)
hand.sents.df <- as.data.frame(hand.sents, stringsAsFactors = FALSE)

print(length(hand.sents.df$hand.sents))

bad_spots <-c(0)
for(i in seq(1:length(hand.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a cahandal letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(hand.sents[i], nchar(hand.sents[i]), nchar(hand.sents[i]))
  test2 <- substr(hand.sents[i+1], 1, 1)
  test3 <- substr(hand.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      hand.sents[i] <- paste(hand.sents[i], hand.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
hand.sents[bad_spots]
hand.sents <- hand.sents[-c(bad_spots)]

hand.sents.df <- as.data.frame(hand.sents, stringsAsFactors = FALSE)
hand.sents <- hand.sents[hand.sents!=""]
print(length(hand.sents))


