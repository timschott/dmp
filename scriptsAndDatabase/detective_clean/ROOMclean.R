setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

# just do the paragraphs first then distill down. 

room.paragraphs <- read.csv("Python_Scripts/checkCorpus/75_paras.csv", stringsAsFactors = FALSE)
room.paragraphs$X0[20]
room.paragraphs <- room.paragraphs[-c(1:15,774:777),]
colnames(room.paragraphs) <- c("arb", "paragraphs")

room.paragraphs <- room.paragraphs %>% 
  transmute(paras=  gsub("\n", " ", paragraphs) )

room.paragraphs<- room.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

room.paragraphs <- room.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

room.paragraphs <- room.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

room.paragraphs <- room.paragraphs %>% 
  transmute(paras=  gsub("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs) )

room.paragraphs <- room.paragraphs %>% 
  filter(paras!="")

room.paragraphs <- room.paragraphs %>% 
  filter(paras!="  ")
colnames(room.paragraphs)

room.paragraphs <- room.paragraphs %>% 
  transmute(paragraphs = replace_abbreviation(paras))

room.paragraphs <- room.paragraphs %>% 
  transmute(paras=  gsub("MR\\.", "Mr", paragraphs))

print(length(room.paragraphs$paras))

room.paragraphs <- room.paragraphs %>% 
  filter(paras!="")

room.paragraphs <- room.paragraphs %>% 
  filter(paras!="  ")
room.paragraphs <- room.paragraphs %>% 
  transmute(paragraphs=  gsub("_", "", paras))
print(length(room.paragraphs$paragraphs))
room.paragraphs$paragraphs[555:568]

print(length(room.paragraphs$paragraphs))

room.title <- rep("theMysteryOfRoom75", 758)
room.para.type <- rep("paragraph",758)
room.para.counter<-seq(1, 758)
room.para.id <- paste0("THE_MYSTERY_OF_ROOM_75_", "PARAGRAPH_", room.para.counter)
room.label <- rep("0", 758)
print(length(room.para.id))

room.para.matrix <- cbind(room.title, room.para.type, room.para.id, room.paragraphs, room.label)
room.para.df <- as.data.frame(room.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(room.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", room.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theMysteryOfRoom75' LIMIT 2")
dbDisconnect(con)

room <- room.paragraphs$paragraphs

# sents 

first_bite <- room[1:758]

room.sents.first <- paste0(first_bite, collapse = "\n")
room.sents.first <- unlist(tokenize_sentences(room.sents.first))

room.sents <- c(room.sents.first)
room.sents.df <- as.data.frame(room.sents, stringsAsFactors = FALSE)

print(length(room.sents.df$room.sents))
