setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

pit <- scan("rawTexts/detective/arthur-j-rees-the-shrieking-pit.txt",what="character",sep="\n")
pit.start <- which(pit=="Colwyn had never seen anything quite so eccentric in a public room as the behaviour of the young man breakfasting alone at the alcove table in the bay embrasure, and he became so absorbed in watching him that he permitted his own meal to grow cold, impatiently waving away the waiter who sought with obtrusive obsequiousness to recall his wandering attention by thrusting the menu card before him.")
pit.end <- 1878
pit<-pit[pit.start:pit.end]

pit.paragraphs <- as.data.frame(pit, stringsAsFactors=FALSE)
colnames(pit.paragraphs) <- c("paras")

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr.", "Mr", paras))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paras=  gsub("Mrs.", "Mr", paragraphs))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "", paras))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paras=  gsub("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paragraphs=  gsub("CHAPTER I", "", paras))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paras=  gsub("\\*", "", paragraphs))

pit.paragraphs <- pit.paragraphs %>% 
  transmute(paragraphs = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paras))

pit.paragraphs <- pit.paragraphs %>% 
  filter(paragraphs!="")

pit.paragraphs <- pit.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(pit.paragraphs$paragraphs))

# database time. 
pit.title <- rep("theShriekingPit", 1740)
pit.para.type <- rep("paragraph",1740)
pit.para.counter<-seq(1, 1740)
pit.para.id <- paste0("THE_SHRIEKING_PIT", "PARAGRAPH_", pit.para.counter)
pit.label <- rep("0", 1740)
print(length(pit.para.id))

pit.para.matrix <- cbind(pit.title, pit.para.type, pit.para.id, pit.paragraphs, pit.label)
pit.para.df <- as.data.frame(pit.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(pit.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", pit.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theShriekingPit' LIMIT 2")
dbDisconnect(con)

# sents 




