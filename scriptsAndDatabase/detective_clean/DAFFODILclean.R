setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

daff <- scan("rawTexts/detective/edgar-wallace-the-daffodil-mystery.txt",what="character",sep="\n")
daff.start <- which(daff=="\"I am afraid I don't understand you, Mr. Lyne.\"")
daff.end <- which(daff=="\"Because,\" said Tarling, \"I was reading an article on horticulture in this morning's papers and I learnt that daffodils do not grow in the Argentine.\"")
daff<-daff[daff.start:daff.end]
print(length(daff))

spots <- grep('[A-Z]{2,}[^a-z]', daff)
daff[spots]
daff <- daff[-c(spots[-c(47)])]

daff.paragraphs <- as.data.frame(daff, stringsAsFactors=FALSE)
colnames(daff.paragraphs) <- c("paras")

daff.paragraphs <- daff.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

daff.paragraphs <- daff.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs))

daff.paragraphs <- daff.paragraphs %>% 
  transmute(paragraphs=  gsub("\"", "", paras))

daff.paragraphs <- daff.paragraphs %>% 
  transmute(paras=  gsub("\\*", "", paragraphs))

daff.paragraphs <- daff.paragraphs %>% 
  transmute(paragraphs = gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '',perl=TRUE, paras))

daff.paragraphs <- daff.paragraphs %>% 
  filter(paragraphs!="")

daff.paragraphs <- daff.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(daff.paragraphs$paragraphs))

daff.title <- rep("theDaffodilMystery", 2300)
daff.para.type <- rep("paragraph",2300)
daff.para.counter<-seq(1, 2300)
daff.para.id <- paste0("THE_DAFFODIL_MYSTERY_", "PARAGRAPH_", daff.para.counter)
daff.label <- rep("0", 2300)
print(length(daff.para.id))

daff.para.matrix <- cbind(daff.title, daff.para.type, daff.para.id, daff.paragraphs, daff.label)
daff.para.df <- as.data.frame(daff.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(daff.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", daff.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theDaffodilMystery' LIMIT 2")
dbDisconnect(con)

# sents. 

