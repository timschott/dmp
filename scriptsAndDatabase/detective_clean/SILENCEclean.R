setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

silence <- scan("rawTexts/detective/harrington-strong-brand-of-silence.txt",what="character",sep="\n")
silence.start <- which(silence =="Now the fog was clearing and the mist was lifting, and the bright sunshine was struggling to penetrate the billows of damp vapor and touch with its glory the things of the world beneath. In the lower harbor there still was a chorus of sirens and foghorns, as craft of almost every description made way toward the metropolis or out toward the open sea.")
silence.fin <- which(silence =="THE END")
silence <- silence[silence.start:silence.fin-1]

spots <- grep('[A-Z]{2,}[^a-z]', silence)
silence[spots]
silence <- silence[-spots]

silence.paragraphs <- as.data.frame(silence, stringsAsFactors=FALSE)
colnames(silence.paragraphs) <- c("paras")

silence.paragraphs<- silence.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

silence.paragraphs <- silence.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

silence.paragraphs <- silence.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

silence.paragraphs <- silence.paragraphs %>% 
  filter(paragraphs!="")

silence.paragraphs <- silence.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(silence.paragraphs)

silence.paragraphs <- silence.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

silence.paragraphs <- silence.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(silence.paragraphs$paragraphs))

silence.paragraphs <- silence.paragraphs %>% 
  filter(paragraphs!="")

silence.paragraphs <- silence.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(silence.paragraphs$paragraphs))

silence.title <- rep("theBrandOfSilence", 2067)
silence.para.type <- rep("paragraph",2067)
silence.para.counter<-seq(1, 2067)
silence.para.id <- paste0("THE_BRAND_OF_SILENCE_", "PARAGRAPH_", silence.para.counter)
silence.label <- rep("0", 2067)
print(length(silence.para.id))

silence.para.matrix <- cbind(silence.title, silence.para.type, silence.para.id, silence.paragraphs, silence.label)
silence.para.df <- as.data.frame(silence.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(silence.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", silence.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theBrandOfSilence' LIMIT 2")
dbDisconnect(con)

# sents.


