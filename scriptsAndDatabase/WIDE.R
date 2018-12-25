setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)


stock <- c("Title", "Type", "ID", "Unit")
sea <- scan("rawTexts/Jean-Rhys-Wide-Sargasso-Sea.txt",what="character", sep="\n")
sea.start <- which(sea=="They say when trouble come close ranks, and so the white people did. But we were not in their ranks. The Jamaican ladies had never approved of my mother, ‘because she pretty like pretty self’ Christophine said.")
sea.end<- which(sea=="Grace Poole was sitting at the table but she had heard the scream too, for she said, ‘What was that?’ She got up, came over and looked at me. I lay still, breathing evenly with my eyes shut. ‘I must have been dreaming,’ she said. Then she went back, not to the table but to her bed. I waited a long time after I heard her snore, then I got up, took the keys and unlocked the door. I was outside holding my candle. Now at last I know why I was brought here and what I have to do. There must have been a draught for the flame flickered and I thought it was out. But I shielded it with my hand and it burned up again to light me along the dark passage.")
sea <- sea[sea.start:sea.end]
print(length(sea))
grep("Part Two", sea)
grep("Part three", sea)
sea <- sea[-c(254, 1111)]
sea <- gsub("Mr.", "Mr", sea)
sea <- gsub("Mrs.", "Mrs", sea)
print(length(sea))
sea <- sea[sea!=""]
print(length(sea))
sea.paragraphs <- sea
sea.title <- rep("wideSargassoSea", 1175)
sea.para.type <- rep("paragraph", 1175)
sea.para.counter<-seq(1, 1175)
sea.para.id <- paste0("WIDE_SARGASSO_SEA_", "PARAGRAPH_", sea.para.counter)
print(length(sea.para.id))

sea.para.matrix <- cbind(sea.title, sea.para.type, sea.para.id, sea.paragraphs)
sea.para.df <- as.data.frame(sea.para.matrix, stringsAsFactors = FALSE)
colnames(sea.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", sea.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='wideSargassoSea' LIMIT 2")
dbDisconnect(con)

## sents.






