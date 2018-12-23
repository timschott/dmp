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

# eureaka, poe. 
stock <- c("Title", "Type", "ID", "Unit")
poe <- scan("rawTexts/edgar-allen-poe-eureka.txt",what="character",sep="\n")

poe.start <- 6
poe.end <- 269
poe <- poe[poe.start:poe.end]

poe <- gsub('_', '', perl=TRUE, poe)
poe<-  gsub('(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)', '', perl=TRUE, poe)
poe <- gsub('\"', '', perl=TRUE, poe)

length(poe)


first_bite <- poe[1:264]

poe.sents.first <- paste0(first_bite, collapse = "\n")
poe.sents.first <- unlist(tokenize_sentences(poe.sents.first))

poe.sents <- c(poe.sents.first)

poe.sents <- poe.sents[-c(1:4)]
poe.sents <- poe.sents[-c(1)]
poe.sents <- gsub("EUREKA: AN ESSAY ON THE MATERIAL AND SPIRITUAL UNIVERSE IT", "It", poe.sents)













