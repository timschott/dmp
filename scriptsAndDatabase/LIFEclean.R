setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)
# library(rJava)
# library("openNLPdata")
# paras first. like pale fire. 

stock <- c("Title", "Type", "ID", "Unit")
life <- scan("rawTexts/j-m-coetzee-michael-k.txt",what="character", sep="\n")
life.end<- which(life=="that they recognized who it was.")
life <- life[5:785]

