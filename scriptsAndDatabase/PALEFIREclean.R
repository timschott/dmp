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
# nab.
stock <- c("Title", "Type", "ID", "Unit")
fire <- scan("rawTexts/vladimir-nabokov-pale-fire.txt",what="character", sep="\n")
grep('http://www.en8848.com.cn/『原版英语』', fire)
fire <- gsub('http://www.en8848.com.cn/『原版英语』', '', fire)
fire <- gsub('\"', '', fire)
fire <- fire[-c(1:13)]
fire <- fire[-c(2334:2448)]
print(length(fire))

print(fire[5])
print(fire[6])
print(fire[7])
print(fire[8])
print(fire[9])

### sew up: lowercase_ --> lowercase. 
print(nchar(fire[5]))
substr(fire[5], nchar(fire[5]), nchar(fire[5]))
substr(fire[6],1,1)

## Isolate the poem first... i want to keep it, but maybe as a separate document....
## just per line, count it as a "sentence" but Lets do that separate from the rest.


grep("I was the shadow of the waxwing slain", fire)
