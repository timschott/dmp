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
# getting rid of the Index because there's not much intentional prosody there.
# there are of course important facts for the PLOT but hey I'm a formalist.. take it ez.
fire <- fire[-c(2334:2448)]
print(length(fire))

print(fire[5])
print(fire[6])
print(fire[7])
print(fire[8])
print(fire[9])

### sew up: lowercase_ --> lowercase. #
### or lowercase- --> lowercase. 
#### or lowercase: ---> lowercase.
print(nchar(fire[5]))
test<-substr(fire[5], nchar(fire[5]), nchar(fire[5]))
test %in% c("_ ", "- ", ': ')
substr(fire[6],1,1)

## Isolate the poem first... i want to keep it, but maybe as a separate document....
## just per line, count it as a "sentence" but Lets do that separate from the rest.
poem <- fire[46:1124]
fire <- fire[-c(46:1124)]
print(length(fire))
print(fire[1:50])

## loop through fire.. deal with the above issues. 
## also going to want to remove blank lines.
# x[x != ""] 
grep('_',perl=TRUE, fire)
# patchitino


bad_spots <-c(0)
for(i in seq(1:length(fire))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(fire[i], nchar(fire[i]), nchar(fire[i]))
  test2 <- substr(fire[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("_", "-", ':'," ") && test2==tolower(test2)){
      fire[i] <- paste(fire[i], fire[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
fire <- fire[-c(bad_spots)]
print(length(fire))
grep("Dr.", fire)
### 10 22
fire[1153:1155]
# manually hotfixes
fire[9] <- paste0(fire[9], fire[10])
fire[10] <- paste0(fire[1146], fire[1147])
fire[1153] <- paste0(fire[1153], fire[1154], fire[1155])
fire <- fire[-c(10, 1147, 1154, 1155)]

# once more. 
bad<-c(0)

## scrub blanks. ## ensure it's a LETTER or a - or a ( or " " or a [ or a .
## maybe say that it should NOT be in c(100:999)
for(i in seq(1:length(fire))){
  test <- substr(fire[i], 1, 1)
  if(test == tolower(test) && test%in%c(LETTERS, letters, " ", "-", "(", "[", ".")){
    fire[i-1] <- paste(fire[i-1], fire[i])
    bad<-append(bad, i)
  }
}
fire[bad]
fire <- fire[-bad]

print(length(fire))

# okay next, remove blanks
fire <- fire[fire != ""]
print(length(fire))

fileConn <- file("test.txt")    
writeLines(fire, fileConn)

# get rid of ..
#PALE FIRE
#A Poem in Four Cantos
#Canto One



# paste
# 40 and The sun with stolen ice, the moon with leaves
# The next masquerader will be shot at sight. What's your real name, Charlie? I'm British. I'm a tourist, said
#theKing. Well, anyway, take off that red fufa. And the cap. Give them here. He tossed the things in the back of the car and drove off.
# to lines 286 and 408).
# Migraine again worse today.

# separate
#Sun: Ground meat \\ (All it got from me was milk and sardines
# Verbálala wod gév ut trí phantána \\(I have marked the stress accents).
# answered.  \\ Bon soir, Sybil.