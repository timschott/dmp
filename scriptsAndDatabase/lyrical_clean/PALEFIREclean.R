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
stock <- c("Title", "Type", "ID", "Unit", "Label")
fire <- scan("rawTexts/lyrical/vladimir-nabokov-pale-fire.txt",what="character", sep="\n")
grep('http://www.en8848.com.cn/『原版英语』', fire)
fire <- gsub('http://www.en8848.com.cn/『原版英语』', '', fire)
fire <- gsub('\"', "'", fire)
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
# fire[9] <- paste0(fire[9], fire[10])
# fire[10] <- paste0(fire[1146], fire[1147])
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

grep("\\* \\* \\*", perl=TRUE, fire)
fire[c(494,550)] <- ""
fire[494] <- c("pada ata lane pad not ogo old wart alan ther tale feur far rant lant tal told")
fire <- gsub("  ", " ", fire) # do this again.
grep("PALE FIRE", fire)
fire[33:35] <- ""
# get rid of ..
# replace
# * * *
# for
# pada ata lane pad not ogo old wart alan ther tale feur far rant lant tal told
# (first time) DONE
# "" second time DONE
# "  " for " " DONE

# 698 DONE. 
fire[698] <- "Upon referring to my little diary, I see that during the five-month period of my intercourse with the Shades I was invited to their table exactly three times. Initiation took place on Saturday, March the 14th, when I dined at their house with the following people: Nattochdag (whom I saw every day in his office); Professor Gordon of the Music Department (who completely dominated the conversation); the Head of the Russian Department (a farcical pedant of whom the less said the better); and three or four interchangeable women (of whom one - Mrs. Gordon, I think) was enceinte, and another, a perfect stranger, steadily talked to me, or rather into me, from eight to eleven owing to an unfortunate afterdinner distribution of available seats. My next treat, a smaller but by no means cozier souper on Saturday, May 23, was attended by Milton Stone (a new librarian, with whom Shade discussed till midnight the classification of certain Wordsmithiana); good old Nattochdag (whom I continued to see every day); and an undeodorized Frenchwoman (who gave me a complete picture of language-teaching conditions at the University of California). The date of my third and last meal at the Shades is not entered in my little book but I know it was one morning in June when I brought over a beautiful plan I had drawn of the King's Palace in Onhava with all sorts of heraldic niceties, and a touch of gold paint that I had some trouble in obtaining, and was graciously urged to stay for an impromptu lunch. I should add that, despite my protests, at all three meals my vegetarian limitations of fare were not taken into account, and I was exposed to animal matter in, or around, some of the contaminated greens I might have deigned to taste. I revanched myself rather neatly. Of a dozen or so invitations that I extended, the Shades accepted just three. Every one of these meals was built around some vegetable that I subjected to as many exquisite metamorphoses as Parmentier had his pet tuber undergo. Every time I had but one additional guest to entertain Mrs. Shade (who, if you please - thinning my voice to a feminine pitch - was allergic to artichokes, avocado pears, African acorns - in fact to everything beginning with an 'a'). I find nothing more conducive to the blunting of one's appetite than to have none but elderly persons sitting around one at table, fouling their napkins with the disintegration of their make-up, and surreptitiously trying, behind noncommittal smiles, to dislodge the red-hot torture point of a raspberry seed from between false gum and dead gum. So I had young people, students: the first time, the son of a padishah; the second time, my gardener; and the third time, that girl in the black leotard, with that long white face and eyelids painted a ghoulish green; but she came very late, and the Shades left very early - in fact, I doubt if the confrontation lasted more than ten minutes, whereupon I had the task of entertaining the young lady with phonograph records far into the night when at last she rang up somebody to accompany her to a 'diner' in Dulwich."
# okay pretty okay.


# paste
# 40 and The sun with stolen ice, the moon with leaves DONE
fire[64] <- paste(fire[64], fire[65])
fire[65] <- ""

# The next masquerader will be shot at sight. What's your real name, Charlie? I'm British. I'm a tourist, said
#theKing. Well, anyway, take off that red fufa. And the cap. Give them here. He tossed the things in the back of the car and drove off. DONE
fire[303] <- paste(fire[303], fire[304])
fire[304] <- ""

# to lines 286 and 408).
# Migraine again worse today. ## DONE

fire[350] <- paste(fire[350], fire[351])
fire[351] <- ""
#10:23. #Scrappy and scrabbly sounds. DONE
fire[487] <- paste(fire[487], fire[488],sep=" ")
fire[488] <- ""
#10:25.
it <- c('A roundlet of pale light, the size of a small doily; flitted across the dark walls, the boarded windows, and the floor; changed its place; lingered here and there, dancing up and down; seemed to wait in teasing play for evadable pounce. Gone.')
fire[489] <- paste(fire[489], it, sep=" ")
fire[490] <- ""
# done 
#10:37. SEW TO #Back again. 
#DONE
fire[491] <- paste(fire[491], fire[492], sep=" ")
fire[492]<- ""
# (whom they spelled SEW Khrushchev) DONE
fire[962] <-paste(fire[962], fire[963], sep=" ")
fire[963] <-""
# separate
#Sun: Ground meat \\ (All it got from me was milk and sardines
# make a first part, second part, and third, and then merge them. 
# done
length(fire)
first <- fire[1:79]
second <- "Sun: Ground meat"
third <- " (All it got from me was milk and sardines; it was a likable little creature but after a while its movements began to grate on my nerves and I farmed it out to Mrs. Finley, the cleaning woman.) But perhaps the funniest note concerned the manipulations of the window curtains which had to be drawn in different ways at different hours to prevent the sun from getting at the upholstery. A description of the position of the sun, daily and seasonal, was given for the several windows, and if I had heeded all this I would have been kept as busy as a participant in a regatta. A footnote, however, generously suggested that instead of manning the curtains, I might prefer to shift and reshift out of sun range the more precious pieces of furniture (two embroidered armchairs and a heavy royal console) but should do it carefully lest I scratch the wall moldings. I cannot, alas, reproduce the meticulous schedule of these transposals but seem to recall that I was supposed to castle the long way before going to bed and the short way first thing in the morning. My dear Shade roared with laughter when I led him on a tour of inspection and had him find some of those bunny eggs for himself. Thank God, his robust hilarity dissipated the atmosphere of damnum infectum in which I was supposed to dwell. On his part, he regaled me with a number of anecdotes concerning the judge's dry wit and courtroom mannerisms; most of these anecdotes were doubtless folklore exaggerations, a few were evident inventions, and all were harmless. He did not bring up, my sweet old friend never did, ridiculous stories about the terrifying shadows that Judge Goldsworth's gown threw across the underworld, or about this or that beast lying in prison and positively dying of raghdirst (thirst for revenge) - crass banalities circulated by the scurrilous and the heartless - by all those for whom romance, remoteness, sealskin-lined scarlet skies, the darkening dunes of a fabulous kingdom, simply do not exist. But enough of this."
fourth <- fire[80:1069]
fire <- c(first, second, third, fourth)
# Verbálala wod gév ut trí phantána \\(I have marked the stress accents).
# done.
first <- fire[1:172]
second <- "Verbálala wod gév ut trí phantána"
third <- "(I have marked the stress accents)."
fourth <-fire[174:1069]
fire <- c(first, second, third, fourth)
# answered.  \\ Bon soir, Sybil.
# done.

# THE HAUNTED BARN \\ Pitch-darkness. Father, Mother and Daughter are heard breathing gently in different corners. Three minutes pass.
first <- fire[1:506]
second <- "The minutes of that third session in the barn have not been preserved but I offer the reader the following scene which I feel cannot be too far removed from the truth:"
third <- "THE HAUNTED BARN"
fourth <-fire[508:1069]
fire <- c(first, second, third, fourth)

# get rid of blank lines.
fire <- fire[fire!=""]
fire <- gsub("  ", " ", fire) # do this again.


print(length(fire))
fire[c(1, 150)] <- ""
fire <- fire[fire!=""]
print(length(fire))

fileConn <- file("test.txt")    
writeLines(fire, fileConn)

#Mr emerald. 
fire[1053] <- paste(fire[1053], fire[1054], fire[1055])
fire[1054] <- ""
fire[1055] <- ""

# poems... 
fire[97:100]
fire[97] <- paste(fire[97], fire[98], fire[99], fire[100])
fire[98] <-""
fire[99] <-""
fire[100] <- ""

fire[105] <- paste(fire[105],fire[106],fire[107],fire[108],fire[109],fire[110], fire[111])
fire[106] <- ""
fire[107] <-""
fire[108] <- ""
fire[109] <-""
fire[110] <-""
fire[111] <-""


fire[116] <- paste(fire[116],fire[117],fire[118],fire[119],fire[120],fire[121],fire[122],fire[123],fire[124],
                   fire[125],fire[126],fire[127])
fire[117] <- ""
fire[118] <- ""
fire[119] <- ""
fire[120] <- ""
fire[121] <- ""
fire[122] <-""
fire[123] <-""
fire[124] <-""
fire[125] <-""
fire[126] <-""
fire[127] <-""

fire[138] <- paste(fire[138],fire[139],fire[140],fire[141],fire[142],fire[143], fire[144], fire[145])
fire[139] <- ""
fire[140] <- ""
fire[141] <- ""
fire[142] <-""
fire[143] <-""
fire[144] <-""
fire[145] <-""

fire[163] <- paste(fire[163],fire[164],fire[165],fire[166])
fire[164] <- ""
fire[165] <-""
fire[166] <-""

fire[200] <- paste(fire[200],fire[201],fire[202],fire[203],fire[204],fire[205],fire[206],fire[207],fire[208],
                   fire[209],fire[210])
fire[201] <- ""
fire[202] <- ""
fire[203] <- ""
fire[204] <- ""
fire[205] <- ""
fire[206] <-""
fire[207] <-""
fire[208] <-""
fire[209] <-""
fire[210] <-""
# fire[200] <- "Between the mountain and the eye The spirit of the distance draws A veil of blue amorous gauze, The very texture of the sky. A breeze reaches the pines, and I Join in the general applause. But we all know it cannot last, The mountain is too weak to wait - Even if reproduced and glassed In me as in a paperweight."
# fire[210] <- "Line 98: On Chapman's Homer"

fire[383] <- paste(fire[383],fire[384],fire[385],fire[386])
fire[384] <- ""
fire[385] <-""
fire[386] <-""

fire[520] <- paste(fire[520],fire[521],fire[522],fire[523],fire[524],fire[525],fire[526],fire[527],fire[528],
                   fire[529],fire[530],fire[531], fire[532],fire[533],fire[534])
fire[521] <- ""
fire[522] <- ""
fire[523] <- ""
fire[524] <- ""
fire[525] <- ""
fire[526] <-""
fire[527] <-""
fire[528] <-""
fire[529] <-""
fire[530] <-""
fire[531] <-""
fire[532] <-""
fire[533] <-""
fire[534] <-""

fire[573] <- paste(fire[573],fire[574],fire[575],fire[576], fire[577])
fire[574] <- ""
fire[575] <-""
fire[576] <-""
fire[577] <-""

fire[712] <- paste(fire[712],fire[713],fire[714])
fire[713] <- ""
fire[714] <-""

fire[723] <- paste(fire[723],fire[724],fire[725],fire[726], fire[727], fire[728])
fire[724] <- ""
fire[725] <-""
fire[726] <-""
fire[727] <-""
fire[728] <-""

fire[765]

first <- fire[1:765]
second <- "Mighty and dreadful, for, thou art not so"
third <- "one deplores the superfluous ejaculation in the second line introduced there only to coagulate the caesura:"
fourth <-fire[766:1069]
fire <- c(first, second, third, fourth)

fire[908] <- "Lines 895-899: The more I weigh ... or this dewlap Instead of these facile and revolting lines, the draft gives:"
fire[909] <- "For Parody, that last resort of wit: 'In nature's strife when fortitude prevails The victim falters and the victor fails.'"

fire[919] <- "This is not quite exact. In the advertisement to which it refers, the whiskers are held up by a bubbly foam, not by a creamy substance. After this line, instead of lines 923-930, we find the following, lightly deleted, variant:"
fire[920] <- "All artists have been born in what they call A sorry age; mine is the worst of all: An age that thinks spacebombs and spaceships take A genius with a foreign name to make, When any jackass can rig up the stuff; An age in which a pack of rogues can bluff The selenographer; a comic age That sees in Dr. Schweitzer a great sage."
fire[927:928] <- ""
fire[929]
fire[930] <- "All artists have been born in what they call A sorry age; mine is the worst of all: An age that thinks spacebombs and spaceships take A genius with a foreign name to make, When any jackass can rig up the stuff; An age in which a pack of rogues can bluff The selenographer; a comic age That sees in Dr. Schweitzer a great sage."
fire[931:935] <- ""

fire[1000] <- paste(fire[1000], fire[1001], fire[1002], fire[1003])
fire[1001:1003]<-""

fire[1011] <- paste(fire[1011], fire[1012], fire[1013], fire[1014])
fire[1012:1014] <-""
fire <- fire[fire!=""]
print(length(fire))

fire.title <- rep("paleFire", 983)
fire.para.type <- rep("paragraph", 983)
fire.para.counter<-seq(1, 983)
fire.para.id <- paste0("PALE_FIRE_", "PARAGRAPH_", fire.para.counter)
fire.label <- rep("1", 983)
print(length(fire.para.id))
fire.para.matrix <- cbind(fire.title, fire.para.type, fire.para.id, fire)
fire.para.df <- as.data.frame(fire.para.matrix, stringsAsFactors = FALSE)
colnames(fire.para.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", fire.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='paleFire' LIMIT 2")
# dbExecute(con, "DELETE FROM textTable WHERE Type='paragraph' AND Title='paleFire'")

dbDisconnect(con)

# done with paras. sentence time.. easy.

fire <- gsub("Mr\\.", "Mr", fire) # do this again.
fire <- gsub("Dr\\.", "Dr", fire) # do this again.
fire <- gsub("Mrs\\.", "Mrs", fire) # do this again.
fire <- gsub("Prof\\.", "Prof", fire) # do this again.
fire <- gsub("10:23\\.", "10:23", fire) # do this again.
fire <- gsub("10:25\\.", "10:25", fire) # do this again.
fire <- gsub("10:37\\.", "10:37", fire) # do this again.
fire <- gsub("Oct\\.", "Oct", fire) # do this again.
fire <- gsub("Mt\\.", "Mt", fire) # do this again.
fire <- gsub("b\\. 1874", "b 1874", fire) # do this again.

first_bite <- fire[1:983]

fire.sents.first <- paste0(first_bite, collapse = "\n")
fire.sents.first <- unlist(tokenize_sentences(fire.sents.first))

fire.sents <- c(fire.sents.first)

bad_spots<-c(0)

## standalone, you need the third condition. 
for(i in seq(1:length(fire.sents))){
  #if the sentence ends with a punctuation mark and the next sentence starts with a lowercase, combine them
  test <- substr(fire.sents[i], nchar(fire.sents[i]), nchar(fire.sents[i]))
  test2 <- substr(fire.sents[i+1], 1, 1)
  test3 <- substr(fire.sents[i], 1, 1)
  if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
    fire.sents[i] <- paste(fire.sents[i], fire.sents[i+1])
    # print(fire.sents[i])
    bad_spots<-append(bad_spots, i+1)
  }
}
bad_spots <- bad_spots[-c(1)]
fire.sents <- fire.sents[-bad_spots]
print(length(fire.sents))

bad_spots <-c(0)
for(i in seq(1:length(fire.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them
  test <- substr(fire.sents[i], nchar(fire.sents[i])-1, nchar(fire.sents[i]))
  test2 <- substr(fire.sents[i+1], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c("?'", "!'") && test2==tolower(test2)){
      fire.sents[i] <- paste(fire.sents[i], fire.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
fire.sents[bad_spots]
fire.sents <- fire.sents[-c(bad_spots)]

length(fire.sents)

fire.title <- rep("paleFire", 2546)
fire.sents.type <- rep("sentence", 2546)
fire.sents.counter<-seq(1, 2546)
fire.label <- rep("1", 2546)
fire.sents.id <- paste0("PALE_FIRE_", "SENT_", fire.sents.counter)
print(length(fire.sents.id))
fire.sents.matrix <- cbind(fire.title, fire.sents.type, fire.sents.id, fire.sents, fire.label)
fire.sents.df <- as.data.frame(fire.sents.matrix, stringsAsFactors = FALSE)
colnames(fire.sents.df) <- stock

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
#dbExecute(con, "DELETE FROM textTable WHERE Type='sentence' AND Title='paleFire'")
dbWriteTable(con, "textTable", fire.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='paleFire' LIMIT 2")
dbDisconnect(con)

# done with paras. words. easy.

fire.temp <- fire
fire.temp <- paste(fire.temp, collapse=" ")
fire.temp <-tolower(fire.temp)
# a better regex that is going to maintain contractions. important! 

fire.temp <- unlist(strsplit(fire.temp, "[^\\w']", perl=TRUE))
fire.not.blanks <- which(fire.temp != "")
fire.words <- fire.temp[fire.not.blanks]

## # # db words.

fire.title <- rep("paleFire", 68132)
fire.words.type <- rep("word", 68132)
fire.words.counter <- seq(1, 68132)
fire.words.id <- paste0("PALE_FIRE_", "WORD_", fire.words.counter)

fire.words.matrix <- cbind(fire.title, fire.words.type, fire.words.id, fire.words)

fire.words.df <- as.data.frame(fire.words.matrix, stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
colnames(fire.words.df) <- c("Title", "Type", "ID", "Unit")
dbWriteTable(con, "textTable", fire.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='paleFire' LIMIT 10")
dbDisconnect(con)