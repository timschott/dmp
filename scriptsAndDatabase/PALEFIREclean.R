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

grep("\\* \\* \\*", perl=TRUE, fire)
fire[537] <- ""
fire[483] <- c("pada ata lane pad not ogo old wart alan ther tale feur far rant lant tal told")
fire <- gsub("  ", " ", fire)
grep("PALE FIRE", fire)
fire[32:34] <- ""
# get rid of ..
# replace
# * * *
# for
# pada ata lane pad not ogo old wart alan ther tale feur far rant lant tal told
# (first time) DONE
# "" second time DONE
# "  " for " " DONE

# 678 DONE. 
fire[677] <- "Upon referring to my little diary, I see that during the five-month period of my intercourse with the Shades I was invited to their table exactly three times. Initiation took place on Saturday, March the 14th, when I dined at their house with the following people: Nattochdag (whom I saw every day in his office); Professor Gordon of the Music Department (who completely dominated the conversation); the Head of the Russian Department (a farcical pedant of whom the less said the better); and three or four interchangeable women (of whom one - Mrs. Gordon, I think) was enceinte, and another, a perfect stranger, steadily talked to me, or rather into me, from eight to eleven owing to an unfortunate afterdinner distribution of available seats. My next treat, a smaller but by no means cozier souper on Saturday, May 23, was attended by Milton Stone (a new librarian, with whom Shade discussed till midnight the classification of certain Wordsmithiana); good old Nattochdag (whom I continued to see every day); and an undeodorized Frenchwoman (who gave me a complete picture of language-teaching conditions at the University of California). The date of my third and last meal at the Shades is not entered in my little book but I know it was one morning in June when I brought over a beautiful plan I had drawn of the King's Palace in Onhava with all sorts of heraldic niceties, and a touch of gold paint that I had some trouble in obtaining, and was graciously urged to stay for an impromptu lunch. I should add that, despite my protests, at all three meals my vegetarian limitations of fare were not taken into account, and I was exposed to animal matter in, or around, some of the contaminated greens I might have deigned to taste. I revanched myself rather neatly. Of a dozen or so invitations that I extended, the Shades accepted just three. Every one of these meals was built around some vegetable that I subjected to as many exquisite metamorphoses as Parmentier had his pet tuber undergo. Every time I had but one additional guest to entertain Mrs. Shade (who, if you please - thinning my voice to a feminine pitch - was allergic to artichokes, avocado pears, African acorns - in fact to everything beginning with an \"a\"). I find nothing more conducive to the blunting of one's appetite than to have none but elderly persons sitting around one at table, fouling their napkins with the disintegration of their make-up, and surreptitiously trying, behind noncommittal smiles, to dislodge the red-hot torture point of a raspberry seed from between false gum and dead gum. So I had young people, students: the first time, the son of a padishah; the second time, my gardener; and the third time, that girl in the black leotard, with that long white face and eyelids painted a ghoulish green; but she came very late, and the Shades left very early - in fact, I doubt if the confrontation lasted more than ten minutes, whereupon I had the task of entertaining the young lady with phonograph records far into the night when at last she rang up somebody to accompany her to a \"diner\" in Dulwich."
# okay pretty okay.

# paste
# 40 and The sun with stolen ice, the moon with leaves
# The next masquerader will be shot at sight. What's your real name, Charlie? I'm British. I'm a tourist, said
#theKing. Well, anyway, take off that red fufa. And the cap. Give them here. He tossed the things in the back of the car and drove off.
# to lines 286 and 408).
# Migraine again worse today.
# 10:14 P.M. Investigation commenced.
#10:23.
#Scrappy and scrabbly sounds.
#10:25.
#A roundlet of pale light, the size of a small doily; flitted across the dark walls, the boarded windows,
#10:37.
#Back again.
# (whom they spelled SEW Khrushchev)

# separate
#Sun: Ground meat \\ (All it got from me was milk and sardines
# Verbálala wod gév ut trí phantána \\(I have marked the stress accents).
# answered.  \\ Bon soir, Sybil.
# THE HAUNTED BARN \\ Pitch-darkness. Father, Mother and Daughter are heard breathing gently in different corners. Three minutes pass.
