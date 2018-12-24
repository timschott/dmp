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
fire <- gsub("  ", " ", fire) # do this again.
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
# 40 and The sun with stolen ice, the moon with leaves DONE
fire[63] <- paste(fire[63], fire[64])
fire[64] <- ""

# The next masquerader will be shot at sight. What's your real name, Charlie? I'm British. I'm a tourist, said
#theKing. Well, anyway, take off that red fufa. And the cap. Give them here. He tossed the things in the back of the car and drove off. DONE
fire[297]
fire[298]
fire[297] <- paste(fire[297], fire[298])
fire[298] <- ""

# to lines 286 and 408).
# Migraine again worse today. ## DONE

fire[342] <- paste(fire[342], fire[343])
fire[343] <- ""
#10:23. #Scrappy and scrabbly sounds. DONE
fire[476] <- paste(fire[476], fire[477],sep=" ")
fire[477] <- ""
#10:25.
it <- c('A roundlet of pale light, the size of a small doily; flitted across the dark walls, the boarded windows, and the floor; changed its place; lingered here and there, dancing up and down; seemed to wait in teasing play for evadable pounce. Gone.')
fire[478] <- paste(fire[478], it, sep=" ")
fire[479] <- ""
# done 
#10:37. SEW TO #Back again. 
#DONE
fire[480] <- paste(fire[480], fire[481], sep=" ")
fire[481]<- ""
# (whom they spelled SEW Khrushchev) DONE
fire[936] <-paste(fire[936], fire[937], sep=" ")
fire[937] <-""
# separate
#Sun: Ground meat \\ (All it got from me was milk and sardines
# make a first part, second part, and third, and then merge them. 
# done
first <- fire[1:78]
second <- "Sun: Ground meat"
third <- " (All it got from me was milk and sardines; it was a likable little creature but after a while its movements began to grate on my nerves and I farmed it out to Mrs. Finley, the cleaning woman.) But perhaps the funniest note concerned the manipulations of the window curtains which had to be drawn in different ways at different hours to prevent the sun from getting at the upholstery. A description of the position of the sun, daily and seasonal, was given for the several windows, and if I had heeded all this I would have been kept as busy as a participant in a regatta. A footnote, however, generously suggested that instead of manning the curtains, I might prefer to shift and reshift out of sun range the more precious pieces of furniture (two embroidered armchairs and a heavy royal console) but should do it carefully lest I scratch the wall moldings. I cannot, alas, reproduce the meticulous schedule of these transposals but seem to recall that I was supposed to castle the long way before going to bed and the short way first thing in the morning. My dear Shade roared with laughter when I led him on a tour of inspection and had him find some of those bunny eggs for himself. Thank God, his robust hilarity dissipated the atmosphere of damnum infectum in which I was supposed to dwell. On his part, he regaled me with a number of anecdotes concerning the judge's dry wit and courtroom mannerisms; most of these anecdotes were doubtless folklore exaggerations, a few were evident inventions, and all were harmless. He did not bring up, my sweet old friend never did, ridiculous stories about the terrifying shadows that Judge Goldsworth's gown threw across the underworld, or about this or that beast lying in prison and positively dying of raghdirst (thirst for revenge) - crass banalities circulated by the scurrilous and the heartless - by all those for whom romance, remoteness, sealskin-lined scarlet skies, the darkening dunes of a fabulous kingdom, simply do not exist. But enough of this."
fourth <- fire[79:1039]
fire <- c(first, second, third, fourth)
# Verbálala wod gév ut trí phantána \\(I have marked the stress accents).
# done.
first <- fire[1:167]
second <- "Verbálala wod gév ut trí phantána"
third <- "(I have marked the stress accents)."
fourth <-fire[168:1041]
fire <- c(first, second, third, fourth)
# answered.  \\ Bon soir, Sybil.
# done.
first <- fire[1:351]
second <- "Now there is nothing a lonesome man relishes more than an impromptu birthday party, and thinking nay, feeling certain - that my unattended telephone had been ringing all day, I blithely dialed the Shades' number, and of course it was Sybil who answered."
third <- "Bon soir, Sybil."
fourth <-fire[352:1043]
fire <- c(first, second, third, fourth)
# THE HAUNTED BARN \\ Pitch-darkness. Father, Mother and Daughter are heard breathing gently in different corners. Three minutes pass.
first <- fire[1:498]
second <- "The minutes of that third session in the barn have not been preserved but I offer the reader the following scene which I feel cannot be too far removed from the truth:"
third <- "THE HAUNTED BARN"
fourth <-fire[499:1045]
fire <- c(first, second, third, fourth)

# get rid of blank lines.
fire <- fire[fire!=""]
fire <- gsub("  ", " ", fire) # do this again.

# okay, in theory, this is pale fire's foreword, and the commentary,
# in separated paragraphs. 