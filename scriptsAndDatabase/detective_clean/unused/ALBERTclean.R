setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library(stringi)
library("stringr")
library("tm")
library(qdap)

# just do the paragraphs first then distill down. 

alb.paragraphs <- read.csv("Python_Scripts/checkCorpus/ALBERT_paras.csv", stringsAsFactors = FALSE)
alb.paragraphs$X0[20]

alb.paragraphs <- alb.paragraphs[-c(1:18,2241:2244),]
colnames(alb.paragraphs) <- c("arb", "paragraphs")
alb.paragraphs$paragraphs[c(779, 1979)]
# 6, 11, 14, 17*, 22, 31, 34, 35*, 47, 53, 67, 71, 77, 78*, 94, 95
# *Illustration
alb.paragraphs$paragraphs[174] <- "The earl's face had brightened at the prospect of meeting his fiancée under the favourable conditions of Brett's presence. But he yielded with\ngood grace, and promptly sat down to write a brief note explanatory of\nthe barrister's identity and position in the inquiry."
alb.paragraphs$paragraphs[284] <- "The appearance of Winter at the door caused the gaping idlers in the\nstreet to endeavour to draw nearer to the mysterious portals. Thereupon\nthree policemen on duty outside hustled the mob back, and Brett took\nadvantage of the confusion thus created to slip to the doorway almost\nunperceived. One of the police constables turned round to make a grab at\nhim, but a signal from a confrère inside prevented this, and Brett\nquickly found himself within a spacious entrance hall with the door\nclosed and bolted behind him."
alb.paragraphs$paragraphs[288] <- "Inspector Walters assumed the role of guide."
alb.paragraphs$paragraphs[317] <- "\"I expected as much,\" he said, taking hold of the torn part of the\nscreen and giving it a vigorous pull, with the result that a small\npiece, measuring about eight inches by six, came bodily out. \"This has\nbeen cut away, as you will see, by some instrument which did not even\nbend the wire. It was subsequently replaced, whilst the fractured parts\nwere sufficiently cemented by some composition to retain this section in\nits place, and practically defy observation. There was nothing for it\nbut force to reveal it thus early. No doubt in time the composition\nwould have dried, or been washed away, and then this bit of the screen\nwould have fallen out by the action of wind and weather. Here, at any\nrate, is a hole in your defensive armour.\" He held out the pièce de conviction to the discomfited Sharpe, who surveyed it in silence."
alb.paragraphs$paragraphs[385] <- "He did not explain to his professional confrère that it was a positive\nstimulant to his abounding energy and highly-strung nerves to find that\nhe was actually following the path taken by the criminal whom he was\npursuing. The mere fact lent reality to the chase. For a mile, at any\nrate, there could be no mistake, though he might expect a check at the\nCarlton. Arrived there, Brett alighted."
alb.paragraphs$paragraphs[554] <- "On their way they captured a railway official and told him to reserve a coupè lit compartment. In the midst of their hasty meal the Frenchman\narrived, voluble, apologetic. The train was crowded. Never had there\nbeen such a rush to the South. By the exercise of most profound care he\nhad secured them two seats in a compartment, but the third had already\ntaken itself. He was sorry for it; he had done his best."
alb.paragraphs$paragraphs[621] <- "He glanced at his watch. \"It is just about time for déjeuner,\" he\ncontinued. \"What do you say if we drive to the Rue Barbette at once?\""
alb.paragraphs$paragraphs[642] <- "\"Have you had déjeuner, or have you time to join me in a cigarette?\"\nhe went on."
alb.paragraphs$paragraphs[775] <- ""
alb.paragraphs$paragraphs[872] <- "\"That is the way people live in Paris, my dear fellow. Life is an\nartificial matter here. But all this excitement has made me hungry. Let\nus have déjeuner.\""
alb.paragraphs$paragraphs[1382] <- "\"'_Vous etes un très bel Anglais, mon vieux,_' she cried, coquettishly\nsetting her head on one side and glancing first at him and then at me.\""
alb.paragraphs$paragraphs[1407] <- "Brett now deemed it advisable to take the commissary of police fully\ninto his confidence. The official promptly suggested that every\npersonage in Paris connected even remotely with the mystery--Gros Jean,\nthe Turks, the waiter at the Café Noir, and even the little thief \"Le\nVer\"--should be arrested and subjected to a procès-verbal."
alb.paragraphs$paragraphs[1422] <- ""
alb.paragraphs$paragraphs[1690] <- "It is a most curious fact that young ladies in the engaged stage regard\ntheir fiancée's male friends with extreme suspicion; the more\nenthusiastic the man, the more suspicious the woman."
alb.paragraphs$paragraphs[1793] <- "He had hardly quitted the hotel when a waiter announced that a jeune Française wished to see Mr. Brett."
alb.paragraphs$paragraphs[1949] <- "\"Voilà! Ils viennent! Venez vite!_\" cried Gros Jean."
alb.paragraphs$paragraphs[2220] <- "\"What a darling!\" cried Edith. \"I do wish he would say something. Cher Prophète, parlez avec moi!_\""
alb.paragraphs$paragraphs[2222] <- "\"_Vive Mahomet! Vive le Sultan! ¿ bas les Grecs! ‡ bas! ‡ bas!_\""
alb.paragraphs$paragraphs[779] <- ""
alb.paragraphs$paragraphs[1979] <- ""
alb.paragraphs$paragraphs[583] <- "Although Gaultier had not said as much, Brett guessed that his destination was the British Embassy in the Rue du Faubourg St. Honoré. The route followed by the cabman led straight to that well-known locality. The Frenchman in the second cab evidently thought likewise, for, at the corner of the Rue Boissy he pulled up, and Brett was just in time to give his driver instructions to go ahead and thus avoid attracting undue notice to himself."
alb.paragraphs$paragraphs[597] <- "\"Yes,\" replied the King's messenger, \"and what is more, I have discovered his residence since we parted. It seems that one of the attachés at the Embassy met him recently and thought it advisable to keep in touch with the Young Turkish party, of which Hussein-ul-Mulk is a shining light. So he asked him where he lived, and as the result I have jotted down the address in my note-book.\" Gaultier searched through his memoranda, and speedily found what he wanted."
alb.paragraphs$paragraphs[653] <- "Gaultier knew that there was more behind the apparent exchange of compliments than appeared on the surface. Having fulfilled his pledge to Brett, he said hurriedly, \"Both of you gentlemen will understand that I cannot very well take part in a political discussion. With your permission, Hussein, I will now leave my friend with you for a half-hour's chat, as I have an appointment at the Café Riche.\""
alb.paragraphs$paragraphs[855]<-"\"Thank you,\" said Brett. The two re-entered their cab, and Brett told the driver to proceed as rapidly as possible to the Rue St. Honoré."
alb.paragraphs$paragraphs[875] <- "On their way to the hotel, Brett, yielding apparently to a momentary impulse, stopped the cab at a house in the Rue du Chaussée d'Antin. Without any explanation to Lord Fairholme he disappeared into the interior, and did not rejoin his companion for nearly ten minutes."

# loop to find non UTF 8 lines. jeez.
bad_spots <-c(0)
for(i in seq(1:length(alb.paragraphs$paragraphs))){
  if(all(stri_enc_isutf8(alb.paragraphs$paragraphs[i]))==FALSE){
      bad_spots<-append(bad_spots, i+1)
    }
}
# bad_spots
alb.paragraphs$paragraphs[1174] <- "Apologising to André with a laugh, he then sauntered towards the front café, where he purchased another drink at the counter. He assured\nhimself that he had not been mistaken. The only private door out of the\nbar led into the passage, so that the room beyond could only be reached\nby a staircase or through a trap-door."
alb.paragraphs$paragraphs[1549] <- "How much further the revelations as to Père Didon's iniquity might have\ngone, Miss Talbot could not say, but at that moment there came an\ninterruption."
alb.paragraphs$paragraphs[1924] <- "\"Ah, monsoo,\" he cried with boisterous good humour, \"permittez-moi\nintroducer un friend of mine, Monsoo Smeeth, de Londres, you know. Je ne\nsavez pas les noms de votre companiongs, but they are très bons camarades, je suis certain.\""
alb.paragraphs$paragraphs[908] <- "Soon after three o'clock a report arrived from the agent in the Rue du Chaussée d'Antin. It read--"
# alb.paragraphs$paras[950]
# alb.paragraphs$paras[grep("Caf\xfc\xbe\x8e\x96\x94\xbc Noir", alb.paragraphs$paras)]

alb.paragraphs <- alb.paragraphs %>% 
  transmute(paras=  gsub("Caf\xfc\xbe\x8e\x96\x94\xbc Noir", "Café Noir", paragraphs) )
# 44,32,21,
colnames(alb.paragraphs) <- c("paragraphs")
alb.paragraphs <- alb.paragraphs %>% 
  transmute(paras=  gsub("Caf\xe9\nNoir", "Café Noir", paragraphs) )

colnames(alb.paragraphs) <- c("paragraphs")

alb.paragraphs <- alb.paragraphs %>% 
  transmute(paras=  gsub("\n", " ", paragraphs) )
alb.paragraphs$paras[959]

alb.paragraphs<- alb.paragraphs %>%
  transmute(paragraphs=gsub("\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

alb.paragraphs <- alb.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

alb.paragraphs <- alb.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

colnames(alb.paragraphs) <- c("paras")
alb.paragraphs <- alb.paragraphs %>% 
  filter(paras!="")

alb.paragraphs <- alb.paragraphs %>% 
  filter(paras!="  ")
colnames(alb.paragraphs)

alb.paragraphs <- alb.paragraphs %>% 
  transmute(paragraphs = replace_abbreviation(paras))

alb.paragraphs <- alb.paragraphs %>% 
  transmute(paras=  gsub("\"", "'", paragraphs))

print(length(alb.paragraphs$paras))

alb.paragraphs <- alb.paragraphs %>% 
  filter(paras!="")

alb.paragraphs <- alb.paragraphs %>% 
  filter(paras!="  ")
alb.paragraphs <- alb.paragraphs %>% 
  transmute(paragraphs=  gsub("_", "", paras))