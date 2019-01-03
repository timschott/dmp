setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

stow.paragraphs <- read.csv("Python_Scripts/checkCorpus/STOW_paras.csv", stringsAsFactors = FALSE)
stow.paragraphs$X0[20]
stow.paragraphs <- stow.paragraphs[-c(1:40,2631:2634),]
colnames(stow.paragraphs) <- c("arb", "paragraphs")
colnames(stow.paragraphs)

spots <- grep('[A-Z]{2,}[^a-z]', stow.paragraphs$paragraphs)
stow.paragraphs$paragraphs[spots]
spots <- spots[-c(17:18, 39, 53, 54)]
stow.paragraphs <- stow.paragraphs[-c(spots),]
colnames(stow.paragraphs) <- c("arb", "paragraphs")

stow.paragraphs$paragraphs[64] <- "\"The Stowmarket Mystery is a strange mixture of the real and the unreal.\nSir Alan Hume-Frazer, fourth baronet, met his death on the hunting-field.\nHis horse blundered at a brook and the rider was impaled on a hidden\nstake, placed in the stream by his own orders to prevent poachers from\nnetting trout. His wife, née Somers, a Bristol family, had pre-deceased\nhim."
stow.paragraphs$paragraphs[65] <- "\"There were two children, a daughter, Margaret, aged twenty-five, and a\nson, Alan, aged twenty-three. By his will, Sir Alan left all his real and\npersonal estate to his son, with a life charge of £31,000 per annum for the\ndaughter. As he was a very wealthy man, almost a millionaire, the\nprovision for his daughter was niggardly, which might be accounted for by\nthe fact that the girl, several years before her father's death,\nquarrelled with him and left home, residing in London and in Florence.\nBoth children, by the way, were born in Italy, where Sir Alan met and\nmarried Miss Somers."
stow.paragraphs$paragraphs[66] <- "\"The old gentleman, it appeared, allowed Miss Hume-Frazer £35,000 per annum\nduring his life. His son voluntarily continued this allowance, but the\nbrother and sister continued to live apart, he devoted to travel and\nsport, she to music and art, with a leaning towards the occult--a woman\ndivorced from conventionality and filled with a hatred of restraint."
stow.paragraphs$paragraphs[160] <- "\"No. He told me she laughed at him, and invited him to witness the trying\non of a fancy dress costume, the 'Queen of Night,' which she wore at a bal masqué the night he was murdered.\""
stow.paragraphs$paragraphs[249] <- "\"Yes; the story cannot end here. You and your fiancée have suffered. Miss Layton must be a very estimable young lady--one worth winning. She will be a true and loyal wife.\""
stow.paragraphs$paragraphs[299] <- "A black velvet coat and a brilliant tie were the only bizarre features of his costume. They served sufficiently to enhance his foreign appearance. Such a man would be correctly placed in the marble frame of a Neapolitan villa; here he was unusual, outré, \"un-English,\" as Brett put it."
stow.paragraphs$paragraphs[475] <- "\"Quite sure, Miss Layton,\" he said, with the smile which made him such a prompt favourite with women. \"I had nothing to do but observe the mise-en-scène. The stage was quite clear for the chief actors. And now, may I make a suggestion? The longer we remain here the more likely are we to attract observation. Mr. Hume and I are going to call on Mrs. Eastham. May we expect you in an hour's time?\""
stow.paragraphs$paragraphs[620] <- "\"That is a difficult question to answer. I was very careless in money matters, but it is clear that the curtailment of my rate of living from £15,000 to £5,000 per annum must make considerable difference to all connected with me.\""
stow.paragraphs$paragraphs[635] <- "Her voice was cold, impassive, marvellously under control. It warned him, threw him back into the safe rule of Hume's adviser and friend."
stow.paragraphs$paragraphs[637] <- "\"Well, what I tell you is true. There are no less than ten people, all living, I have no doubt, who can testify to its correctness. I had a box at the Fancy Dress Ball that New Year's Eve. I invited nine guests. One of them, an attaché at the Italian Embassy, brought Giovanni and introduced him to me. We were together from midnight until 4.30 a.m. Whilst poor Alan was lying here dead, I was revelling at a bal masqué. Do you think I am likely to forget the circumstances?\""
stow.paragraphs$paragraphs[652] <- "Hume was too happy, after a prolonged tête-à-tête with his beloved, to harbour malice against any person."
stow.paragraphs$paragraphs[831] <- "\"He means mischief to somebody,\" was Winter's summing up. \"I wonder if he intends to knife Hume?\" for Brett had given his professional confrère a synopsis of all that happened before they met, and of his subsequent conversation with the \"happy couple\" in Beechcroft Hall."
stow.paragraphs$paragraphs[899] <- "Brett wanted to hand him £50, but found that all the money he had in his possession at the moment only totalled up to £35."
stow.paragraphs$paragraphs[907] <- "Brett gave him a resumé of events. A chance allusion to Sir Alan caused the young man to exclaim:"
stow.paragraphs$paragraphs[947] <- "Although the hour was late for calling upon a complete stranger, the barrister could not rest until he had inspected the Jiro ménage. No. 17 was a long way from the ground level. Indeed, the cats of Kensington, if sufficiently enterprising, inhabitated the floor above."
stow.paragraphs$paragraphs[1168] <- "\"Perhaps you are mistaken. Events have conspired to point to you as the unconscious source of a good deal that has happened. Personally, Miss Layton, I incline to the belief that you are no more responsible than David Hume-Frazer. If the mystery of Sir Alan's death is ever solved, I feel assured that its genesis will be found in circumstances not only beyond your control, but wholly independent, and likely to operate in the same way if both you and your fiancée had never either seen or heard of Beechcroft Hall.\""
stow.paragraphs$paragraphs[1197] <- "Whatever the failings of Beechcroft might be, they had not reached the kitchen. Delightful little rolls of thin bread and butter, sandwiches of cucumber and pâté de foie gras, tempting morsels of pastry, home-made jam, and crisp biscuits showed that the housekeeper had unconsciously adopted Brett's view of her mistress's needs."
stow.paragraphs$paragraphs[1268] <- "\"By no means. He is far from rich as we understand the word. He is worth, I believe, £1,500 a-year. Why do you ask? Had you the impression that he married me for my money?\""
stow.paragraphs$paragraphs[1307] <- "\"You are both doctor and lawyer, Mr. Brett. My heart is quite sound. I have been foolish enough to seek relief from my troubles in morphia. Do not be alarmed. I am not a morphinée. I promised Nellie yesterday to stop it, and I am quite certain to succeed.\""
stow.paragraphs$paragraphs[1310] <- "Helen wore a simple white muslin dress, with pale stow ribbons. Margaret, mindful of the barrister's hint concerning her attire, now appeared in pale grey crêpe de chine, trimmed with cerise panne velvet."
stow.paragraphs$paragraphs[1394] <- "\"How did you come to be in such a state?\" asked Margaret nervously. \"It is hardly six months since I sent you £500; not a very large sum, I admit, but all you asked me for, and more than enough to live on for a much longer period.\""
stow.paragraphs$paragraphs[1396] <- "\"My dear girl,\" he answered, \"I am really a very unfortunate person. I own a hundred thousand acres of the best land in South America, and I have been in England nearly two years trying to raise capital to develop it. If I owned a salted reef or an American brewery I could have got the money for the asking. Because my stock-raising proposition is a sound paying concern, requiring a delay of at least three years before a penny of profit can be realised, I have worn my boots out in climbing up and down office stairs to no purpose. Out of your £500, nearly £400 went out at once to pay arrears of Government taxation to save my property. Of the remaining hundred I spent fifty in a fortnight on dinners and suppers given to a gang of top-hatted scoundrels, who, I found subsequently, were not worth a red cent. They hoped to fleece me in some way, and their very association discredited me in the eyes of one or two honest men. Oh, I have had a bad time of it, I can assure you!\""
stow.paragraphs$paragraphs[1402] <- "\"Thank you, Rita. You are a good sort. But I am not here on a matter of high finance. I want you to lend me, say, £250. I will return to the Argentine, and take twenty years to accomplish what I could do in five with the necessary capital.\""
stow.paragraphs$paragraphs[1815] <- "\"Does it convey no moral to you? I fear not. Now mark me, Winter. Just as the breed of the chicken is indelibly stamped on it in the eyes of a man skilled in chickens, so is the murder we are investigating marked by characteristics so plain that a child of ten, properly trained to use his eyes, might discern them. What you and I suffer from are defects implanted by idle nursemaids and doting mothers. Let us, for the moment, adopt the policy of the theosophists and sit in consultation apart from our astral bodies. Who killed Sir Alan Hume-Frazer? I answer, a relative. What relative? Someone we do not know, whom he did not know, or who committed murder because he was known. What sort of person is the murderer? A man physically like either David or Robert, so like that 'Rabbit Jack' would swear to the identity of either of them as readily as to the person of the real murderer. Why did he use such a weird instrument as the Ko-Katana? Because he found it under his hand and recognised its sinister purpose, to be left implanted in the breast or brain of an enemy's lifeless body. Where is the man now? In London, perhaps outside this building, perhaps watching the Northumberland Avenue Hotel, waiting quietly for another chance to take the life of the person who caused us to reopen this inquiry. To sum up, Winter, let us find such an individual, a Hume-Frazer with black, deadly eyes, with a cold, calculating, remorseless brain, with a knowledge of trick and fence not generally an attribute of the Anglo-Saxon race--let us lay hands on him, I say, and you can book him for kingdom come, vi the Old Bailey.\""
stow.paragraphs$paragraphs[1875] <- "\"Wait just a little while,\" said David kindly. \"You hardly understand this business. The madman who attacked us meant to injure me, not you. Here is a cheque for £100, which will not only replace your horse and cab, but leave you a little over for the loss of your time.\""
stow.paragraphs$paragraphs[2146] <- "\"This gentleman has twenty other names,\" he added; \"but the foregoing list will suffice. Doesn't it strike you as odd that the man who struck down the fifth Hume-Frazer baronet on the spot so fatal to his four predecessors, should bring from a country given to such name-changes a cognomen that irresistibly recalls the original enemy of the family, David Hume"
stow.paragraphs$paragraphs[2165] <- "\"I find,\" said Mr. Holden, when the mise-en-scène was quite to his liking, \"that a good map, and a few realistic models of the principal buildings dealt with in my discourse, give a lucidity and a coherence otherwise foreign to the narrative.\""
stow.paragraphs$paragraphs[2181] <- "\"You are leaving out of count the biggest sensation, namely, the title to the Beechcroft estates. Under her father's will, if it is very cleverly drawn, Mrs. Capella may receive £1,000 per annum. She has not the remotest claim to Beechcroft and its revenues or to her brother's intestate estate.\""
stow.paragraphs$paragraphs[2213] <- "Next morning Brett went to Somerset House to consult the will in which Margaret's father left her £1,000 a year. Her brother died intestate."
stow.paragraphs$paragraphs[1505] <- "payment of the land-tax on Mr Frazer's estate-- £650 per"

stow.paragraphs <- stow.paragraphs %>% 
  transmute(paras=  gsub("\n", " ", paragraphs) )

stow.paragraphs<- stow.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

stow.paragraphs <- stow.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

stow.paragraphs <- stow.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

colnames(stow.paragraphs) <- c("paras")
stow.paragraphs <- stow.paragraphs %>% 
  filter(paras!="")

stow.paragraphs <- stow.paragraphs %>% 
  filter(paras!="  ")
colnames(stow.paragraphs)

stow.paragraphs <- stow.paragraphs %>% 
  transmute(paragraphs = replace_abbreviation(paras))

stow.paragraphs <- stow.paragraphs %>% 
  transmute(paras=  gsub("MR\\.", "Mr", paragraphs))

print(length(stow.paragraphs$paras))

stow.paragraphs <- stow.paragraphs %>% 
  filter(paras!="")

stow.paragraphs <- stow.paragraphs %>% 
  filter(paras!="  ")
stow.paragraphs <- stow.paragraphs %>% 
  transmute(paragraphs=  gsub("_", "", paras))
stow.paragraphs$paragraphs[1504] <- paste(stow.paragraphs$paragraphs[1504], stow.paragraphs$paragraphs[1505], stow.paragraphs$paragraphs[1506], stow.paragraphs$paragraphs[1504],
                                          stow.paragraphs$paragraphs[1507], stow.paragraphs$paragraphs[1508], stow.paragraphs$paragraphs[1509],
                                          stow.paragraphs$paragraphs[1510], stow.paragraphs$paragraphs[1511], stow.paragraphs$paragraphs[1512],
                                          stow.paragraphs$paragraphs[1513], stow.paragraphs$paragraphs[1514], stow.paragraphs$paragraphs[1515])
stow.paragraphs$paragraphs[1505:1515] <- ""
stow.paragraphs <- stow.paragraphs %>% 
  filter(paragraphs!="")

stow.title <- rep("theStowmarketMystery", 2514)
stow.para.type <- rep("paragraph",2514)
stow.para.counter<-seq(1, 2514)
stow.para.id <- paste0("THE_STOWMARKET_MYSTERY_", "PARAGRAPH_", stow.para.counter)
stow.label <- rep("0", 2514)
print(length(stow.para.id))

stow.para.matrix <- cbind(stow.title, stow.para.type, stow.para.id, stow.paragraphs, stow.label)
stow.para.df <- as.data.frame(stow.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(stow.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", stow.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theStowmarketMystery' LIMIT 2")
dbDisconnect(con)

# sents. 
stow <- stow.para.df$Unit
first_bite <- stow[1:2514]
first_bite <- gsub("No\\.", "No", perl=TRUE, first_bite)

stow.sents.first <- paste0(first_bite, collapse = "\n")
stow.sents.first <- unlist(tokenize_sentences(stow.sents.first))

stow.sents <- c(stow.sents.first)
stow.sents.df <- as.data.frame(stow.sents, stringsAsFactors = FALSE)

print(length(stow.sents.df$stow.sents))

bad_spots <-c(0)
for(i in seq(1:length(stow.sents))){
  #if the sentence ends with a punctuation mark and the next character is a lowercase, combine them,
  # if the sequence starts with a capital letter... but for eg ha! ha! ha! don't combine
  # so check if the first sentence starts with a lowercase as well
  test <- substr(stow.sents[i], nchar(stow.sents[i]), nchar(stow.sents[i]))
  test2 <- substr(stow.sents[i+1], 1, 1)
  test3 <- substr(stow.sents[i], 1, 1)
  if(test2 %in% c(LETTERS, letters)){
    if(test %in% c('?', '!') && test2==tolower(test2) && test3!=tolower(test3)){
      #print(i)
      stow.sents[i] <- paste(stow.sents[i], stow.sents[i+1])
      bad_spots<-append(bad_spots, i+1)
    }
  }
}
bad_spots <- bad_spots[-c(1)]
stow.sents[bad_spots]
stow.sents <- stow.sents[-c(bad_spots)]
stow.sents <- stow.sents[stow.sents!=""]
print(length(stow.sents))

stow.title <- rep("theStowmarketMystery", 5333)
stow.sents.type <- rep("sentence", 5333)
stow.sents.counter<-seq(1, 5333)
stow.sents.id <- paste0("THE_STOWMARKET_MYSTERY_", "SENT_", stow.sents.counter)
stow.label <- rep("0", 5333)
print(length(stow.sents.id))

stow.sents.matrix <- cbind(stow.title, stow.sents.type, stow.sents.id, stow.sents, stow.label)
stow.sents.df <- as.data.frame(stow.sents.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(stow.sents.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")

dbWriteTable(con, "textTable", stow.sents.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='sentence' AND Title='theStowmarketMystery' LIMIT 2")
dbDisconnect(con)

# words.

stow.temp <- stow
stow.temp <- paste(stow.temp, collapse=" ")
stow.temp <-tolower(stow.temp)
# a better regex that is going to maintain contractions. important! 

stow.temp <- unlist(strsplit(stow.temp, "[^\\w']", perl=TRUE))
stow.not.blanks <- which(stow.temp != "")
stow.words <- stow.temp[stow.not.blanks]
print(length(stow.words))
stow.words<- stow.words[which(stow.words!="^'")]
stow.words<- stow.words[which(stow.words!="'")]
stow.words<- stow.words[which(stow.words!="''")]
print(length(stow.words))
stow.words.df <- as.data.frame(stow.words, stringsAsFactors = FALSE)

stow.title <- rep("theStowmarketMystery", 66788)
stow.words.type <- rep("word", 66788)
stow.words.counter <- seq(1, 66788)
stow.words.id <- paste0("THE_STOWMARKET_MYSTERY_", "WORD_", stow.words.counter)
stow.label<- rep("0", 66788)
stow.words.matrix <- cbind(stow.title, stow.words.type, stow.words.id, stow.words, stow.label)

stow.words.df <- as.data.frame(stow.words.matrix, stringsAsFactors = FALSE)

stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(stow.words.df) <- c("Title", "Type", "ID", "Unit", "Label")
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", stow.words.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT * FROM textTable WHERE Type= 'word' AND Title='theStowmarketMystery' LIMIT 10")
dbGetQuery(con, "SELECT COUNT(*) FROM textTable WHERE Type='word' and Label='0'")
dbDisconnect(con)
