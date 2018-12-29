setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

js <- scan("rawTexts/detective/js-fletcher-the-paradise-mystery.txt",what="character",sep="\n")
js.start <- which(js == "American tourists, sure appreciators of all that is ancient and picturesque in England, invariably come to a halt, holding their breath in a sudden catch of wonder, as they pass through the half-ruinous gateway which admits to the Close of Wrychester. Nowhere else in England is there a fairer prospect of old-world peace. There before their eyes, set in the centre of a great green sward, fringed by tall elms and giant beeches, rises the vast fabric of the thirteenth-century Cathedral, its high spire piercing the skies in which rooks are for ever circling and calling. The time-worn stone, at a little distance delicate as lacework, is transformed at different hours of the day into shifting shades of colour, varying from grey to purple: the massiveness of the great nave and transepts contrasts impressively with the gradual tapering of the spire, rising so high above turret and clerestory that it at last becomes a mere line against the ether. In morning, as in afternoon, or in evening, here is a perpetual atmosphere of rest; and not around the great church alone, but in the quaint and ancient houses which fence in the Close. Little less old than the mighty mass of stone on which their ivy-framed windows look, these houses make the casual observer feel that here, if anywhere in the world, life must needs run smoothly. Under those high gables, behind those mullioned windows, in the beautiful old gardens lying between the stone porches and the elm-shadowed lawn, nothing, one would think, could possibly exist but leisured and pleasant existence: even the busy streets of the old city, outside the crumbling gateway, seem, for the moment, far off.")
js.fin <- which(js =="“You must have been very blind not to have seen that for a long time!” she answered.")
js <- js[js.start:js.fin]

spots <- grep('[A-Z]{2,}[^a-z]', js)
js[spots]
js <- js[-c(spots[-c(9)])]

js.paragraphs <- as.data.frame(js, stringsAsFactors=FALSE)
colnames(js.paragraphs) <- c("paras")

js.paragraphs<- js.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

js.paragraphs <- js.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

js.paragraphs <- js.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

js.paragraphs <- js.paragraphs %>% 
  filter(paragraphs!="")

js.paragraphs <- js.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(js.paragraphs)

js.paragraphs <- js.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

js.paragraphs <- js.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(js.paragraphs$paragraphs))

js.paragraphs <- js.paragraphs %>% 
  filter(paragraphs!="")

js.paragraphs <- js.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(js.paragraphs$paragraphs))


js.title <- rep("theParadiseMystery", 1742)
js.para.type <- rep("paragraph",1742)
js.para.counter<-seq(1, 1742)
js.para.id <- paste0("THE_PARADISE_MYSTERY_", "PARAGRAPH_", js.para.counter)
js.label <- rep("0", 1742)
print(length(js.para.id))

js.para.matrix <- cbind(js.title, js.para.type, js.para.id, js.paragraphs, js.label)
js.para.df <- as.data.frame(js.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(js.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", js.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theParadiseMystery' LIMIT 2")
dbDisconnect(con)

# sents
