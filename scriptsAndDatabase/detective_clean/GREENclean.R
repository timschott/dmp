setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

green <- scan("rawTexts/detective/anna-katharine-green-the-leavenworth-case.txt",what="character",sep="\n")
green.start <- which(green=="I had been a junior partner in the firm of Veeley, Carr & Raymond, attorneys and counsellors at law, for about a year, when one morning, in the temporary absence of both Mr. Veeley and Mr. Carr, there came into our office a young man whose whole appearance was so indicative of haste and agitation that I involuntarily rose at his approach and impetuously inquired:")
green.stop<- which(green=="And leaving them there, with the light of growing hope and confidence on their faces, we went out again into the night, and so into a dream from which I have never waked, though the shine of her dear eyes have been now the load-star of my life for many happy, happy months.")
green <- green[green.start:green.stop]

spots <- grep('[A-Z]{2,}[^a-z]', green)
green[spots]
green.chaps<-spots[c(1,3,5,7,9,12,14,16,18,26,28,37,40,42,44,
                   46,48,50,52,54,56,62,64,66,68,71,73,75,
                   77,80,84,87,90, 95,97,99,104)]
green.content<-spots[c(2,4,6,8,11,13,15,17,19,27,30,38,41,43,45,
                     47,49,51,53,55,57,63,65,67,69,72,74,76,
                     79,81,85,88,92,96,98,100,105)]
green.books<-spots[c(39,70, 89)]
# hold out. green.bad <- spots[94]
green.chaps[1]
green.content[1]
green[green.chaps[1]:(green.content[1]-1)]
green[green.content]
green.content
toRemove <-c(0)

green <- green[-green.books]
grep("BOOK IV", green)
green <- green[-c(976, 2055, 2548)]
print(length(green))

green.paragraphs <- as.data.frame(green, stringsAsFactors=FALSE)
colnames(green.paragraphs) <- c("paras")

green.paragraphs<- green.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

green.paragraphs <- green.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

green.paragraphs <- green.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

green.paragraphs <- green.paragraphs %>% 
  filter(paragraphs!="")

green.paragraphs <- green.paragraphs %>% 
  filter(paragraphs!="  ")
colnames(green.paragraphs)

green.paragraphs <- green.paragraphs %>% 
  transmute(paras = replace_abbreviation(paragraphs))

green.paragraphs <- green.paragraphs %>% 
  transmute(paragraphs=  gsub("MR\\.", "Mr", paras))

print(length(green.paragraphs$paragraphs))

green.paragraphs <- green.paragraphs %>% 
  filter(paragraphs!="")

green.paragraphs <- green.paragraphs %>% 
  filter(paragraphs!="  ")
print(length(green.paragraphs$paragraphs))


rayn.title <- rep("theRaynerSladeAmalgamation", 3093)
rayn.para.type <- rep("paragraph",3093)
rayn.para.counter<-seq(1, 1717)
rayn.para.id <- paste0("THE_RAYNER_SLADE_AMALGAMATION_", "PARAGRAPH_", rayn.para.counter)
rayn.label <- rep("0", 1717)
print(length(rayn.para.id))

rayn.para.matrix <- cbind(rayn.title, rayn.para.type, rayn.para.id, rayn.paragraphs, rayn.label)
rayn.para.df <- as.data.frame(rayn.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(rayn.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", rayn.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theRaynerSladeAmalgamation' LIMIT 2")
dbDisconnect(con)
