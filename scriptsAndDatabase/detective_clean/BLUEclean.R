setwd("~/Documents/7thSemester/dmp/corpus")
library("RSQLite")
library("tokenizers")
library("dplyr")
library("textclean")
library("stringr")
library("tm")
library(qdap)

# just do the paragraphs first then distill down. 

blue.paragraphs <- read.csv("Python_Scripts/checkCorpus/BLUE_paras.csv", stringsAsFactors = FALSE)
blue.paragraphs$X0[20]

blue.paragraphs <- blue.paragraphs[-c(1:18,1183:1186),]
colnames(blue.paragraphs) <- c("arb", "paragraphs")


blue.paragraphs <- blue.paragraphs %>% 
  transmute(paras=  gsub("\n", " ", paragraphs) )

blue.paragraphs<- blue.paragraphs %>%
  transmute(paragraphs=gsub("\"|\\*|(?<=[A-Z])(\\.)(?=[A-Z]|\\.|\\s)", "", perl=TRUE, paras))

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paras=  gsub("Mrs\\.", "Mrs", paragraphs) )

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paragraphs=  gsub("Mr\\.", "Mr", paras))

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paras=  gsub("CHAPTER.X{0,3}(IX|IV|V?I{0,3}).", "", paragraphs) )

blue.paragraphs <- blue.paragraphs %>% 
  filter(paras!="")

blue.paragraphs <- blue.paragraphs %>% 
  filter(paras!="  ")
colnames(blue.paragraphs)

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paragraphs = replace_abbreviation(paras))

blue.paragraphs <- blue.paragraphs %>% 
  transmute(paras=  gsub("MR\\.", "Mr", paragraphs))

print(length(blue.paragraphs$paras))

blue.paragraphs <- blue.paragraphs %>% 
  filter(paras!="")

blue.paragraphs <- blue.paragraphs %>% 
  filter(paras!="  ")
blue.paragraphs <- blue.paragraphs %>% 
  transmute(paragraphs=  gsub("_", "", paras))
# Remove 
spots <- grep("^--", blue.paragraphs$paragraphs)
blue.paragraphs <- as.data.frame(blue.paragraphs[-c(spots),], stringsAsFactors = FALSE)
colnames(blue.paragraphs) <- c("paragraphs")
blue.paragraphs$paragraphs[555:568]

print(length(blue.paragraphs$paragraphs))

blue.title <- rep("theLadyInBlue", 1127)
blue.para.type <- rep("paragraph",1127)
blue.para.counter<-seq(1, 1127)
blue.para.id <- paste0("THE_LADY_IN_BLUE_", "PARAGRAPH_", blue.para.counter)
blue.label <- rep("0", 1127)
print(length(blue.para.id))

blue.para.matrix <- cbind(blue.title, blue.para.type, blue.para.id, blue.paragraphs, blue.label)
blue.para.df <- as.data.frame(blue.para.matrix, stringsAsFactors = FALSE)
stock <- c("Title", "Type", "ID", "Unit", "Label")
colnames(blue.para.df) <- stock
con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
dbWriteTable(con, "textTable", blue.para.df, append=TRUE, row.names=FALSE)
dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='paragraph' AND Title='theLadyInBlue' LIMIT 2")
dbDisconnect(con)

# blue sents. 
