library(gridExtra)
library(dplyr)
lyrical_authors_vec <- c(
  "Cormac McCarthy",
  "Cormac McCarthy",
  "D.H. Lawrence",
  "D.H. Lawrence",
  "D.H. Lawrence",
  "Edgar Allen Poe",
  "Edgar Allen Poe",
  "F. Scott Fitzgerald",
  "Herman Melville",
  "Herman Melville",
  "James Joyce",
  "Jean Rhys",
  "Joseph Conrad",
  "Joseph Heller", 
  "J. M. Coetzee",
  "Malcolm Lawry", 
  "Oscar Wilde",
  "Thomas Pynchon", 
  "Virginia Woolf",
  "Virginia Woolf",
  "Virginia Woolf",
  "Vladimir Nabokov",
  "Vladimir Nabokov", 
  "William Faulkner", 
  "William Faulkner",
  "William H. Gass")

lyrical_books_vec <- c("Blood Meridian",
  "The Road",
  "The Rainbow",
  "The Serpent", 
  "Women in Love",
  "Gravity's Rainbow",
  "Eureka: A Prose Poem",
  "The Great Gatsby",
  "Billy Budd",
  "Moby Dick",
  "Portrait of the Artist as a Young Man",
  "Wide Sargasso Sea",
  "Heart of Darkness",
  "Something Happened",
  "Life and Times of Michael K",
  "Under the Volcano", 
  "The Picture of Dorian Gray",
  "The Narrative of Arthur Gordon Pym of Nantucket",
  "Mrs. Dalloway", 
  "Orlando", 
  "To The Lighthouse", 
  "Pale Fire", 
  "Lolita",
  "Absalom Absalom", 
  "The Sound and the Fury",
  "In The Heart of the Heart of the Country")

det_authors_vec <- c("Agatha Christie",
                     "Anna Katherine Green",
                     "Arthur Conan Doyle", 
                     "Arthur Conan Doye",
                     "Arthur J. Rees", 
                     "Arthur J. Rees",
                     "Arthur J. Rees",
                     "Carolyn Wells",
                     "Edgar Wallace", 
                     "Edgar Wallace",
                     "Emmuska Orczy",
                     "Ethel Lina White",
                     "Fred Merrick White",
                     "Fred Merrick White",   
                     "G.K. Chesterton",
                     "Harrington Strong",
                     "J.S. Fletcher", 
                     "J.S. Fletcher", 
                     "J.S. Fletcher",
                     "Mary Roberts Rinehart",
                     "Mrs. Charles Bryce",
                     "R. Austin Freedman",
                     "Raymond Chandler",   
                     "Wilkie Collins")

det_books_vec <- c("The Secret Adversary",
                   "The Leavenworth Case",
                   "A Study in Scarlet",
                   "The Sign of Four",
                   "The Shrieking Pit", 
                   "The Moon Rock",
                   "The Hand in the Dark",
                   "The Maxwell Mystery", 
                   "The Angel Of Terror", 
                   "The Daffodil Mystery",
                   "The Old Man in the Corner",
                   "The Spiral Staircase",
                   "The Lady in Blue",
                   "The Mystery of Room 75",
                   "The Brand of Silence",
                   "The Innocence of Father Brown",   
                   "The Paradise Mystery", 
                   "The Rayner Slade Amalgamation", 
                   "The Scarhaven Keep",
                   "The Circular Staircase",
                   "The Ashiel Mystery",
                   "The Red Thumb Mark",
                   "The Big Sleep", 
                   "The Moonstone")

lyrical_df <- as.data.frame(cbind(lyrical_authors_vec, lyrical_books_vec))
colnames(lyrical_df) <- c("Author", "Title")
png("lyrical_corpus.png", height = 25*nrow(corpus_df), width = 250*ncol(corpus_df), title="Lyrical Corpus")
grid.table(lyrical_df)

detective_df <- as.data.frame(cbind(det_authors_vec, det_books_vec))
colnames(detective_df) <- c("Author", "Title")
png("detective_corpus.png", height = 25*nrow(corpus_df), width = 250*ncol(corpus_df), title="Lyrical Corpus")
grid.table(detective_df)


big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)


str(big_boy)
novel_df <- big_boy %>% dplyr::select(-c(numeric_labels_vec, titles_vec))
length(novel_df)
big_boy$titles_vec
t <- as.data.frame(t(novel_df), stringsAsFactors = FALSE)
length(t)
colnames(t) <- c(big_boy$titles_vec)

png("data_points.png", height = 75*nrow(t), width = 150*ncol(t))
grid.table(t)
dev.off()

# make size 7 subsets

df1 <- t %>%
  select(colnames(t)[1:7])

df2 <- t %>%
  select(colnames(t)[8:14])

df3 <- t %>%
  select(colnames(t)[15:21])

df4 <- t %>%
  select(colnames(t)[22:28])

df5 <- t %>%
  select(colnames(t)[29:35])

df6 <- t %>%
  select(colnames(t)[36:42])

df7 <- t %>%
  select(colnames(t)[42:50])


png("data_points_1.png", height = 25*nrow(df1), width = 150*ncol(df1))
grid.table(df1)
dev.off()

png("data_points_2.png", height = 25*nrow(df2), width = 150*ncol(df2))
grid.table(df2)
dev.off()

png("data_points_3.png", height = 25*nrow(df3), width = 150*ncol(df3))
grid.table(df3)
dev.off()

png("data_points_4.png", height = 25*nrow(df4), width = 150*ncol(df4))
grid.table(df4)
dev.off()

png("data_points_5.png", height = 25*nrow(df5), width = 150*ncol(df5))
grid.table(df5)
dev.off()

png("data_points_6.png", height = 25*nrow(df6), width = 150*ncol(df6))
grid.table(df6)
dev.off()

png("data_points_7.png", height = 25*nrow(df7), width = 150*ncol(df7))
grid.table(df7)
dev.off()



