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
  "The Plumed Serpent", 
  "Women in Love",
  "The Narrative of Arthur Gordon Pym of Nantucket",
  "Eureka: A Prose Poem",
  "The Great Gatsby",
  "Billy Budd",
  "Moby Dick",
  "Portrait of the Artist as a Young Man",
  "Wide Sargasso Sea",
  "Heart of Darkness",
  "Something Happened",
  "Life & Times of Michael K",
  "Under the Volcano", 
  "The Picture of Dorian Gray",
  "Gravity's Rainbow",
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
                   "The Innocence of Father Brown",
                   "The Brand of Silence",
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
png("lyrical_corpus.png", height = 25*nrow(lyrical_df), width = 250*ncol(lyrical_df), title="Lyrical Corpus")
grid.table(lyrical_df)
dev.off()

detective_df <- as.data.frame(cbind(det_authors_vec, det_books_vec))
colnames(detective_df) <- c("Author", "Title")
png("detective_corpus.png", height = 25*nrow(detective_df), width = 250*ncol(detective_df), title="Lyrical Corpus")
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

# got to make feature matrix selection 

identifiers <- c("labels_vec", "word_counts", "sent_counts", "para_counts", "sent_comma_freq",
                 "para_comma_freq", "words_per_sentence", "words_per_paragraph", 
                 "sents_per_paragraph", "consecutive_counts", "consecutive_repeat_freq",
                 "syll_and_word_freq", "polysyll_and_word_freq", "polysyll_and_sent_freq",
                 "unique_counts", "type_token_ratio", "mean_usage_freq","median_MATTR",
                 "object_freq", "relationship_freq", "time_freq","self_freq", "perceive_freq",
                 "i_freq", "top_ten_freq", "dialogue_freq", "question_vec", "exclamation_vec",
                 "sentiment_vec")
explanation <- c("Category",
                 "Number of words",
                 "Number of sentences",
                 "Number of paragraphs",
                 "Number of commas per sentence",
                 "Number of commas per paragraph",
                 "Number of words per sentence",
                 "Number of words per paragraph",
                 "Number of sentences per paragraph",
                 "Number of anaphoric sentences",
                 "Frequency of anaphoric sentences",
                 "Number of syllables per word",
                 "Number of polysyllables per word",
                 "Number of polysyllables per sentence",
                 "Number of unique words",
                 "Type-Token Ratio",
                 "Mean Usage Frequency (1 over Type-Token Ratio)",
                 "Median Moving-Average Type-Token Ratio",
                 "Percent of words in Harvard Inquirer's “object” category",
                 "Percent of words in Harvard Inquirer's “relationship” category",
                 "Percent of words in Harvard Inquirer's “time” category",
                 "Percent of words in Harvard Inquirer's “self” category",
                 "Percent of words in Harvard Inquirer's “perceive” category",
                 "Frequency of the word 'I'",
                 "Frequency of a book's top ten words",
                 "Percentage of dialogue",
                 "Number of question marks",
                 "Number of exclamation points",
                 "average sentiment across text")
explanation <- as.data.frame((cbind(identifiers, explanation)))
colnames(explanation) <- c("Feature Names", "Explanation")

png("explanation.png", height = 25*nrow(explanation), width =250*ncol(explanation))
grid.table(explanation)
dev.off()

some_weights <- c(0.7237152, -0.3074419, -1.345788)
some_labels <- c("anaphora-frequency", "i-frequency", "perception-frequency")
both <- as.data.frame(cbind(some_labels, some_weights))
colnames(both) <- c("Features", "Weights")

png("feature_weights.png", height = 50*nrow(both), width =150*ncol(both))
grid.table(both)
dev.off()

books <-c("Billy Budd", "Eureka: A Prose Poem", "Heart of Darkness",
           "The Sound and the Fury", "Wide Sargasso Sea",
           "The Big Sleep", "The Circular Staircase", "The Mystery of Room 75")
authors <-c("Herman Meville", "Edgar Allan Poe", "Joseph Conrad",
            "William Faulkner", "Jean Rhys", "Raymond Chandler",
            "Mary Roberts Rinehart", "Fred Merrick White")
labeled <- c(rep("detective", 5), rep("lyrical", 3))
correct <- c(rep("lyrical", 5), rep("detective",3))

titles <- as.data.frame(cbind(books, authors, labeled, correct))
colnames(titles) <- c("Title","Author", "Classified Label", "Correct Label")

png("bad_books.png", height = 25*nrow(titles), width =125*ncol(titles))
grid.table(titles)
dev.off()
