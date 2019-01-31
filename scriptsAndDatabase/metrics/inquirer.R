#### #exploration of Harvard Inquirer Dictionary for Sentiment Analysis. 
## 10/10/18
## to be worked into data frame 1/31/19
# as inspired by https://nlp.stanford.edu/pubs/kaojurafsky12.pdf

#install.packages("readxl")
# install.packages("beepr")
big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)
library(beepr)
library(readxl)
library(tibble)
library(dplyr)
data1 = read_excel("scriptsAndDatabase/metrics/inquireraugmented.xls")
data1 %>% dim()
feature_names<-colnames(data1)
sort(feature_names)

### FILTER down to Object , Abs@, Rel, Time@, Perceiv, Self

data2 <- data1 %>% select(Entry, Object, `Abs@`, Rel, `Time@`, Perceiv, Self)
# a healthier 6 feature cols. 
data2 %>% dim()

# a way to get my lists of each of these categories. 
abs <- data2 %>%
  filter(!is.na(`Abs@`)) %>%
  group_by(`Abs@`, Entry) %>%
  summarise (n = n()) %>%
  .$Entry %>%
  gsub("#.", "", .) %>%
  unique() %>%
  tolower()

object <- data2 %>%
  filter(!is.na(Object)) %>%
  group_by(Object, Entry) %>%
  summarise (n = n()) %>%
  .$Entry %>%
  gsub("#.", "", .) %>%
  unique() %>%
  tolower()

relationship <- data2 %>%
  filter(!is.na(Rel)) %>%
  group_by(Rel, Entry) %>%
  summarise (n = n()) %>%
  .$Entry %>%
  gsub("#.", "", .) %>%
  unique() %>%
  tolower()

time <- data2 %>%
  filter(!is.na(`Time@`)) %>%
  group_by(`Time@`, Entry) %>%
  summarise (n = n()) %>%
  .$Entry %>%
  gsub("#.", "", .) %>%
  unique() %>%
  tolower()

perceive <- data2 %>%
  filter(!is.na(Perceiv)) %>%
  group_by(Perceiv, Entry) %>%
  summarise (n = n()) %>%
  .$Entry %>%
  gsub("#.", "", .) %>%
  unique() %>% 
  tolower

# http://www.wjh.harvard.edu/~inquirer/Self.html
# CSV is entirely NA for this column but this entry describes which words fit in here
self <- c("I", "I'M", "ME", "MINE", "MY", "MYSELF", "ONESELF")
i_vec <- c("i")
self <- self %>% tolower

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
# pull out lyrical and detective sentences. 
detective_word_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='word' AND Label ='0'")
dbDisconnect(con)
detective_titles <- sort(unique(detective_word_df$Title))
# angel_words <- filter(detective_word_df, Title==detective_titles[1])
# length(angel_words$Unit)
# the number of commas is fixed. so let's achieve this readout in one, collective loop. 
word_test <- c("i", "i'm", "dog", "blanket", "hello")

detective_object_count <- c(0)
detective_i_count <- c(0)
detective_relationship_count <- c(0)
detective_time_count <- c(0)
detective_abstract_count <- c(0)
detective_self_count <- c(0)
detective_perceive_count <- c(0)

for(i in seq(1:24)){
  words <- filter(detective_word_df, Title==detective_titles[i])
  detective_object_count <- append(detective_object_count,  sum(words$Unit %in% object, na.rm = TRUE))
  detective_i_count <- append(detective_i_count,  sum(words$Unit %in% i_vec, na.rm = TRUE))
  detective_relationship_count <- append(detective_relationship_count,  sum(words$Unit %in% relationship, na.rm = TRUE))
  detective_time_count <- append(detective_time_count,  sum(words$Unit %in% time, na.rm = TRUE))
  detective_abstract_count <- append(detective_abstract_count,  sum(words$Unit %in% abs, na.rm = TRUE))
  detective_self_count <- append(detective_self_count,  sum(words$Unit %in% self, na.rm = TRUE))
  detective_perceive_count <- append(detective_perceive_count,  sum(words$Unit %in% perceive, na.rm = TRUE))
}

detective_object_count <- detective_object_count[-c(1)]
detective_relationship_count <- detective_relationship_count[-c(1)]
detective_i_count <- detective_i_count[-c(1)]
detective_time_count <- detective_time_count[-c(1)]
detective_abstract_count <- detective_abstract_count[-c(1)]
detective_self_count <- detective_self_count[-c(1)]
detective_perceive_count <-detective_perceive_count[-c(1)]

detective_object_freq <- detective_object_count/big_boy$word_counts_vec[27:50]
detective_relationship_freq <- detective_relationship_count/big_boy$word_counts_vec[27:50]
detective_time_freq <- detective_time_count/big_boy$word_counts_vec[27:50]
detective_i_freq <- detective_i_count/big_boy$word_counts_vec[27:50]
detective_abstract_freq <- detective_abstract_count/big_boy$word_counts_vec[27:50]
detective_self_freq <- detective_self_count/big_boy$word_counts_vec[27:50]
detective_perceive_freq <- detective_perceive_count/big_boy$word_counts_vec[27:50]

tg <- c(abs, time, perceive, self, relationship,object)
length(unique(tg))
duplicate_words <- tg[duplicated(tg)]

### lyrical freqs. 

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
# pull out lyrical and lyrical sentences. 
lyrical_word_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='word' AND Label ='1'")
dbDisconnect(con)
lyrical_titles <- sort(unique(lyrical_word_df$Title))
# angel_words <- filter(lyrical_word_df, Title==lyrical_titles[1])
# length(angel_words$Unit)
# the number of commas is fixed. so let's achieve this readout in one, collective loop. 
# word_test <- c("i", "i'm", "dog", "blanket", "hello")

lyrical_object_count <- c(0)
lyrical_relationship_count <- c(0)
lyrical_time_count <- c(0)
lyrical_abstract_count <- c(0)
lyrical_self_count <- c(0)
lyrical_perceive_count <- c(0)
lyrical_i_count <- c(0)

for(i in seq(1:26)){
  words <- filter(lyrical_word_df, Title==lyrical_titles[i])
  lyrical_object_count <- append(lyrical_object_count,  sum(words$Unit %in% object, na.rm = TRUE))
  lyrical_relationship_count <- append(lyrical_relationship_count,  sum(words$Unit %in% relationship, na.rm = TRUE))
  lyrical_i_count <- append(lyrical_i_count,  sum(words$Unit %in% i_vec, na.rm = TRUE))
  lyrical_time_count <- append(lyrical_time_count,  sum(words$Unit %in% time, na.rm = TRUE))
  lyrical_abstract_count <- append(lyrical_abstract_count,  sum(words$Unit %in% abs, na.rm = TRUE))
  lyrical_self_count <- append(lyrical_self_count,  sum(words$Unit %in% self, na.rm = TRUE))
  lyrical_perceive_count <- append(lyrical_perceive_count,  sum(words$Unit %in% perceive, na.rm = TRUE))
  beep()
  }

lyrical_object_count <- lyrical_object_count[-c(1)]
lyrical_relationship_count <- lyrical_relationship_count[-c(1)]
lyrical_time_count <- lyrical_time_count[-c(1)]
lyrical_abstract_count <- lyrical_abstract_count[-c(1)]
lyrical_i_count <- lyrical_i_count[-c(1)]
lyrical_self_count <- lyrical_self_count[-c(1)]
lyrical_perceive_count <-lyrical_perceive_count[-c(1)]

lyrical_object_freq <- lyrical_object_count/big_boy$word_counts_vec[1:26]
lyrical_relationship_freq <- lyrical_relationship_count/big_boy$word_counts_vec[1:26]
lyrical_time_freq <- lyrical_time_count/big_boy$word_counts_vec[1:26]
lyrical_i_freq <- lyrical_i_count/big_boy$word_counts_vec[1:26]
lyrical_abstract_freq <- lyrical_abstract_count/big_boy$word_counts_vec[1:26]
lyrical_self_freq <- lyrical_self_count/big_boy$word_counts_vec[1:26]
lyrical_perceive_freq <- lyrical_perceive_count/big_boy$word_counts_vec[1:26]

object_freq <- c(lyrical_object_freq, detective_object_freq)
relationship_freq <- c(lyrical_relationship_freq, detective_relationship_freq)
time_freq <- c(lyrical_time_freq, detective_time_freq)
self_freq <- c(lyrical_self_freq, detective_self_freq)
perceive_freq <- c(lyrical_perceive_freq, detective_perceive_freq)
i_freq <- c(lyrical_i_freq, detective_i_freq)
#big_boy <- as.data.frame(cbind(big_boy, object_freq, relationship_freq, time_freq, self_freq, perceive_freq), stringsAsFactors = FALSE)
big_boy <- as.data.frame(cbind(big_boy, i_freq), stringsAsFactors = FALSE)
colnames(big_boy)
write.csv(big_boy,'starts.csv')

summary(big_boy$i_freq[1:26])
summary(big_boy$i_freq[27:50])



