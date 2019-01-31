#### #exploration of Harvard Inquirer Dictionary for Sentiment Analysis. 
## 10/10/18
## to be worked into data frame 1/31/19
# as inspired by https://nlp.stanford.edu/pubs/kaojurafsky12.pdf

#install.packages("readxl")
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
  unique()

object <- data2 %>%
  filter(!is.na(Object)) %>%
  group_by(Object, Entry) %>%
  summarise (n = n()) %>%
  .$Entry %>%
  gsub("#.", "", .) %>%
  unique()

relationship <- data2 %>%
  filter(!is.na(Rel)) %>%
  group_by(Rel, Entry) %>%
  summarise (n = n()) %>%
  .$Entry %>%
  gsub("#.", "", .) %>%
  unique()

time <- data2 %>%
  filter(!is.na(`Time@`)) %>%
  group_by(`Time@`, Entry) %>%
  summarise (n = n()) %>%
  .$Entry %>%
  gsub("#.", "", .) %>%
  unique()

perceive <- data2 %>%
  filter(!is.na(Perceiv)) %>%
  group_by(Perceiv, Entry) %>%
  summarise (n = n()) %>%
  .$Entry %>%
  gsub("#.", "", .) %>%
  unique()

# http://www.wjh.harvard.edu/~inquirer/Self.html
# CSV is entirely NA for this column but this entry describes which words fit in here
self <- c("I", "I'M", "ME", "MINE", "MY", "MYSELF", "ONESELF")






