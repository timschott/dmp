library(keras)
library(tidyverse)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
word_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='word'")
dbDisconnect(con)