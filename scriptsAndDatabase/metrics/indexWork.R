big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)

con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
word_df <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='word'")
grav <- dbGetQuery(con, "SELECT Unit,ID,Title  FROM textTable WHERE Type='word' AND Title='gravitysRainbow'")
# dbExecute(con, "DELETE FROM textTable WHERE Label IS NULL")

dbDisconnect(con)

# need to do a gatsby hotfix because that is just confusing. 
# later. 

# make the matrix. 

# g.words.matrix <- cbind(g.title, g.words.type, g.words.id, g.words)
# g.words.df <- as.data.frame(g.words.matrix, stringsAsFactors=FALSE)
# colnames(g.words.df) <- stock

# con <- dbConnect(RSQLite::SQLite(), ":memory:", dbname="textTable.sqlite")
### Columns. 
# we should no longer need to set the column names
# dbWriteTable(con, "textTable", g.words.df, append=TRUE, row.names=FALSE)
# dbGetQuery(con, "SELECT Unit FROM textTable WHERE Type='word' AND Title='theGreatGatsby' LIMIT 10")
print(length(word_df$Unit))
print(unique(word_df$Title))

titles <- c(unique(word_df$Title))
lengths <- c(big_boy$word_counts_vec[6],
             big_boy$word_counts_vec[20],
             big_boy$word_counts_vec[16],
             big_boy$word_counts_vec[26],
             big_boy$word_counts_vec[13],
             big_boy$word_counts_vec[8],
             big_boy$word_counts_vec[19],
             big_boy$word_counts_vec[9],
             big_boy$word_counts_vec[21],
             big_boy$word_counts_vec[14],
             big_boy$word_counts_vec[24],
             big_boy$word_counts_vec[11],
             big_boy$word_counts_vec[23],
             big_boy$word_counts_vec[4],
             big_boy$word_counts_vec[12],
             big_boy$word_counts_vec[2],
             big_boy$word_counts_vec[22],
             big_boy$word_counts_vec[17],
             big_boy$word_counts_vec[28],
             big_boy$word_counts_vec[7],
             big_boy$word_counts_vec[1],
             big_boy$word_counts_vec[3],
             big_boy$word_counts_vec[10],
             big_boy$word_counts_vec[15],
             big_boy$word_counts_vec[40],
             big_boy$word_counts_vec[47],
             big_boy$word_counts_vec[46],
             big_boy$word_counts_vec[45],
             big_boy$word_counts_vec[43],
             big_boy$word_counts_vec[44], 
             big_boy$word_counts_vec[37],
             big_boy$word_counts_vec[42],
             big_boy$word_counts_vec[39],
             big_boy$word_counts_vec[34],
             big_boy$word_counts_vec[33],
             big_boy$word_counts_vec[35],
             big_boy$word_counts_vec[31],
             big_boy$word_counts_vec[32],
             big_boy$word_counts_vec[29],
             big_boy$word_counts_vec[41],
             big_boy$word_counts_vec[36],
             big_boy$word_counts_vec[38],
             big_boy$word_counts_vec[27],
             big_boy$word_counts_vec[30],
             big_boy$word_counts_vec[48],
             big_boy$word_counts_vec[18],
             big_boy$word_counts_vec[49],
             big_boy$word_counts_vec[25],
             big_boy$word_counts_vec[5],
             big_boy$word_counts_vec[50])
print(titles)
print(lengths)
count <- 1
for(i in seq(1:50)){
  print(titles[i])
  print(lengths[i])
  count <- count + lengths[i]
  print(count)
}



