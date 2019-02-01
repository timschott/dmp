# most freqeuent words. 

words <- filter(lyrical_word_df, Title==lyrical_titles[1])

length(words$Unit)

t<-sort(table(words$Unit), decreasing=TRUE)
t<-as.data.frame(t, stringsAsFactors=FALSE)
length(words$Unit)
vec<-c(0)
for(i in seq(1:length(words$Unit))){
  vec <- append(vec, (is.numeric(words$Unit) && length(words$Unit)>0))
  detective_object_count <- append(detective_object_count,  sum(words$Unit %in% object, na.rm = TRUE))

  
  }
sum(vec, na.rm = TRUE)

lyrical_top_ten_freq <- c(0)

for(i in seq(1:26)){
  words <- filter(lyrical_word_df, Title==lyrical_titles[i])
  length(words$Unit)
  t<-sort(table(words$Unit), decreasing=TRUE)
  t<-as.data.frame(t, stringsAsFactors=FALSE)
  length(words$Unit)
  lyrical_top_ten_freq <- append(lyrical_top_ten_freq, sum(t$Freq[1:10])/length(words$Unit))
  beep()
}
lyrical_top_ten_freq <- lyrical_top_ten_freq[-c(1)]

detective_top_ten_freq <- c(0)

for(i in seq(1:24)){
  words <- filter(detective_word_df, Title==detective_titles[i])
  length(words$Unit)
  t<-sort(table(words$Unit), decreasing=TRUE)
  t<-as.data.frame(t, stringsAsFactors=FALSE)
  length(words$Unit)
  detective_top_ten_freq <- append(detective_top_ten_freq, sum(t$Freq[1:10])/length(words$Unit))
  beep()
}
detective_top_ten_freq <- detective_top_ten_freq[-c(1)]

top_ten_freq <- c(lyrical_top_ten_freq, detective_top_ten_freq)

big_boy <- as.data.frame(cbind(big_boy, top_ten_freq), stringsAsFactors = FALSE)
colnames(big_boy)
write.csv(big_boy,'starts.csv')

summary(big_boy$top_ten_freq[1:26])
summary(big_boy$top_ten_freq[27:50])

