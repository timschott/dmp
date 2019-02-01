# most freqeuent words. 

words <- filter(lyrical_word_df, Title==lyrical_titles[1])

length(words$Unit)

t<-sort(table(words$Unit), decreasing=TRUE)
t<-as.data.frame(t, stringsAsFactors=FALSE)
length(words$Unit)

sum(t$Freq[1:10]))/length(words$Unit)

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

