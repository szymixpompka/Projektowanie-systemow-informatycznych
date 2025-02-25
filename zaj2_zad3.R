install.packages("qdap", dependencies = TRUE)
library(qdap)

text1 <- readLines(file.choose())
text2 <- readLines(file.choose())
 
frequent_terms <- freq_terms(text1)
frequent_terms
frequent_terms <- freq_terms(text1, stopwords = Top200Words)
plot(frequent_terms)

install.packages("wordcloud", dependencies = TRUE)
library(wordcloud)
wordcloud(frequent_terms$WORD, frequent_terms$FREQ)
?wordcloud
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4)
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues"),max.words = 5)

frequent_terms2 <- freq_terms(text2)
frequent_terms2 <- freq_terms(text2, stopwords = Top200Words)
plot(frequent_terms2)

wordcloud(frequent_terms2$WORD, frequent_terms2$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds"),max.words = 5)
