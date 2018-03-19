

install.packages("tm")
install.packages("ggplot2")
install.packages("sentimentr")
install.packages("wordcloud")
install.packages("Rstem")
install.packages("syuzhet")
install.packages("antiword")

library(tm)
library(sentimentr)
library(ggplot2)
library(wordcloud)
library(Rstem)
library(syuzhet)
library(antiword)

setwd("C:\\Users\\user.TALCL6N3N88RNM\\Desktop\\EQUISKILL\\10_BI Case Studies\\E-commerce Analytics")
getwd()

edata <- read.csv("data_raw.csv")

ecorp <- VCorpus(VectorSource(edata))

inspect(ecorp)

ecorp <- tm_map(ecorp, content_transformer(tolower))
ecorp <- tm_map(ecorp, removePunctuation)
ecorp <- tm_map(ecorp, removeNumbers)
ecorp <- tm_map(ecorp, removeWords, stopwords("english"))
ecorp <- tm_map(ecorp, content_transformer(bracketX))
ecorp <- tm_map(ecorp, content_transformer(replace_number))
ecorp <- tm_map(ecorp, content_transformer(replace_contraction))
ecorp <- tm_map(ecorp, stemDocument)

twtdm <- DocumentTermMatrix(ecorp)

twparse <- removeSparseTerms(twtdm, .99)
inspect(twparse)


tf <- sort(colSums(as.matrix(twparse)), decreasing = TRUE)


words <- names(tf)


wordcloud(words, tf, colors = brewer.pal(8, "Set2"), scale = c(5, 1))



mysentiment <- get_nrc_sentiment(tweets)

SentimentScores <- data.frame(colSums(mysentiment[,]))

names(SentimentScores) <- "Score"

SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)

rownames(SentimentScores) <- NULL

ggplot(data = SentimentScores, aes(x = sentiment, y = scores)) +
  geom_bar(aes(fill = sentiment), stat = "identity") + 
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")






