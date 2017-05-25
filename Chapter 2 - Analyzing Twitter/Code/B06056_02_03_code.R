############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and perform sentiment analysis 
# Note         :   Your results may be different than the ones discussed 
#                  in the chapter due to dyanmic nature of Twitter
############################################################################

library(twitteR)
library(data.table)
library(tm)
library(ggplot2)
library(stringr)
library(syuzhet)
library(wordcloud)

CONSUMER_SECRET = "XXXXXXXXXXXXXXXXXXX"
CONSUMER_KEY = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"


############################################################################
#       Utility Functions
############################################################################

# optional
library(httr)
# Set proxy options
set_config( use_proxy( url  = "http://proxy-chain.intel.com"
                       , port = 911
)
);

#extract timeline tweets
extractTimelineTweets <- function(username,tweetCount){
  # timeline tweets
  twitterUser <- getUser(username)
  tweets = userTimeline(twitterUser,n=tweetCount)
  tweets.df = twListToDF(tweets)
  tweets.df$text <- sapply(tweets.df$text,function(x) iconv(x,to='UTF-8'))
  
  return(tweets.df)
}


encodeSentiment <- function(x) {
  if(x <= -0.5){
    "very negative"
  }else if(x > -0.5 & x < 0){
    "negative"
  }else if(x > 0 & x < 0.5){
    "positive"
  }else if(x >= 0.5){
    "very positive"
  }else {
    "neutral"
  }
}


############################################################################
#             Sentiment Analysis
############################################################################

# connect to twitter app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,consumer_secret = CONSUMER_SECRET)

tweetsDF <- extractTimelineTweets("POTUS",1500)
tweetsDF$text <- sapply(tweetsDF$text,function(x) iconv(x,to='UTF-8'))

nohandles <- str_replace_all(tweetsDF$text, "@\\w+", "")
wordCorpus <- Corpus(VectorSource(nohandles))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("amp"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)
  wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=1000, random.order=FALSE, 
            rot.per=0.35, use.r.layout=FALSE, colors=pal)


tweetSentiments <- get_sentiment (tweetsDF$text,method = "syuzhet")
tweets <- cbind(tweetsDF, tweetSentiments)
tweets$sentiment <- sapply(tweets$tweetSentiments,encodeSentiment)

qplot(tweets$tweetSentiments) + theme(legend.position="none")+
  xlab("Sentiment Score") +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Sentiment Score") 

ggplot(tweets, aes(sentiment)) +
  geom_bar(fill = "aquamarine4") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Sentiment") 



# NRC Sample
tweetSentiments <- get_nrc_sentiment(tweetsDF$text)
tweets <- cbind(tweetsDF, tweetSentiments)

sentimentTotals <- data.frame(colSums(tweets[,c(17:26)]))

names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

############################################################################
#              Cluster Analysis
############################################################################



twtrTermDocMatrix <- TermDocumentMatrix(wordCorpus)
inspect(twtrTermDocMatrix[57:70, 100:110])

# Terms occuring in more than 30 times
which(apply(twtrTermDocMatrix,1,sum)>=30)


# Frequency-Association
(frequentTerms<-findFreqTerms(twtrTermDocMatrix,lowfreq = 10))


# clustering
twtrTermDocMatrix2 <- removeSparseTerms(twtrTermDocMatrix, sparse = 0.97)

tweet_matrix <- as.matrix(twtrTermDocMatrix2)

# cluster terms
distMatrix <- dist(scale(tweet_matrix))

fit <- hclust(distMatrix,method="single")
plot(fit)
