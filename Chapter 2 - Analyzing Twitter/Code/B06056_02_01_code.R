############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and extract sample tweets
############################################################################

# load the package
library(twitteR)

# set the credentials
CONSUMER_SECRET <- "XXXXXXXX"
CONSUMER_KEY <- "XXXXXX"

# connect to twitter app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET)

# set twitter user
twitterUser <- getUser("jack")

# extract a few sample tweets from this user's timeline
tweets <- userTimeline(twitterUser, n = 10)

# display attributes and function of tweet object
tweets[[1]]$getClass()

# display favorite count
tweets[[1]]$favoriteCount



