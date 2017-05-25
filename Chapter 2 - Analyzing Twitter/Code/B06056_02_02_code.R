############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and perform trend analysis using
#                  different visualizations
# Note         :   Your results may be different than the ones discussed 
#                  in the chapter due to dyanmic nature of Twitter
############################################################################

library(tm)
library(ggmap)
library(ggplot2)
library(twitteR)
library(stringr)
library(wordcloud)
library(lubridate)
library(data.table)


CONSUMER_SECRET = "XXXXXXXX"
CONSUMER_KEY = "XXXXXXXX"


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


# plot by country
mapCountry <- function(x) {
  if(!is.na(str_match(tolower(x),"japan")) |!is.na(str_match(tolower(x),"fukushima")) ){
    "japan"
  }else if(!is.na(str_match(tolower(x),"new zealand")) |!is.na(str_match(tolower(x),"nz"))){
    "new zealand"
  }else if(!is.na(str_match(tolower(x),"india"))){
    "india"
  }else if(!is.na(str_match(tolower(x),"italy")) |!is.na(str_match(tolower(x),"italia"))){
    "italy"
  }else if(!is.na(str_match(tolower(x),"spain"))){
    "spain"
  }else if(!is.na(str_match(tolower(x),"mexico"))){
    "mexico"
  }else if(!is.na(str_match(tolower(x),"philippine"))){
    "philippines"
  }else if(!is.na(str_match(tolower(x),"peru"))){
    "peru"
  }else if(!is.na(str_match(tolower(x),"puerto"))){
    "puerto rico"
  }else if(!is.na(str_match(tolower(x),"indonesia"))){
    "indonesia"
  }else if(!is.na(str_match(tolower(x),"argentina"))){
    "argentina"
  }else if(!is.na(str_match(tolower(x),"unitedstates"))|!is.na(str_match(tolower(x),"united states"))){
    "US"
  }else {
    "rest_of_the_world"
  }
}



# plot by source
# encode tweet source as iPhone, iPad, Android or Web
enodeSource <- function(x) {
  if(x=="<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone</a>"){
    gsub("<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone</a>", "iphone", x,fixed=TRUE)
  }else if(x=="<a href=\"http://twitter.com/#!/download/ipad\" rel=\"nofollow\">Twitter for iPad</a>"){
    gsub("<a href=\"http://twitter.com/#!/download/ipad\" rel=\"nofollow\">Twitter for iPad</a>","ipad",x,fixed=TRUE)
  }else if(x=="<a href=\"http://twitter.com/download/android\" rel=\"nofollow\">Twitter for Android</a>"){
    gsub("<a href=\"http://twitter.com/download/android\" rel=\"nofollow\">Twitter for Android</a>","android",x,fixed=TRUE)
  } else if(x=="<a href=\"http://twitter.com\" rel=\"nofollow\">Twitter Web Client</a>"){
    gsub("<a href=\"http://twitter.com\" rel=\"nofollow\">Twitter Web Client</a>","Web",x,fixed=TRUE)
  } else if(x=="<a href=\"http://www.twitter.com\" rel=\"nofollow\">Twitter for Windows Phone</a>"){
    gsub("<a href=\"http://www.twitter.com\" rel=\"nofollow\">Twitter for Windows Phone</a>","windows phone",x,fixed=TRUE)
  }else if(x=="<a href=\"http://dlvr.it\" rel=\"nofollow\">dlvr.it</a>"){
    gsub("<a href=\"http://dlvr.it\" rel=\"nofollow\">dlvr.it</a>","dlvr.it",x,fixed=TRUE)
  }else if(x=="<a href=\"http://ifttt.com\" rel=\"nofollow\">IFTTT</a>"){
    gsub("<a href=\"http://ifttt.com\" rel=\"nofollow\">IFTTT</a>","ifttt",x,fixed=TRUE)
  }else if(x=="<a href=\"http://earthquaketrack.com\" rel=\"nofollow\">EarthquakeTrack.com</a>"){
    gsub("<a href=\"http://earthquaketrack.com\" rel=\"nofollow\">EarthquakeTrack.com</a>","earthquaketrack",x,fixed=TRUE)
  }else if(x=="<a href=\"http://www.didyoufeel.it/\" rel=\"nofollow\">Did You Feel It</a>"){
    gsub("<a href=\"http://www.didyoufeel.it/\" rel=\"nofollow\">Did You Feel It</a>","did_you_feel_it",x,fixed=TRUE)
  }else if(x=="<a href=\"http://www.mobeezio.com/apps/earthquake\" rel=\"nofollow\">Earthquake Mobile</a>"){
    gsub("<a href=\"http://www.mobeezio.com/apps/earthquake\" rel=\"nofollow\">Earthquake Mobile</a>","earthquake_mobile",x,fixed=TRUE)
  }else if(x=="<a href=\"http://www.facebook.com/twitter\" rel=\"nofollow\">Facebook</a>"){
    gsub("<a href=\"http://www.facebook.com/twitter\" rel=\"nofollow\">Facebook</a>","facebook",x,fixed=TRUE)
  }else {
    "others"
  }
}


############################################################################
#             Trend Analysis
############################################################################

# connect to twitter app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,consumer_secret = CONSUMER_SECRET)

# extract tweets based on a search term
searchTerm <- "#earthquake"
trendingTweets = searchTwitter(searchTerm,n=1000)

# perform a quick cleanup/transformation
trendingTweets.df = twListToDF(trendingTweets)
trendingTweets.df$text <- sapply(trendingTweets.df$text,function(x) iconv(x,to='UTF-8'))
trendingTweets.df$created <- ymd_hms(trendingTweets.df$created)

# see how many missing values are there on a per column basis
sapply(trendingTweets.df, function(x) sum(is.na(x)))


############################################################################

# plot on tweets by time
ggplot(data = trendingTweets.df, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


# identify earthquake affected countries
trendingTweets.df$quakeCountry <- sapply(trendingTweets.df$text,mapCountry)

# geocode tweets->map to earthquake locations
quakeAffectedCountries <- subset(trendingTweets.df,quakeCountry != 'rest_of_the_world')$quakeCountry
unqiueCountries <- unique(sort(quakeAffectedCountries))
geoCodedCountries <- geocode(unqiueCountries)
country.x <- geoCodedCountries$lon
country.y <- geoCodedCountries$lat

mp <- NULL

# create a layer of borders
mapWorld <- borders("world", colour="gray50", fill="gray50") 
mp <- ggplot() +   mapWorld

# Now Layer the cities on top
mp <- mp+ geom_point(aes(x=country.x, y=country.y) ,
                     color="orange", 
                     size=sqrt(table(sort(quakeAffectedCountries))))
mp




# plot tweets by counts
ggplot(subset(trendingTweets.df,quakeCountry != 'rest_of_the_world'), aes(quakeCountry)) +
  geom_bar(fill = "aquamarine4") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Country") 



############################################################################

# plot tweets by source system (android, iphone, web, etc)
trendingTweets.df$tweetSource = sapply(trendingTweets.df$statusSource,
                                       function(sourceSystem) enodeSource(sourceSystem))


ggplot(trendingTweets.df[trendingTweets.df$tweetSource != 'others',], aes(tweetSource)) +
  geom_bar(fill = "aquamarine4") + 
  theme(legend.position="none", 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Source") 


############################################################################

# accounts which tweet about quakes
pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]

quakeAccounts <- str_extract_all(trendingTweets.df$text, "@\\w+")
namesCorpus <- Corpus(VectorSource(quakeAccounts))

set.seed(42)
wordcloud(words = namesCorpus, scale=c(3,0.5), max.words=100, random.order=FALSE, 
          rot.per=0.10, use.r.layout=TRUE, colors=pal)